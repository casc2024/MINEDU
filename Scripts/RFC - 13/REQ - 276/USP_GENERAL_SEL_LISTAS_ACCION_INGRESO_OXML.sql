USE [EducaRH_Desa]
GO
/****** Object:  StoredProcedure [dbo].[USP_GENERAL_SEL_LISTAS_ACCION_INGRESO_OXML]    Script Date: 17/05/2022 19:42:18 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

--exec [USP_GENERAL_SEL_LISTAS_ACCION_INGRESO_OXML]  52,0
ALTER PROCEDURE [dbo].[USP_GENERAL_SEL_LISTAS_ACCION_INGRESO_OXML]
	@ID_MOTIVO_ACCION INT,
	@ID_SERVIDOR_PUBLICO INT
AS
BEGIN

	SET DATEFORMAT 'dmy'

	DECLARE	@ID_ACCION INT = 0,
			@ID_REGIMEN INT = 0,
			@REGLA6 BIT  = 0,
			@CODIGO_MOTIVO_ACCION INT = 0,
			@xml_categoria_remunerativa XML,
			@xml_categoria_remunerativa_origen XML,
			@xml_situacion_laboral XML,
			@xml_condicion_laboral XML,
			@xml_motivo_accion XML,
			@ID_CONDICION_LABORAL INT,
			@TOTAL INT,
			@ACCION_REEMPLAZO BIT = 0,
            @xml_subtipo_trabajador XML,
            @xml_subtipo_trabajador_categoria_remunerativa XML

	SELECT	@ID_ACCION = MA.ID_ACCION,
			@ID_REGIMEN = RG.ID_REGIMEN,
			@CODIGO_MOTIVO_ACCION = MA.CODIGO_MOTIVO_ACCION
	FROM	sistema.motivo_accion AS MA WITH (NOLOCK)
			INNER JOIN sistema.accion AS A WITH (NOLOCK) ON MA.ID_ACCION = A.ID_ACCION
			INNER JOIN parametrica.regimen_por_grupo_accion AS RG WITH (NOLOCK) ON A.ID_REGIMEN_POR_GRUPO_ACCION = RG.ID_REGIMEN_POR_GRUPO_ACCION
	WHERE	ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION

	IF EXISTS(	SELECT	ER.ID_ENTIDAD_REGLA
				FROM	sistema.entidad_regla AS ER WITH (NOLOCK)
						INNER JOIN sistema.regla AS R WITH (NOLOCK)
							ON ER.ID_REGLA = R.ID_REGLA
				WHERE	R.CODIGO_REGLA = 6 --MOTIVOS AS ACCION QUE SELECCIONAN LA SIGUIENTE ESCALA INMEDIATA SUPERIOR
						AND ER.ID_REGISTRO_ENTIDAD = @ID_MOTIVO_ACCION)
		BEGIN
			SET @REGLA6 = 1
		END

	/*
		1788: PROFESOR INTERINO A AUXILIAR DE EDUCACIÓN
	*/
	IF @CODIGO_MOTIVO_ACCION IN (1360, 1788)
		BEGIN
			SET @xml_categoria_remunerativa = 
				(SELECT	r.Value, r.Code, r.Text, @REGLA6 AS Generico, r.[Group],
						CASE WHEN @CODIGO_MOTIVO_ACCION = 1591 THEN CONVERT(BIT,1) ELSE CONVERT(BIT,0) END AS Vigente
				 FROM	(	SELECT	CR.ID_CATEGORIA_REMUNERATIVA AS [Value], 
									CR.CODIGO_CATEGORIA_REMUNERATIVA AS Code, 
									CR.DESCRIPCION_CATEGORIA_REMUNERATIVA AS [Text], 
									CR.ORDEN,
									@CODIGO_MOTIVO_ACCION AS [Group]
							FROM	sistema.categoria_remunerativa_motivo_accion AS CTMA WITH (NOLOCK)
									INNER JOIN sistema.categoria_remunerativa AS CR WITH (NOLOCK)
										ON CTMA.ID_CATEGORIA_REMUNERATIVA = CR.ID_CATEGORIA_REMUNERATIVA
									INNER JOIN sistema.motivo_accion AS MA WITH (NOLOCK)
										ON CTMA.ID_MOTIVO_ACCION = MA.ID_MOTIVO_ACCION
							WHERE	MA.CODIGO_MOTIVO_ACCION = @CODIGO_MOTIVO_ACCION
									AND CTMA.ES_VIGENTE = 1
									AND CTMA.ORIGEN_DESTINO IN (1,2)
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)
		END
	ELSE
        BEGIN
			/*
			67:ASCENSO DE CARGO
			56:DESIGNACIÓN
			65:POR NECESIDAD INSTITUCIONAL
			64:POR RAZONES DE SALUD
			66:POR UNIDAD FAMILIAR
			724:DE FUNCIONES
			68:DE PUESTO
			58:POR INTERES PERSONAL
			59:POR RACIONALIZACIÓN
			57:POR RAZONES DE SALUD
			725:POR UNIDAD FAMILIAR
			1789:AUXILIAR DE EDUCACIÓN A ADMINISTRATIVO --> SE QUITO DE LA LINEA 180 Y SE AGREGO A LA LINEA 90
			1790: PROFESOR INTERINO A ADMINISTRATIVO --> SE QUITO DE LA LINEA 180 Y SE AGREGO A LA LINEA 90
			60:POR NECESIDAD INSTITUCIONAL
			61:POR RAZONES DE INTERES PERSONAL
			62:POR RAZONES DE SALUD
			*/
            IF (@CODIGO_MOTIVO_ACCION IN (67, 56, 65, 64, 66, 724, 68, 58, 59, 57, 725, 1789, 1790, 60, 61, 62, 52, 809, 1900, 55)) 
                BEGIN
                    SET @xml_categoria_remunerativa = 
                        (SELECT	r.Value, r.Code, r.Text, @REGLA6 AS Generico
                        FROM	(	SELECT CR.ID_CATEGORIA_REMUNERATIVA AS [Value]
                                        , CR.CODIGO_CATEGORIA_REMUNERATIVA AS Code
                                        , CR.DESCRIPCION_CATEGORIA_REMUNERATIVA AS [Text]
                                        , CR.ORDEN
                                    FROM sistema.categoria_remunerativa AS CR WITH (NOLOCK)
                                    WHERE CR.ID_REGIMEN = @ID_REGIMEN
                                        AND CR.ES_VIGENTE = 1
                                        AND CR.ES_BORRADO = 0
                                ) r 
                        ORDER BY r.ORDEN
                        FOR XML AUTO ,TYPE)

                    SET @xml_categoria_remunerativa_origen =  @xml_categoria_remunerativa

                    SET @xml_subtipo_trabajador = 
                        (SELECT	r.Value, r.Code, r.Text
                        FROM	(	SELECT ST.ID_SUBTIPO_TRABAJADOR AS [Value],
                                    ST.CODIGO_SUBTIPO_TRABAJADOR AS [Code],
                                    ST.DESCRIPCION_SUBTIPO_TRABAJADOR AS [Text]
                                    FROM [sistema].subtipo_trabajador AS ST WITH(NOLOCK)
                                    INNER JOIN sistema.area_servidor AS SA WITH(NOLOCK) ON ST.ID_AREA_SERVIDOR = SA.ID_AREA_SERVIDOR
                                    WHERE SA.ID_REGIMEN = @ID_REGIMEN 
                                        AND ST.ES_VIGENTE = 1 
                                        AND ST.ES_BORRADO = 0
                                ) r 
                        ORDER BY r.Code
                        FOR XML AUTO ,TYPE)

                    SET @xml_subtipo_trabajador_categoria_remunerativa = 
                        (SELECT	r.Value, r.[Group], r.Code 
                        FROM	(	SELECT cst.ID_CATEGORIA_REMUNERATIVA_POR_SUBTIPO_TRABAJADOR AS [Value]
                                    , st.ID_SUBTIPO_TRABAJADOR AS [Group] 
                                    , cr.ID_CATEGORIA_REMUNERATIVA AS [Code]
                                    FROM parametrica.categoria_remunerativa_por_subtipo_trabajador AS cst WITH (NOLOCK)
                                    JOIN [sistema].subtipo_trabajador AS st WITH (NOLOCK)
                                        ON cst.ID_SUBTIPO_TRABAJADOR = st.ID_SUBTIPO_TRABAJADOR
                                    JOIN [sistema].[categoria_remunerativa] AS cr WITH (NOLOCK)
                                        ON cst.ID_CATEGORIA_REMUNERATIVA = cr.ID_CATEGORIA_REMUNERATIVA
                                    WHERE cr.ID_REGIMEN = @ID_REGIMEN
                                        AND cst.ES_VIGENTE = 1
                                        AND cst.ES_BORRADO = 0
                                ) r 
                        ORDER BY r.[Group]
                        FOR XML AUTO ,TYPE)
                END
            ELSE
                BEGIN
                    SET @xml_categoria_remunerativa = 
                        (SELECT	r.Value, r.Code, r.Text, @REGLA6 AS Generico
                        FROM	(	SELECT	CR.ID_CATEGORIA_REMUNERATIVA AS [Value], CR.CODIGO_CATEGORIA_REMUNERATIVA AS Code, CR.DESCRIPCION_CATEGORIA_REMUNERATIVA AS [Text],
                                            CR.ORDEN
                                    FROM	sistema.categoria_remunerativa AS CR WITH (NOLOCK)
                                    WHERE	CR.ID_REGIMEN = @ID_REGIMEN
                                            AND CR.ES_VIGENTE = 1 AND CR.ES_BORRADO = 0
                                ) r 
                        ORDER BY r.ORDEN
                        FOR XML AUTO ,TYPE)
                END
        END
	IF EXISTS (SELECT	ER.ID_ENTIDAD_REGLA
				FROM	sistema.entidad_regla AS ER WITH (NOLOCK)
						INNER JOIN sistema.regla AS R WITH (NOLOCK) ON ER.ID_REGLA = R.ID_REGLA
				WHERE	R.CODIGO_REGLA = 37
						AND ER.ES_VIGENTE = 1
						AND ER.ID_REGISTRO_ENTIDAD = @ID_MOTIVO_ACCION)
		BEGIN
			SET @ACCION_REEMPLAZO = 1
		END

	DECLARE @xml_accion_reemplazo XML = 		
				(SELECT r.Value, r.Code, r.Text, r.Vigente
				 FROM	(	SELECT	@ACCION_REEMPLAZO Vigente, 
									0 AS [Value], 0 AS Code, '' AS [Text]
						) r 
				 FOR XML AUTO ,TYPE)
	/*
		1591: UBICACIÓN EN LA LEY 29944
		1663: ADMINISTRATIVO A DOCENTE
		1664: AUXILIAR DE EDUCACION A DOCENTE
		1779: INCORPORACION A LA LEY 29944
		1788: PROFESOR INTERINO A AUXILIAR DE EDUCACIÓN
		1789: AUXILIAR DE EDUCACION A ADMINISTRATIVO --> SE QUITO Y SE AGREGO  A LA LINEA 90
		1790: PROFESOR INTERINO A ADMINISTRATIVO --> SE QUITO Y SE AGREGO A LA LINEA 90

	*/

	IF @CODIGO_MOTIVO_ACCION IN (1360,1591,1663,1664,1779,1788)
		BEGIN
			SET @xml_categoria_remunerativa_origen = 
				(SELECT	r.Value, r.Code, r.Text, @REGLA6 AS Generico, r.[Group],
						CASE WHEN @CODIGO_MOTIVO_ACCION IN (1591,1360) THEN CONVERT(BIT,1) ELSE CONVERT(BIT,0) END AS Vigente
				 FROM	(	SELECT	CR.ID_CATEGORIA_REMUNERATIVA AS [Value], 
									CR.CODIGO_CATEGORIA_REMUNERATIVA AS Code, 
									CR.DESCRIPCION_CATEGORIA_REMUNERATIVA AS [Text], 
									CR.ORDEN,
									@CODIGO_MOTIVO_ACCION AS [Group]
							FROM	sistema.categoria_remunerativa_motivo_accion AS CTMA WITH (NOLOCK)
									INNER JOIN sistema.categoria_remunerativa AS CR WITH (NOLOCK)
										ON CTMA.ID_CATEGORIA_REMUNERATIVA = CR.ID_CATEGORIA_REMUNERATIVA
									INNER JOIN sistema.motivo_accion AS MA WITH (NOLOCK)
										ON CTMA.ID_MOTIVO_ACCION = MA.ID_MOTIVO_ACCION
							WHERE	MA.CODIGO_MOTIVO_ACCION = @CODIGO_MOTIVO_ACCION
									AND CTMA.ES_VIGENTE = 1
									AND CTMA.ORIGEN_DESTINO IN (0,2)
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)
		END
	ELSE
		BEGIN
            IF (@CODIGO_MOTIVO_ACCION NOT IN (67))
            BEGIN
                SET @xml_categoria_remunerativa_origen = 
                    (SELECT	r.Value, r.Code, r.Text, @REGLA6 AS Generico, CONVERT(BIT,0) AS Vigente
                    FROM	(	SELECT	CR.ID_CATEGORIA_REMUNERATIVA AS [Value], CR.CODIGO_CATEGORIA_REMUNERATIVA AS Code, CR.DESCRIPCION_CATEGORIA_REMUNERATIVA AS [Text],
                                        CR.ORDEN									
                                FROM	sistema.categoria_remunerativa AS CR WITH (NOLOCK)
                                WHERE	CR.ID_REGIMEN = @ID_REGIMEN
                                        AND CR.ES_VIGENTE = 1 AND CR.ES_BORRADO = 0
                                        AND NOT CR.CODIGO_CATEGORIA_REMUNERATIVA IN (167,168,169)
                            ) r 
                    ORDER BY r.ORDEN
                    FOR XML AUTO ,TYPE)
            END
		END

	IF @ID_SERVIDOR_PUBLICO <> 0
		BEGIN
			SELECT	@TOTAL = COUNT(SM.ID_SITUACION_LABORAL_MOTIVO_ACCION)
			FROM	sistema.situacion_laboral_motivo_accion AS SM WITH (NOLOCK)
				INNER JOIN sistema.situacion_laboral AS S WITH (NOLOCK)
					ON SM.ID_SITUACION_LABORAL = S.ID_SITUACION_LABORAL
			WHERE	SM.ES_VIGENTE = 1 AND SM.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION
		END

	IF @TOTAL > 1
		BEGIN
			SELECT	@ID_CONDICION_LABORAL = ID_CONDICION_LABORAL
			FROM	transaccional.accion_por_servidor_publico_registro AS ASPR WITH (NOLOCK)
			WHERE	ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO

			SET @xml_situacion_laboral = 
				(SELECT	r.Value, r.Code, r.Text, r.[Group]
				 FROM	(
							 SELECT	S.ID_SITUACION_LABORAL AS [Value], S.CODIGO_SITUACION_LABORAL AS Code,
									S.DESCRIPCION_SITUACION_LABORAL AS [Text], S.ID_CONDICION_LABORAL AS [Group]
							 FROM	sistema.situacion_laboral_motivo_accion AS SM WITH (NOLOCK)
									INNER JOIN sistema.situacion_laboral AS S WITH (NOLOCK)
										ON SM.ID_SITUACION_LABORAL = S.ID_SITUACION_LABORAL
							 WHERE	SM.ES_VIGENTE = 1 AND SM.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION
									AND S.ID_CONDICION_LABORAL = @ID_CONDICION_LABORAL
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)

			SET @xml_condicion_laboral = 
				(SELECT	r.Value, r.Code, r.Text
				 FROM	(
							SELECT DISTINCT C.ID_CONDICION_LABORAL AS [Value], C.CODIGO_CONDICION_LABORAL AS Code,
								C.DESCRIPCION_CONDICION_LABORAL AS [Text]
							FROM	sistema.condicion_laboral AS C WITH (NOLOCK)
							WHERE	C.ID_CONDICION_LABORAL = @ID_CONDICION_LABORAL
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)
		END
	ELSE
		BEGIN
			SET @xml_situacion_laboral = 
				(SELECT	r.Value, r.Code, r.Text, r.[Group]
				 FROM	(
							 SELECT	S.ID_SITUACION_LABORAL AS [Value], S.CODIGO_SITUACION_LABORAL AS Code,
									S.DESCRIPCION_SITUACION_LABORAL AS [Text], S.ID_CONDICION_LABORAL AS [Group]
							 FROM	sistema.situacion_laboral_motivo_accion AS SM WITH (NOLOCK)
									INNER JOIN sistema.situacion_laboral AS S WITH (NOLOCK)
										ON SM.ID_SITUACION_LABORAL = S.ID_SITUACION_LABORAL
							 WHERE	SM.ES_VIGENTE = 1 AND SM.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)

			SET @xml_condicion_laboral = 
				(SELECT	r.Value, r.Code, r.Text
				 FROM	(
							SELECT DISTINCT C.ID_CONDICION_LABORAL AS [Value], C.CODIGO_CONDICION_LABORAL AS Code,
								C.DESCRIPCION_CONDICION_LABORAL AS [Text]
							FROM	sistema.situacion_laboral_motivo_accion AS SM WITH (NOLOCK)
								INNER JOIN sistema.situacion_laboral AS S WITH (NOLOCK)
									ON SM.ID_SITUACION_LABORAL = S.ID_SITUACION_LABORAL
								INNER JOIN sistema.condicion_laboral AS C WITH (NOLOCK)
									ON S.ID_CONDICION_LABORAL = C.ID_CONDICION_LABORAL
							WHERE	SM.ES_VIGENTE = 1 AND SM.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION
						) r 
				 ORDER BY r.Text
				 FOR XML AUTO ,TYPE)
		END
    
    SELECT  @xml_categoria_remunerativa AS 'CATEGORIA_REMUNERATIVA',
			@xml_categoria_remunerativa_origen AS 'CATEGORIA_REMUNERATIVA_ORIGEN',
			@xml_situacion_laboral AS 'SITUACION_LABORAL',
			@xml_condicion_laboral AS 'CONDICION_LABORAL',
			@xml_accion_reemplazo AS 'ACCION_REEMPLAZO',
			@xml_subtipo_trabajador AS 'GRUPO_OCUPACIONAL',
            @xml_subtipo_trabajador_categoria_remunerativa AS 'GRUPO_OCUPACIONAL_CATEGORIA'
    FOR     XML PATH('') ,ROOT('ROOT');

END
