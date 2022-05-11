DROP PROCEDURE IF EXISTS [dbo].[USP_ESCALAFON_INS_SERVIDOR_PUBLICO_DATOS_ADICIONALES_IOXML]
GO
DROP PROCEDURE IF EXISTS [dbo].[USP_ESCALAFON_SEL_SEPARACION_PREVENTIVAS_OXML]
GO
DROP PROCEDURE IF EXISTS [dbo].[USP_ESCALAFON_ACT_SEPARACION_PREVENTIVA_ANULA]
GO
DROP PROCEDURE IF EXISTS [dbo].[USP_ESCALAFON_SEL__LISTADO_SEPARACION_PREVENTIVA]
GO
/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_INS_SERVIDOR_PUBLICO_DATOS_ADICIONALES_IOXML]    Script Date: 11/05/2022 18:18:11 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[USP_ESCALAFON_INS_SERVIDOR_PUBLICO_DATOS_ADICIONALES_IOXML]
@REQUEST_XML XML 	
AS
BEGIN

	DECLARE
		@FECHA_NOTIFICACION DATETIME,
		@FECHA_INICIO DATETIME,
		@FECHA_FIN DATETIME,
		@NOMBRE_DOCUMENTO VARCHAR(250)='',				
		@NOMBRE_ARCHIVO VARCHAR(250)='',	
		@FOLIOS_DOCUMENTO INT,
		@ANOTACIONES VARCHAR(250) = '',
		@ID_RESULTADO INT,
		@DESCRIPCION_RESULTADO VARCHAR(250),
		@ID_SEPARACION_PREVENTIVA INT,
		@DESCRIPCION_SEPARACION_PREVENTIVA VARCHAR(250),
		@ACCION VARCHAR(100),
		@USUARIO VARCHAR(20),
		@NOMBRE_USUARIO VARCHAR(50),
		@AMBITO VARCHAR(100),
		@ROL VARCHAR(100),
		@ID_ESTADO_REGISTRO INT,
		@Message VARCHAR(MAX) = '',
		@MessageReal VARCHAR(MAX) = '',
		@ID_SERVIDOR_PUBLICO INT,
		@ID_CONDICION_LABORAL INT,
		@CODIGO_GRUPO_ACCION INT,
		@ID_TIPO_RESOLUCION INT,
		@ID_SITUACION_LABORAL INT,
		@NUMERO_RESOLUCION VARCHAR(50),
		@CONDICION_LABORAL VARCHAR(50),	
		@SITUACION_LABORAL VARCHAR(50),	
		@FECHA_RESOLUCION DATE,
		@ID_MOTIVO_ACCION INT,
		@ES_HISTORICO BIT = 0,		
		@FECHA_ACTUAL DATETIME = GETDATE(),
		@ID_SITUACION_LABORAL_ACTUAL INT,
		@ID_ACCION_XSP int = 0,
		@Success BIT = 0,	
		@Severity INT = 0,
		@xml_acciones XML

	BEGIN TRANSACTION;  
  
	BEGIN TRY

		SELECT	@USUARIO = UPPER(c.value('@USUARIO', 'VARCHAR(20)')),
				@NOMBRE_USUARIO = UPPER(c.value('@NOMBRE_USUARIO', 'VARCHAR(50)')),
				@AMBITO = UPPER(c.value('@AMBITO', 'VARCHAR(100)')),
				@ROL = UPPER(c.value('@ROL', 'VARCHAR(100)'))
		FROM	@REQUEST_XML.nodes('/root/usuario/row') x(c);


		SELECT	@ID_SERVIDOR_PUBLICO = c.value('@ID_SERVIDOR_PUBLICO', 'INT'),
				@CODIGO_GRUPO_ACCION = c.value('@CODIGO_GRUPO_ACCION', 'INT'),
				@ID_TIPO_RESOLUCION = c.value('@ID_TIPO_RESOLUCION', 'INT'),
				@NUMERO_RESOLUCION = UPPER(c.value('@NUMERO_RESOLUCION', 'VARCHAR(50)')),
				@FECHA_RESOLUCION = c.value('@FECHA_RESOLUCION', 'DATE'),
				@ID_MOTIVO_ACCION = c.value('@ID_MOTIVO_ACCION', 'INT'),
				@ES_HISTORICO = c.value('@ES_HISTORICO', 'BIT'),
				@FECHA_NOTIFICACION = c.value('@FECHA_NOTIFICACION', 'DATE'),
				@FECHA_INICIO = c.value('@FECHA_INICIO', 'DATE'),
				@FECHA_FIN = c.value('@FECHA_FIN', 'DATE'),
				@ANOTACIONES = UPPER(NULLIF(c.value('@ANOTACIONES', 'VARCHAR(250)'),'')),

				@ID_RESULTADO = c.value('@ID_RESULTADO', 'INT'),
				@DESCRIPCION_RESULTADO = UPPER(c.value('@DESCRIPCION_RESULTADO', 'VARCHAR(250)')),
				@ID_SEPARACION_PREVENTIVA = c.value('@ID_SEPARACION_PREVENTIVA', 'INT'),
				@DESCRIPCION_SEPARACION_PREVENTIVA = UPPER(c.value('@DESCRIPCION_SEPARACION_PREVENTIVA', 'VARCHAR(250)')),

				@NOMBRE_DOCUMENTO = UPPER(NULLIF(c.value('@NOMBRE_DOCUMENTO', 'VARCHAR(250)'),'')),
				@NOMBRE_ARCHIVO = UPPER(NULLIF(c.value('@NOMBRE_ARCHIVO', 'VARCHAR(250)'),'')),							
				@FOLIOS_DOCUMENTO = c.value('@FOLIOS_DOCUMENTO', 'INT')
		FROM	@REQUEST_XML.nodes('/root/accion') x(c);	
		
		DECLARE @FECHA_FIN_TEXT VARCHAR(50) = (SELECT CAST(@FECHA_FIN AS date))
		IF (@FECHA_FIN_TEXT = '1900-01-01')
		SET @FECHA_FIN = NULL

		IF (@ID_SEPARACION_PREVENTIVA != -1) 
		BEGIN
			UPDATE [transaccional].[accion_por_servidor_publico]
			SET ID_ESTADO_REGISTRO = 10
			WHERE ID_ACCION_POR_SERVIDOR_PUBLICO = @ID_SEPARACION_PREVENTIVA		
		END

		INSERT INTO [transaccional].[accion_por_servidor_publico]
		(ID_MOTIVO_ACCION, ID_SERVIDOR_PUBLICO, ID_MOTIVO_POR_ESTADO_ENTIDAD, CODIGO_ACCION_POR_SERVIDOR_PUBLICO,
		FECHA_INICIO_ACCION_POR_SERVIDOR_PUBLICO, FECHA_FIN_ACCION_POR_SERVIDOR_PUBLICO, NUMERO_EXPEDIENTE_ACCION_POR_SERVIDOR_PUBLICO, CANTIDAD_FOLIOS_ACCION_POR_SERVIDOR_PUBLICO,
		OBSERVACION_ACCION_POR_SERVIDOR_PUBLICO, VERIFICADO_RNSDD, ES_MANDATO_JUDICIAL_ACCION_POR_SERVIDOR_PUBLICO, PERIODO_ACCION_POR_SERVIDOR_PUBLICO, 
		ID_TIPO_RESOLUCION, NUMERO_RESOLUCION, FECHA_RESOLUCION, 
		ID_ESTADO_REGISTRO,	ES_VIGENTE, FECHA_INICIO_VIGENCIA, FECHA_CREACION, USUARIO_CREACION, ES_BORRADO, ES_HISTORICO, DIAS,
		NOMBRE_USUARIO_CREA, AMBITO_USUARIO_CREA,ROL_USUARIO_CREA,NOMBRE_ARCHIVO,NOMBRE_DOCUMENTO, FOLIOS_DOCUMENTO)
		VALUES
		(@ID_MOTIVO_ACCION, @ID_SERVIDOR_PUBLICO, 3, '12345678', @FECHA_INICIO, @FECHA_FIN, '', 0,
		 @ANOTACIONES,	0, 0, '2020', @ID_TIPO_RESOLUCION, @NUMERO_RESOLUCION, @FECHA_RESOLUCION,
		5, 1, @FECHA_ACTUAL, @FECHA_ACTUAL, @USUARIO, 0, @ES_HISTORICO,NULL, @NOMBRE_USUARIO, @AMBITO, @ROL,
		@NOMBRE_ARCHIVO,@NOMBRE_DOCUMENTO, @FOLIOS_DOCUMENTO)
		SET @ID_ACCION_XSP = SCOPE_IDENTITY();
		
		INSERT INTO [transaccional].[accion_por_servidor_publico_datos_adicionales]
		(ID_ACCION_POR_SERVIDOR_PUBLICO,		
		SECCION, PESTANIA,  		
		FECHA_NOTIFICACION, FECHA_INICIO, FECHA_FIN, ANOTACIONES,
		ID_RESULTADO, DESCRIPCION_RESULTADO,
		ID_SEPARACION_PREVENTIVA, DESCRIPCION_SEPARACION_PREVENTIVA,
		FECHA_CREACION, USUARIO_CREACION, ES_BORRADO)
		VALUES
		(@ID_ACCION_XSP,		
		'OTROS', 'SUSPENSION_PREVENTIVA',		
		@FECHA_NOTIFICACION, @FECHA_INICIO, @FECHA_FIN, @ANOTACIONES,
		@ID_RESULTADO, @DESCRIPCION_RESULTADO, @ID_SEPARACION_PREVENTIVA, @DESCRIPCION_SEPARACION_PREVENTIVA,
		@FECHA_ACTUAL, @USUARIO, 0)		
		

		SELECT	@ID_CONDICION_LABORAL = ASPR.ID_CONDICION_LABORAL
		FROM	transaccional.accion_por_servidor_publico_registro AS ASPR WITH (NOLOCK)
		WHERE	ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
		
		SELECT	@ID_CONDICION_LABORAL = S.ID_CONDICION_LABORAL,			
				@ID_SITUACION_LABORAL  = S.ID_SITUACION_LABORAL				
		FROM	[sistema].[situacion_laboral_motivo_accion] AS SMA 
				INNER JOIN [sistema].[situacion_laboral] AS S ON SMA.ID_SITUACION_LABORAL = S.ID_SITUACION_LABORAL
				INNER JOIN [sistema].[condicion_laboral] AS C ON C.ID_CONDICION_LABORAL = S.ID_CONDICION_LABORAL
		WHERE	SMA.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION AND C.ID_CONDICION_LABORAL = @ID_CONDICION_LABORAL

		IF @ID_SITUACION_LABORAL_ACTUAL NOT IN (1,11,20)
			BEGIN
				SELECT	@ACCION = A.DESCRIPCION_ACCION
				FROM	sistema.motivo_accion AS MA WITH (NOLOCK)
						INNER JOIN sistema.accion AS A WITH (NOLOCK) ON MA.ID_ACCION = A.ID_ACCION
				WHERE	MA.ID_MOTIVO_ACCION = @ID_MOTIVO_ACCION

				--Actualizando registro
				UPDATE	A
				SET		ID_CONDICION_LABORAL = @ID_CONDICION_LABORAL,			CONDICION_LABORAL = @CONDICION_LABORAL,
						ID_SITUACION_LABORAL = @ID_SITUACION_LABORAL,			SITUACION_LABORAL = @SITUACION_LABORAL,
						ACCION = @ACCION,										ID_ACCION_POR_SERVIDOR_PUBLICO = @ID_ACCION_XSP
				FROM	transaccional.accion_por_servidor_publico_registro AS A
				WHERE	A.ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
						
			END	

		
		SET @Success = 1
		SET @Message = 'El registro se guardó satisfactoriamente.'

	END TRY  
	BEGIN CATCH  

		SET @MessageReal = ERROR_MESSAGE()
		SET @Message = @MessageReal
		SET @Severity = ERROR_SEVERITY()

		IF @Severity = 16
			SET @Message = 'Ha ocurrido un error. Contacte con el administrador del sistema.'
  
		IF @@TRANCOUNT > 0  
			ROLLBACK TRANSACTION;  
	END CATCH;  
  
	IF @@TRANCOUNT > 0  
		COMMIT TRANSACTION;

	IF @Severity = 16 AND @Success = 0
		BEGIN
			INSERT INTO [auditoria].[log_error]
			([NOMBRE_SP], [INPUT_PARAMS_XML], [MENSAJE_BD], [MENSAJE_USUARIO], [FECHA_CREACION], [USUARIO_CREACION])
			VALUES
			('USP_ESCALAFON_INS_ACCION_VACACIONES_ESCALAFON_IOXML', @REQUEST_XML, @MessageReal, @Message, @FECHA_ACTUAL, @USUARIO)
		END

	DECLARE @xml_status_response XML = 
		(SELECT	r.Success, r.Mensaje as [Message], r.[Value]
		 FROM	(SELECT	@Success AS Success, 
						@Message AS Mensaje,
						@ID_ACCION_XSP AS [Value]
				) r
	FOR XML AUTO ,TYPE)

	SELECT  @xml_status_response AS 'StatusResponse',
			@xml_acciones AS 'AccionServidorResponse'
    FOR     XML PATH('') ,ROOT('ROOT');

END
GO


/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_SEL_SEPARACION_PREVENTIVAS_OXML]    Script Date: 11/05/2022 18:18:56 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[USP_ESCALAFON_SEL_SEPARACION_PREVENTIVAS_OXML] 
	@ID_SERVIDOR_PUBLICO INT,    
    @pPaginaActual INT,
	@pTamanioPagina INT
AS
BEGIN

	SET DATEFORMAT 'dmy'
	

	DECLARE @xml_acciones XML = 
		(SELECT		r.Anular, r.MotivoAccion, r.Regimen,r.TipoResolucion, r.TipoResolucionAbreviado, r.NumeroResolucion, r.FechaResolucion, 
					r.FechaNotificacion, r.FechaInicio,r.FechaFin, 
					r.Anotaciones, r.Estado, r.IdAccionPorServidorPublicoDatosAdiciones, r.IdAccionPorServidorPublico,
					r.FechaCrea, r.NombreUsuarioCrea, r.AmbitoUsuarioCrea, r.RolUsuarioCrea, r.FechaModifica, r.NombreUsuarioModifica, r.AmbitoUsuarioModifica, r.RolUsuarioModifica,
					r.NombreDocumento,r.NombreArchivo,r.FoliosDocumento,
		        r.[Row], r.Total, 26 AS CodigoGrupoAccion
		 FROM	(	
						SELECT	CONVERT(BIT,CASE WHEN er.CODIGO_ESTADO_REGISTRO IN (5) THEN 1 Else 0  END) AS Anular
								, a.DESCRIPCION_ACCION as Accion
								, ma.DESCRIPCION_MOTIVO_ACCION as MotivoAccion
								, r.DESCRIPCION_REGIMEN as Regimen 
								, tr.NOMBRE_TIPO_RESOLUCION as TipoResolucion
								, tr.ABREVIATURA as TipoResolucionAbreviado
								, asp.NUMERO_RESOLUCION  AS NumeroResolucion
								, FORMAT(asp.FECHA_RESOLUCION , 'dd/MM/yyyy') as FechaResolucion
								, FORMAT(aspda.FECHA_NOTIFICACION , 'dd/MM/yyyy') as FechaNotificacion
								, FORMAT(aspda.FECHA_INICIO , 'dd/MM/yyyy') as FechaInicio
								, FORMAT(aspda.FECHA_FIN, 'dd/MM/yyyy')as FechaFin							
								, aspda.ANOTACIONES as Anotaciones
								, er.DESCRIPCION_ESTADO_REGISTRO as Estado								
								, aspda.ID_ACCION_POR_SERVIDOR_PUBLICO_DATOS_ADICIONALES as IdAccionPorServidorPublicoDatosAdiciones
								, aspda.ID_ACCION_POR_SERVIDOR_PUBLICO as IdAccionPorServidorPublico
								, FORMAT(asp.FECHA_CREACION,'dd/MM/yyyy hh:mm tt') AS FechaCrea
								, ISNULL(asp.NOMBRE_USUARIO_CREA,'') AS NombreUsuarioCrea
								, ISNULL(asp.AMBITO_USUARIO_CREA,'') AS AmbitoUsuarioCrea
								, ISNULL(asp.ROL_USUARIO_CREA,'') AS RolUsuarioCrea
								, FORMAT(asp.FECHA_MODIFICACION,'dd/MM/yyyy hh:mm tt') AS FechaModifica
								, ISNULL(asp.NOMBRE_USUARIO_MODIFICA,'') AS NombreUsuarioModifica
								, ISNULL(asp.AMBITO_USUARIO_MODIFICA,'') AS AmbitoUsuarioModifica
								, ISNULL(asp.ROL_USUARIO_MODIFICA,'') AS RolUsuarioModifica
                                , asp.NOMBRE_DOCUMENTO as NombreDocumento
								, asp.NOMBRE_ARCHIVO as NombreArchivo
								, asp.FOLIOS_DOCUMENTO as FoliosDocumento
								--, ga.CODIGO_GRUPO_ACCION as CodigoGrupoAccion

								, ROW_NUMBER() OVER (ORDER BY asp.FECHA_CREACION DESC) AS [Row]
								, COUNT(1) OVER() AS Total 										
						FROM	transaccional.accion_por_servidor_publico_datos_adicionales as  aspda  WITH (NOLOCK) 
								inner join transaccional.accion_por_servidor_publico as asp WITH (NOLOCK) ON asp.ID_ACCION_POR_SERVIDOR_PUBLICO = aspda.ID_ACCION_POR_SERVIDOR_PUBLICO
								inner join sistema.motivo_accion  as ma  WITH (NOLOCK) on asp.ID_MOTIVO_ACCION = ma.ID_MOTIVO_ACCION 
								inner join sistema.accion as a  WITH (NOLOCK) on ma.ID_ACCION = a.ID_ACCION 
								inner join parametrica.regimen_por_grupo_accion as rga  WITH (NOLOCK) on a.ID_REGIMEN_POR_GRUPO_ACCION = rga.ID_REGIMEN_POR_GRUPO_ACCION 
								inner join sistema.grupo_accion ga  WITH (NOLOCK) on rga.ID_GRUPO_ACCION = ga.ID_GRUPO_ACCION 
								inner join sistema.regimen r WITH (NOLOCK) on rga.ID_REGIMEN = r.ID_REGIMEN 
								inner join sistema.tipo_resolucion  tr  WITH (NOLOCK) on asp.ID_TIPO_RESOLUCION = tr.ID_TIPO_RESOLUCION 
								inner join sistema.estado_registro as er  WITH (NOLOCK) on asp.ID_ESTADO_REGISTRO = er.ID_ESTADO_REGISTRO
						WHERE	asp.ID_SERVIDOR_PUBLICO =  @ID_SERVIDOR_PUBLICO	
								and ga.ID_GRUPO_ACCION = 26 --SEPARACIÓN PREVENTIVA
						ORDER BY aspda.FECHA_CREACION	DESC

					OFFSET ((@pPaginaActual - 1) * @pTamanioPagina) ROWS
					FETCH NEXT @pTamanioPagina ROWS ONLY
				) r 
		 FOR XML AUTO ,TYPE)

	SELECT  @xml_acciones AS 'SEPARACION_PREVENTIVA'
    FOR     XML PATH('') ,ROOT('ROOT');

END
GO


/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_ACT_SEPARACION_PREVENTIVA_ANULA]    Script Date: 11/05/2022 18:19:57 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[USP_ESCALAFON_ACT_SEPARACION_PREVENTIVA_ANULA]
	--@ID_BENEFICIO_SERVIDOR_PUBLICO INT,
	--@USUARIO VARCHAR(12)
	@REQUEST_XML XML
AS
BEGIN

--	DECLARE @REQUEST_XML XML = 
--	'<root>
--  <separacionPreventiva ID_ACCION_POR_SERVIDOR_PUBLICO="5648767" />
--  <usuario>
--    <row USUARIO="42813053" NOMBRE_USUARIO="CHRISTIAN ABELARDO SCHEREIBER CORNELIO" ID_CENTRO_AMBITO="173" ID_TIPO_CENTRO_AMBITO="2" AMBITO="UGEL San Juan de Lurigancho 05" CODIGO_ROL="GSD002" ROL="Técnico de escalafón " FECHA_CREACION="05/05/2022 11:32 a. m." ID_USUARIO="80874" />
--  </usuario>
--</root>'

	DECLARE	@ID_PERSONA INT = 0,
			@ID_SERVIDOR_PUBLICO INT = 0,
			@ID_TIPO_DIRECCION INT = 0,
			@CODIGO_TIPO_DIRECCION INT = 0,			
			@FECHA_ACTUAL DATETIME = GETDATE(),
			@ID_ESTADO_ANULADO INT,
			@NOMBRE_USUARIO VARCHAR(50),
			@AMBITO VARCHAR(100),
			@ROL VARCHAR(100),
			@USUARIO VARCHAR(12),
			@ID_ACCION_POR_SERVIDOR_PUBLICO INT,
			@ES_VIGENTE BIT,
			@CODIGO_ESTADO_REGISTRO INT,
			@ID_ENTIDAD INT,

			@Success BIT = 0,
			@Severity INT = 0,
			@Message VARCHAR(MAX) = '',
			@MessageReal VARCHAR(MAX) = ''

	BEGIN TRANSACTION;  
  
	BEGIN TRY

		SELECT	@USUARIO = UPPER(c.value('@USUARIO', 'VARCHAR(20)')),
				@NOMBRE_USUARIO = UPPER(c.value('@NOMBRE_USUARIO', 'VARCHAR(50)')),
				@AMBITO = UPPER(c.value('@AMBITO', 'VARCHAR(100)')),
				@ROL = UPPER(c.value('@ROL', 'VARCHAR(100)'))
		FROM	@REQUEST_XML.nodes('/root/usuario/row') x(c);

		SELECT	@ID_ESTADO_ANULADO = ID_ESTADO_REGISTRO
		FROM	sistema.estado_registro AS ER WITH (NOLOCK)
		WHERE	ER.CODIGO_ESTADO_REGISTRO = 6 --ANULADO

		SELECT	@ID_ACCION_POR_SERVIDOR_PUBLICO = c.value('@ID_ACCION_POR_SERVIDOR_PUBLICO', 'INT')
		FROM	@REQUEST_XML.nodes('/root/separacionPreventiva') x(c);

		--SELECT 
		--@ID_ACCION_POR_SERVIDOR_PUBLICO
		UPDATE	B
				SET		ID_ESTADO_REGISTRO = 6,
						FECHA_MODIFICACION = @FECHA_ACTUAL, 
						USUARIO_MODIFICACION = @USUARIO
				FROM	transaccional.accion_por_servidor_publico AS B
				WHERE	B.ID_ACCION_POR_SERVIDOR_PUBLICO = @ID_ACCION_POR_SERVIDOR_PUBLICO	

		SET @Success = 1
		SET @Message = 'El registro se anuló satisfactoriamente.'


	END TRY  
	BEGIN CATCH

		SET @MessageReal = ERROR_MESSAGE()
		SET @Message = @MessageReal
		SET @Severity = ERROR_SEVERITY()

		IF @Severity = 16
			SET @Message = 'Ha ocurrido un error. Contacte con el administrador del sistema.'
  
		IF @@TRANCOUNT > 0  
			ROLLBACK TRANSACTION;  
	END CATCH;  
  
	IF @@TRANCOUNT > 0  
		COMMIT TRANSACTION;

	IF @Severity = 16 AND @Success = 0
		BEGIN
			INSERT INTO [auditoria].[log_error]
			([NOMBRE_SP], [INPUT_PARAMS_XML], [MENSAJE_BD], [MENSAJE_USUARIO], [FECHA_CREACION], [USUARIO_CREACION])
			VALUES
			('USP_ESCALAFON_ACT_SEPARACION_PREVENTIVA_ANULA', @REQUEST_XML, @MessageReal, @Message, @FECHA_ACTUAL, @USUARIO)
		END

	SELECT	@Success AS Success,
			@Message AS [Message],
			@ID_ACCION_POR_SERVIDOR_PUBLICO AS [Value]

END
GO

/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_SEL__LISTADO_SEPARACION_PREVENTIVA]    Script Date: 11/05/2022 18:22:15 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[USP_ESCALAFON_SEL__LISTADO_SEPARACION_PREVENTIVA]    
@IdServidorPublico INT      
AS    
BEGIN
SET NOCOUNT ON;    
    
SELECT	b.ABREVIATURA +' - '+ a.NUMERO_RESOLUCION + '  - '+ 
			CONVERT(VARCHAR,c.FECHA_NOTIFICACION, 103) AS DETALLE_SEPARACION
			,a.ID_ACCION_POR_SERVIDOR_PUBLICO, ID_SERVIDOR_PUBLICO 
	FROM  transaccional.accion_por_servidor_publico a 
	INNER JOIN sistema.tipo_resolucion b on a.ID_TIPO_RESOLUCION = b.ID_TIPO_RESOLUCION
	INNER JOIN  transaccional.accion_por_servidor_publico_datos_adicionales c on a.ID_ACCION_POR_SERVIDOR_PUBLICO = c.ID_ACCION_POR_SERVIDOR_PUBLICO
	WHERE  
	a.ID_ESTADO_REGISTRO = 5
	AND a.ID_SERVIDOR_PUBLICO = @IdServidorPublico
	AND a.ES_VIGENTE = 1
	AND c.FECHA_FIN IS NULL
	 
END
GO
