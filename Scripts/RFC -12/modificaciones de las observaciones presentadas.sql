USE [EducaRH_Desa]
GO
/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_INS_DATOS_SANCION_JUDICIAL]    Script Date: 20/05/2022 18:40:36 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*

EXEC [dbo].[USP_ESCALAFON_INS_DATOS_SANCION_JUDICIAL]
'<root>
  <datos_sancion CAUSA_SANCION="HGFHGF" FECHA_NOTIFICACION="2022-05-11T05:00:00Z" FECHA_INICIO="1900-01-01T00:00:00" FECHA_FIN="1900-01-01T00:00:00" DIAS_SANCION="-1" ID_SERVIDOR_PUBLICO="183096" ID_TIPO_RESOLUCION="21" NUMERO_RESOLUCION="165345-6465-4" FECHA_RESOLUCION="2022-05-10T05:00:00Z" ENTIDAD_EMISORA_RESOLUCION="HGFHGF" NOMBRE_DOCUMENTO="archivoPrueba.pdf" NOMBRE_ARCHIVO="2/50ada96b-75d1-ec11-b81b-0050569005a4" FOLIOS_DOCUMENTO="2" ES_DESTITUCION="true" OBSERVACION="" CANTIDAD_ANIOS_INHABILITACION="1" FECHA_INICIO_INHABILITACION="2022-05-12T05:00:00Z" FECHA_FIN_INHABILITACION="2023-05-12T05:00:00Z" />
  <usuario>
    <row USUARIO="42813053" NOMBRE_USUARIO="CHRISTIAN ABELARDO SCHEREIBER CORNELIO" ID_CENTRO_AMBITO="173" ID_TIPO_CENTRO_AMBITO="2" AMBITO="UGEL San Juan de Lurigancho 05" CODIGO_ROL="GSD002" ROL="Técnico de escalafón " FECHA_CREACION="11/05/2022 04:59 p. m." ID_USUARIO="80874" />
  </usuario>
</root>'

*/

ALTER PROCEDURE [dbo].[USP_ESCALAFON_INS_DATOS_SANCION_JUDICIAL] 
@REQUEST_XML XML      
AS      
BEGIN      

 DECLARE      
   @ID_PERSONA INT = 0,      
   @ID_SERVIDOR_PUBLICO INT = 0,      
   @USUARIO VARCHAR(20) = '',      
   @FECHA_ACTUAL DATETIME = GETDATE(),
   @FECHA_HOY DATE = GETDATE(),

   @NOMBRE_USUARIO VARCHAR(50),
   @AMBITO VARCHAR(100),
   @ROL VARCHAR(100),
      
   @ID_ESTADO_APROBADO INT,  
   @ID_SANCION_POR_SERVIDOR_PUBLICO INT,   

   @NUMERO_RESOLUCION VARCHAR(50),					
   @ID_TIPO_RESOLUCION INT,				
   @FECHA_RESOLUCION DATE,   
   @FECHA_INICIO_SET DATETIME ,
   @FECHA_FIN_SET DATETIME ,
   @DIAS_SANCION INT,
   @DIF_DIAS INT = 0,
      
   @Success BIT = 0,      
   @Severity INT = 0,      
   @Message VARCHAR(MAX) = '',      
   @MessageReal VARCHAR(MAX) = '',
   @ES_DESTITUCION BIT = 0,
   @ID_SITUACION_LABORAL INT = 0,
   @DESC_SITUACION_LABORAL_ACTUAL VARCHAR(10) =  '',
   @FECHA_FIN_INHABILITACION DATE 
BEGIN TRANSACTION;        
        
BEGIN TRY      
    

		SELECT	@USUARIO = UPPER(c.value('@USUARIO', 'VARCHAR(20)')),
				@NOMBRE_USUARIO = UPPER(c.value('@NOMBRE_USUARIO', 'VARCHAR(50)')),
				@AMBITO = UPPER(c.value('@AMBITO', 'VARCHAR(100)')),
				@ROL = UPPER(c.value('@ROL', 'VARCHAR(100)'))
		FROM	@REQUEST_XML.nodes('/root/usuario/row') x(c);
      
		SELECT @ID_ESTADO_APROBADO = ID_ESTADO_REGISTRO      
		FROM sistema.estado_registro AS ER WITH (NOLOCK)      
		WHERE ER.CODIGO_ESTADO_REGISTRO = 5 --APROBADO      
      
  
  
  
  
		IF OBJECT_ID('tempdb..#DATOS_SANCIONES_JUDICIALES') IS NOT NULL       
			DROP TABLE #DATOS_SANCIONES_JUDICIALES      
      
    
		  CREATE TABLE #DATOS_SANCIONES_JUDICIALES    
		  (
			CODIGO_ESTADO_REGISTRO INT,      
			DIAS_SANCION INT,      
			ID_SERVIDOR_PUBLICO INT,    
			CAUSA_SANCION VARCHAR(250), 
			FECHA_NOTIFICACION VARCHAR(50), 
			FECHA_INICIO VARCHAR(50),   
			FECHA_FIN VARCHAR(50), 
			--USUARIO_CREACION VARCHAR(12) ,  
			ID_TIPO_RESOLUCION INT,
			NUMERO_RESOLUCION VARCHAR(50),
			FECHA_RESOLUCION DATE,
			ENTIDAD_EMISORA_RESOLUCION VARCHAR(60),
			NOMBRE_DOCUMENTO VARCHAR(250),
			NOMBRE_ARCHIVO VARCHAR(250),
			FOLIOS_DOCUMENTO INT,
			OBSERVACION VARCHAR(255),
			ES_DESTITUCION BIT,
			CANTIDAD_ANIOS_INHABILITACION INT,
			FECHA_INICIO_INHABILITACION DATE,
			FECHA_FIN_INHABILITACION DATE
		  )      
   
  INSERT INTO #DATOS_SANCIONES_JUDICIALES      
  SELECT   c.value('@CODIGO_ESTADO_REGISTRO', 'INT')      
    , c.value('@DIAS_SANCION', 'INT')      
    , c.value('@ID_SERVIDOR_PUBLICO', 'INT')      
    , UPPER(NULLIF(c.value('@CAUSA_SANCION', 'VARCHAR(250)'),''))
    , CONVERT(DATETIME,CONVERT(VARCHAR(12),c.value('@FECHA_NOTIFICACION', 'DATETIME')))   
    , CONVERT(DATETIME,CONVERT(VARCHAR(12),c.value('@FECHA_INICIO', 'DATETIME')))   
    , CONVERT(DATETIME,CONVERT(VARCHAR(12),c.value('@FECHA_FIN', 'DATETIME')))   
    --, NULLIF(c.value('@USUARIO_CREACION', 'VARCHAR(12)'),'')     
    , c.value('@ID_TIPO_RESOLUCION', 'INT')      
    , UPPER(NULLIF(c.value('@NUMERO_RESOLUCION', 'VARCHAR(50)'),''))
    , c.value('@FECHA_RESOLUCION', 'DATE')    
    , UPPER(NULLIF(c.value('@ENTIDAD_EMISORA_RESOLUCION', 'VARCHAR(60)'),''))
    , NULLIF(c.value('@NOMBRE_DOCUMENTO', 'VARCHAR(250)'),'')
    , NULLIF(c.value('@NOMBRE_ARCHIVO', 'VARCHAR(250)'),'')
    , c.value('@FOLIOS_DOCUMENTO', 'INT')	
    , NULLIF(c.value('@OBSERVACION', 'VARCHAR(250)'),'')
	, c.value('@ES_DESTITUCION', 'BIT')	
	, c.value('@CANTIDAD_ANIOS_INHABILITACION', 'INT')
	, c.value('@FECHA_INICIO_INHABILITACION', 'DATE')
	, c.value('@FECHA_FIN_INHABILITACION', 'DATE')
  FROM @REQUEST_XML.nodes('/root/datos_sancion') x(c);
    ---select  * from #DATOS_SANCIONES_JUDICIALES    
  
    
	  SELECT @ID_SERVIDOR_PUBLICO = ID_SERVIDOR_PUBLICO--,      
		   --@USUARIO = USUARIO_CREACION   
			, @ID_TIPO_RESOLUCION = ID_TIPO_RESOLUCION
			, @NUMERO_RESOLUCION = NUMERO_RESOLUCION
			, @FECHA_RESOLUCION = FECHA_RESOLUCION
			, @FECHA_INICIO_SET = FECHA_INICIO
			, @FECHA_FIN_SET = FECHA_FIN
			,@DIAS_SANCION = DIAS_SANCION
	   FROM #DATOS_SANCIONES_JUDICIALES     
    
		IF @FECHA_RESOLUCION > @FECHA_HOY
			BEGIN
				SET @Message = 'La feha de la resolución no puede ser mayor a la fecha actual.';
				RAISERROR(@Message,11,1)
			END

		IF @ID_SERVIDOR_PUBLICO = 0      
		BEGIN      
		RAISERROR('Error. No se pudo identificar al servidor.',11,1)          
		END   

		IF YEAR(@FECHA_INICIO_SET) = 1900
				SET @FECHA_INICIO_SET = NULL

	
		IF YEAR(@FECHA_FIN_SET) = 1900
				SET @FECHA_FIN_SET = NULL

		IF @DIAS_SANCION = -1
		BEGIN
			SET  @DIAS_SANCION = NULL
		ENd	

		/*VALIDACION SI ES QUE LA DIFERENCIA DE AÑOS ES IGUAL A LA CANTIDAD DE DIAS*/
		IF(@FECHA_INICIO_SET IS NOT NULL AND @FECHA_FIN_SET IS NOT NULL AND @DIAS_SANCION IS NOT NULL)
		BEGIN
			SET @DIF_DIAS = DATEDIFF(DAY,@FECHA_INICIO_SET,@FECHA_FIN_SET) + 1
			IF(@DIF_DIAS <> @DIAS_SANCION)
			BEGIN
					SET @Message = 'No se puede registrar la acción, debido a que las fechas de inicio, fecha fin o cantidad de días no son válidas.';
					RAISERROR(@Message,11,1)
			END
		END

		/*BEGIN VALIDACION POR TIPO, NUMERO RESOLUCION Y AÑO DE RESOLUCION*/
		IF EXISTS(	SELECT	1
						FROM	transaccional.sancion_por_servidor_publico AS SSP WITH (NOLOCK)
								INNER JOIN sistema.estado_registro AS ER WITH (NOLOCK) ON SSP.ID_ESTADO_REGISTRO_JUDICIAL = ER.ID_ESTADO_REGISTRO
						WHERE	
								SSP.ID_TIPO_SANCION = 9 --SANCIONES JUDICIALES
								AND SSP.ID_TIPO_RESOLUCION = @ID_TIPO_RESOLUCION
								AND SSP.NUMERO_RESOLUCION = @NUMERO_RESOLUCION
								AND YEAR(SSP.FECHA_RESOLUCION) = YEAR(@FECHA_RESOLUCION)
								AND ER.CODIGO_ESTADO_REGISTRO <> 6 --DIFERENTE DE ANULADO
								AND SSP.ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO)
				BEGIN
					SET @Message = 'No se pudo realizar la acción, debido a que ya existe un registro igual al ingresado.';
					RAISERROR(@Message,11,1)
				END
		/*END VALIDACION POR TIPO Y NUMERO RESOLUCION*/

		/*INICIO ---> ACTUALIZAR LA SITUACION LABORAL - CONDICION EN DESTITUCION IGUAL FALSE*/		
		SELECT @DESC_SITUACION_LABORAL_ACTUAL = SITUACION_LABORAL FROM transaccional.accion_por_servidor_publico_registro (NOLOCK) WHERE ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
		IF (@DESC_SITUACION_LABORAL_ACTUAL != 'CESADO')
		BEGIN
			SELECT @ES_DESTITUCION = ES_DESTITUCION    
			FROM #DATOS_SANCIONES_JUDICIALES   
			--PRINT ('@ES_DESTITUCION ' + CONVERT (VARCHAR(10), @ES_DESTITUCION))
			IF (@ES_DESTITUCION = 0)
			BEGIN
				IF (@FECHA_FIN_SET >= GETDATE())
				BEGIN
					SELECT @ID_SITUACION_LABORAL = ID_CONDICION_LABORAL FROM [sistema].[situacion_laboral] (NOLOCK) WHERE ID_CONDICION_LABORAL = (SELECT ID_REGIMEN FROM  transaccional.accion_por_servidor_publico_registro (NOLOCK) 
																												WHERE ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO) AND DESCRIPCION_SITUACION_LABORAL LIKE '%CON SANCIÓN%'
					--PRINT ('@ID_SITUACION_LABORAL ' + CONVERT (VARCHAR(10), @ID_SITUACION_LABORAL))

					UPDATE transaccional.accion_por_servidor_publico_registro 
					SET ID_SITUACION_LABORAL = @ID_SITUACION_LABORAL,
					SITUACION_LABORAL = 'CON SANCIÓN'
					WHERE ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
				END
			END

			IF (@ES_DESTITUCION = 1)
			BEGIN			
			
				SELECT @FECHA_FIN_INHABILITACION = FECHA_FIN_INHABILITACION FROM #DATOS_SANCIONES_JUDICIALES  
				--PRINT (CONVERT(varchar,@FECHA_FIN_INHABILITACION,101) + 'FECHA_FIN_INHABILITACION')
				IF (@FECHA_FIN_INHABILITACION >= GETDATE())				
				BEGIN
					SELECT @ID_SITUACION_LABORAL = ID_CONDICION_LABORAL FROM [sistema].[situacion_laboral] (NOLOCK)  WHERE ID_CONDICION_LABORAL = (SELECT ID_REGIMEN FROM  transaccional.accion_por_servidor_publico_registro (NOLOCK) 
																												WHERE ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO) AND DESCRIPCION_SITUACION_LABORAL LIKE '%CESADO%'
					--PRINT ('@ID_SITUACION_LABORAL ' + CONVERT (VARCHAR(10), @ID_SITUACION_LABORAL))

					UPDATE transaccional.accion_por_servidor_publico_registro 
					SET ID_SITUACION_LABORAL = @ID_SITUACION_LABORAL,
					SITUACION_LABORAL = 'CESADO'
					WHERE ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
				END
			END
		END
		/*FIN ----> ACTUALIZAR LA SITUACION LABORAL - CONDICION EN DESTITUCION IGUAL FALSE*/
     
		SELECT @ID_PERSONA = ID_PERSONA      
		FROM maestro.servidor_publico AS SP WITH (NOLOCK)      
		WHERE SP.ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO      
    
  
		INSERT INTO transaccional.sancion_por_servidor_publico   
		(  
			[ID_SERVIDOR_PUBLICO]  
			,ID_TIPO_SANCION
			,ID_ESTADO_REGISTRO_JUDICIAL 
			,CAUSA_SANCION_POR_SERVIDOR_PUBLICO
			,FECHA_NOTIFICA_SANCION_SANCION_POR_SERVIDOR_PUBLICO
			,FECHA_INICIO_VIGENCIA
			,FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO  
			,FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO  
			, DIAS_SANCION_SANCION_POR_SERVIDOR_PUBLICO
			,[ES_VIGENTE]  
			,[FECHA_CREACION]
			,[NOMBRE_DOCUMENTO]
			,[NOMBRE_ARCHIVO]
			,[FOLIOS_DOCUMENTO]
			,[OBSERVACION_SANCION_POR_SERVIDOR_PUBLICO]
			--,[USUARIO_CREACION]
			, USUARIO_CREACION
			, NOMBRE_USUARIO_CREA
			, AMBITO_USUARIO_CREA
			, ROL_USUARIO_CREA

			,[ES_BORRADO]  
			,ID_TIPO_RESOLUCION
			,NUMERO_RESOLUCION
			,FECHA_RESOLUCION
			,ENTIDAD_EMISORA_RESOLUCION
			,ES_DESTITUCION
			,CANTIDAD_ANIOS_INHABILITACION
			,FECHA_INICIO_INHABILITACION
			,FECHA_FIN_INHABILITACION
		)      
		SELECT     
  
		@ID_SERVIDOR_PUBLICO ,  
		9,
		@ID_ESTADO_APROBADO,  
		CAUSA_SANCION,  
		FECHA_NOTIFICACION,  
		@FECHA_ACTUAL,  
		@FECHA_INICIO_SET,  
		@FECHA_FIN_SET,  
		@DIAS_SANCION,
		1,  
		@FECHA_ACTUAL,
			NOMBRE_DOCUMENTO,
			NOMBRE_ARCHIVO,
			FOLIOS_DOCUMENTO,
			OBSERVACION,
		--USUARIO_CREACION,  
		@USUARIO, 
		@NOMBRE_USUARIO, 
		@AMBITO,
		@ROL,

		0 ,
		ID_TIPO_RESOLUCION,
		NUMERO_RESOLUCION,
		FECHA_RESOLUCION,
		ENTIDAD_EMISORA_RESOLUCION,
		ES_DESTITUCION
		,CANTIDAD_ANIOS_INHABILITACION
		,FECHA_INICIO_INHABILITACION
		,FECHA_FIN_INHABILITACION
		FROM #DATOS_SANCIONES_JUDICIALES      
		SET @ID_SANCION_POR_SERVIDOR_PUBLICO = SCOPE_IDENTITY();      
    

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
   ('USP_ESCALAFON_INS_DATOS_SANCION_JUDICIAL', @REQUEST_XML, @MessageReal, @Message, @FECHA_ACTUAL, @USUARIO)      
  END      
      
 SELECT @Success AS Success,      
   @Message AS [Message],      
   @ID_SANCION_POR_SERVIDOR_PUBLICO AS [Value]      
      
END




USE [EducaRH_Desa]
GO
/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_SEL_SANCIONES_JUDICIALES_OXML]    Script Date: 9/05/2022 15:54:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/******************************************************************************************************        
   AUTOR    :       
   FECHA DE CREACIÓN : 01/01/2016        
   LLAMADO POR   : CUS-ESC-002 Gestionar Sanciones Judiciales        
   DESCRIPCIÓN   : Procedimiento que muestra un listado de sanciones judiciales        
   REVISIONES   :        
   ----------------------------------------------------------------------------------------------------        
   VERSIÓN FECHA MODIF.   USUARIO      DESCRIPCIÓN        
   ----------------------------------------------------------------------------------------------------        
   1.0.0      Versión inicial        
          
   TEST   :          
   ---------------------------------------------------------------------------------------------------        
  EXEC [USP_ESCALAFON_SEL_SANCIONES_JUDICIALES_OXML] 121576,'GSD002',1,10
******************************************************************************************************/        
       
ALTER PROCEDURE [dbo].[USP_ESCALAFON_SEL_SANCIONES_JUDICIALES_OXML]  
	@IdServidorPublico INT = 0,
	@CodigoRol VARCHAR(10),
	@pPaginaActual INT,
	@pTamanioPagina INT
AS  
 BEGIN  
  
	SET NOCOUNT ON;
  	SET DATEFORMAT 'dmy'

	DECLARE @CODIGO_ROL_ESC INT,
			@HABILITA_MODIFICATORIA BIT = 0

	SELECT	@HABILITA_MODIFICATORIA = MS.ES_VIGENTE
	FROM	sistema.modificatoria_seccion AS MS WITH (NOLOCK)
	WHERE	MS.CODIGO_MODIFICATORIA_SECCION = 12 --SECCION 8 - SANCIONES JUDICIALES

	SELECT	@CODIGO_ROL_ESC = R.CODIGO_ROL
	FROM	maestro.rol AS R WITH (NOLOCK)
	WHERE	R.CODIGO_ROL_PASSPORT = @CodigoRol

	DECLARE @xml_pension XML = 


 (SELECT		   
				r.ID_SANCION_POR_SERVIDOR_PUBLICO,r.CAUSA_SANCION, r.ENTIDAD_EMISORA_RESOLUCION,
				r.FECHA_NOTIFICACION,r.FECHA__NOTIFICACION,r.DIAS_SANCION,r.FECHA_INICIO,r.FECHA__INICIO,
				r.FECHA_FIN,r.FECHA__FIN,r.ID_TIPO_SANCION,
				r.NombreDocumento,r.NombreArchivo,r.FoliosDocumento,r.Observacion, 
				r.TipoResolucionAbreviado,r.NumeroResolucion,r.FechaResolucion,r.TipoResolucion,
				r.FechaCrea, r.NombreUsuarioCrea, r.AmbitoUsuarioCrea, r.RolUsuarioCrea,
				r.FechaModifica, r.NombreUsuarioModifica, r.AmbitoUsuarioModifica, r.RolUsuarioModifica,
				r.DESCRIPCION_ESTADO_REGISTRO, r.CodigoEstadoRegistro, r.[Row], r.Total, r.Anular, 
				r.SolicitaAnular,
				r.AnulaModificatoria,r.CantidadAnios,
				CONVERT(BIT,CASE WHEN r.CodigoEstadoRegistro = 5 
						AND @CODIGO_ROL_ESC = 2 
						AND @HABILITA_MODIFICATORIA = 1 THEN 1 ELSE 0 END) AS Modifica
FROM (
		SELECT 	ROW_NUMBER() OVER (ORDER BY sj_sp.FECHA_RESOLUCION DESC) AS [Row],
							COUNT(1) OVER() AS Total,
							CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO = 5 
												AND @CODIGO_ROL_ESC = 2 
												AND ISNULL(sj_sp.PERMITE_ANULAR,0) = 1 THEN 1 Else 0 END) AS Anular,
							CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO IN (5,18) 
												AND @CODIGO_ROL_ESC = 2 
												AND ISNULL(sj_sp.PERMITE_ANULAR,0) = 0 
												AND M.ID_ASP_MODIFICACION IS NULL
												THEN 1 Else 0 END) AS SolicitaAnular,
							CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO IN (5)
											AND @CODIGO_ROL_ESC = 2
											AND @HABILITA_MODIFICATORIA = 1
											AND M.ID_ASP_MODIFICACION IS NOT NULL THEN 1 ELSE 0 END) AS AnulaModificatoria,

							sj_sp.ID_SANCION_POR_SERVIDOR_PUBLICO,        
							CASE ES_DESTITUCION
							WHEN NULL THEN
							 sj_sp.CAUSA_SANCION_POR_SERVIDOR_PUBLICO 
							WHEN 1 THEN
							 'Destitución: '+ sj_sp.CAUSA_SANCION_POR_SERVIDOR_PUBLICO 	
							WHEN 0 THEN
							 'Otro: ' + sj_sp.CAUSA_SANCION_POR_SERVIDOR_PUBLICO 	
							END as CAUSA_SANCION, 
							--sj_sp.CAUSA_SANCION_POR_SERVIDOR_PUBLICO as CAUSA_SANCION, 
							sj_sp.ENTIDAD_EMISORA_RESOLUCION,       
							sj_sp.FECHA_NOTIFICA_SANCION_SANCION_POR_SERVIDOR_PUBLICO as FECHA_NOTIFICACION,   
							FORMAT(sj_sp.FECHA_NOTIFICA_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') AS FECHA__NOTIFICACION,
							sj_sp.DIAS_SANCION_SANCION_POR_SERVIDOR_PUBLICO as DIAS_SANCION,  
							CASE ES_DESTITUCION
							WHEN NULL THEN
								sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO
							WHEN 1 THEN
								sj_sp.FECHA_INICIO_INHABILITACION
							WHEN 0 THEN
							 sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO
							END AS FECHA_INICIO,
							--sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO as FECHA_INICIO,  
							CASE ES_DESTITUCION
							WHEN NULL THEN
							 FORMAT(sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') 
							WHEN 1 THEN
							 FORMAT(sj_sp.FECHA_INICIO_INHABILITACION, 'dd/MM/yyyy')
							WHEN 0 THEN
							 FORMAT(sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') 
							END AS FECHA__INICIO,	
							--FORMAT(sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') AS FECHA__INICIO,
							CASE ES_DESTITUCION
							WHEN NULL THEN
								sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO
							WHEN 1 THEN
								sj_sp.FECHA_FIN_INHABILITACION
							WHEN 0 THEN
							 sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO
							END AS FECHA_FIN,
							--sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO as FECHA_FIN,   
							CASE ES_DESTITUCION
							WHEN NULL THEN
							 FORMAT(sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') 
							WHEN 1 THEN
							 FORMAT(sj_sp.FECHA_FIN_INHABILITACION, 'dd/MM/yyyy')
							WHEN 0 THEN
							 FORMAT(sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') 
							END AS FECHA__FIN,
							--FORMAT(sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') AS FECHA__FIN,
							sj_sp.ID_TIPO_SANCION as ID_TIPO_SANCION, 

							sj_sp.NOMBRE_DOCUMENTO NombreDocumento,
							sj_sp.NOMBRE_ARCHIVO NombreArchivo,
							sj_sp.FOLIOS_DOCUMENTO FoliosDocumento,
							sj_sp.OBSERVACION_SANCION_POR_SERVIDOR_PUBLICO Observacion,

							tr.ABREVIATURA AS TipoResolucionAbreviado,
							sj_sp.NUMERO_RESOLUCION as NumeroResolucion ,
							FORMAT(sj_sp.FECHA_RESOLUCION, 'dd/MM/yyyy') AS FechaResolucion,
							tr.NOMBRE_TIPO_RESOLUCION as TipoResolucion,

							ser.CODIGO_ESTADO_REGISTRO as CodigoEstadoRegistro,
							ser.DESCRIPCION_ESTADO_REGISTRO AS DESCRIPCION_ESTADO_REGISTRO, 
							FORMAT(sj_sp.FECHA_CREACION,'dd/MM/yyyy hh:mm tt') AS FechaCrea,
							ISNULL(sj_sp.NOMBRE_USUARIO_CREA,'') AS NombreUsuarioCrea,
							ISNULL(sj_sp.AMBITO_USUARIO_CREA,'') AS AmbitoUsuarioCrea,
							ISNULL(sj_sp.ROL_USUARIO_CREA,'') AS RolUsuarioCrea,
							FORMAT(sj_sp.FECHA_MODIFICACION,'dd/MM/yyyy hh:mm tt') AS FechaModifica,
							ISNULL(sj_sp.NOMBRE_USUARIO_MODIFICA,'') AS NombreUsuarioModifica,
							ISNULL(sj_sp.AMBITO_USUARIO_MODIFICA,'') AS AmbitoUsuarioModifica,
							ISNULL(sj_sp.ROL_USUARIO_MODIFICA,'') AS RolUsuarioModifica,
							sj_sp.CANTIDAD_ANIOS_INHABILITACION AS CantidadAnios
					FROM	transaccional.sancion_por_servidor_publico sj_sp                
							INNER JOIN sistema.estado_registro ser ON ser.ID_ESTADO_REGISTRO = sj_sp.ID_ESTADO_REGISTRO_JUDICIAL        
							INNER JOIN sistema.tipo_sancion tipo ON tipo.ID_TIPO_SANCION = sj_sp.ID_TIPO_SANCION        
							INNER JOIN sistema.etapa_sancion etapa ON etapa.ID_ETAPA_SANCION = tipo.ID_ETAPA_SANCION        
							INNER JOIN sistema.categoria_sancion categoria ON categoria.ID_CATEGORIA_SANCION = etapa.ID_CATEGORIA_SANCION 
							INNER JOIN sistema.tipo_resolucion tr on sj_sp.ID_TIPO_RESOLUCION = tr.ID_TIPO_RESOLUCION
							--LEFT JOIN transaccional.accion_por_servidor_publico_modificacion AS M WITH (NOLOCK)
							--	ON sj_sp.ID_SANCION_POR_SERVIDOR_PUBLICO = M.ID_ACCION_POR_SERVIDOR_PUBLICO
							--		AND M.ES_VIGENTE = 1 AND ID_ENTIDAD = 36
							LEFT JOIN 
							(	SELECT M.ID_ACCION_POR_SERVIDOR_PUBLICO, M.ID_ASP_MODIFICACION, M.ID_ENTIDAD
								FROM	transaccional.accion_por_servidor_publico_modificacion AS M WITH (NOLOCK)											
										INNER JOIN sistema.motivo_accion AS MMA WITH (NOLOCK)
											ON M.ID_MOTIVO_ACCION_MODIFICATORIA = MMA.ID_MOTIVO_ACCION
								WHERE	NOT MMA.CODIGO_ORIGEN IN (771,49) --EXCLUYENDO DEJAR SIN FECTO Y DECLARAR NULO
										AND M.ES_VIGENTE = 1
							) AS M ON sj_sp.ID_SANCION_POR_SERVIDOR_PUBLICO = M.ID_ACCION_POR_SERVIDOR_PUBLICO
					WHERE	sj_sp.ES_VIGENTE=1           
							AND sj_sp.ID_SERVIDOR_PUBLICO =  @IdServidorPublico AND categoria.CODIGO_CATEGORIA_SANCION = 1 --JUDICIAL  

					ORDER BY sj_sp.FECHA_RESOLUCION DESC
					OFFSET ((@pPaginaActual - 1) * @pTamanioPagina) ROWS
					FETCH NEXT @pTamanioPagina ROWS ONLY
				) r 
		 FOR XML AUTO ,TYPE)

	SELECT  @xml_pension AS 'SANCION'
    FOR     XML PATH('') ,ROOT('ROOT');

 END




USE [EducaRH_Desa]
GO
/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_SEL_SANCIONES_SANCION_ADMINISTRATIVA_OXML]    Script Date: 9/05/2022 16:49:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


--[dbo].[USP_ESCALAFON_SEL_SANCIONES_SANCION_ADMINISTRATIVA_OXML] 56474,1,10

ALTER PROCEDURE [dbo].[USP_ESCALAFON_SEL_SANCIONES_SANCION_ADMINISTRATIVA_OXML]
	@IdServidorPublico INT = 0,
	@CodigoRol VARCHAR(10),
	@pPaginaActual INT,
	@pTamanioPagina INT 
AS  
	BEGIN    
		
		SET NOCOUNT ON;
  		SET DATEFORMAT 'dmy'

	DECLARE @CODIGO_ROL_ESC INT,
			@HABILITA_MODIFICATORIA BIT = 0

	SELECT	@HABILITA_MODIFICATORIA = MS.ES_VIGENTE
	FROM	sistema.modificatoria_seccion AS MS WITH (NOLOCK)
	WHERE	MS.CODIGO_MODIFICATORIA_SECCION = 13 --SECCION 8 - SANCIONES ADMINISTRATIVAS

	SELECT	@CODIGO_ROL_ESC = R.CODIGO_ROL
	FROM	maestro.rol AS R WITH (NOLOCK)
	WHERE	R.CODIGO_ROL_PASSPORT = @CodigoRol

		DECLARE @xml_sancion_administrativa XML = 

		 (SELECT
						r.TipoSancion,r.DiasSancion, r.TipoMoneda,r.MontoMultaSancion,r.ID_SANCION_RELACIONADA,r.DESCRIPCION_SANCION_RELACIONADA,
						r.FechaNotificacionSancion,r.FechaIniSancion,r.FechaFinSancion,r.CODIGO_ESTADO_REGISTRO_SANCION,r.DESCRIPCION_ESTADO_REGISTRO_SANCION,
						r.FechaNotificacionSancionStr,r.FechaIniSancionStr,r.FechaFinSancionStr,r.FechaCeseSancionStr,
						r.RolCreacion,r.UsuarioCreacion,r.IdSancionPorServidorPublico,r.IdTipoSancion,r.Regimen,r.ENTIDAD_EMISORA_RESOLUCION,
						r.TipoResolucionAbreviado,r.NumeroResolucion,r.FechaResolucion,r.TipoResolucion,
						r.NombreDocumento,r.NombreArchivo,r.FoliosDocumento,r.Observacion, r.CantidadAnios,
						r.FechaCrea, r.NombreUsuarioCrea, r.AmbitoUsuarioCrea, r.RolUsuarioCrea,
						r.FechaModifica, r.NombreUsuarioModifica, r.AmbitoUsuarioModifica, r.RolUsuarioModifica,
						r.DESCRIPCION_ESTADO_REGISTRO, r.CodigoEstadoRegistro, r.[Row], r.Total, r.Anular, r.SolicitaAnular,
						CONVERT(BIT,CASE WHEN r.CodigoEstadoRegistro = 5 
										AND @CODIGO_ROL_ESC = 2 
										AND @HABILITA_MODIFICATORIA = 1 THEN 1 ELSE 0 END) AS Modifica,
						r.AnulaModificatoria
		 FROM ( 
		
				SELECT 	ROW_NUMBER() OVER (ORDER BY sj_sp.FECHA_RESOLUCION DESC) AS [Row],
									COUNT(1) OVER() AS Total,
									CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO = 5 
															AND @CODIGO_ROL_ESC = 2 
															AND ISNULL(sj_sp.PERMITE_ANULAR,0) = 1 THEN 1 Else 0 END) AS Anular,
									CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO IN (5,18) 
															AND @CODIGO_ROL_ESC = 2 
															AND ISNULL(sj_sp.PERMITE_ANULAR,0) = 0 
															AND M.ID_ASP_MODIFICACION IS NULL
															THEN 1 Else 0 END) AS SolicitaAnular,
									CONVERT(BIT,CASE WHEN ser.CODIGO_ESTADO_REGISTRO IN (5) 
															AND @CODIGO_ROL_ESC = 2
															AND @HABILITA_MODIFICATORIA = 1
															AND M.ID_ASP_MODIFICACION IS NOT NULL THEN 1 ELSE 0 END) AS AnulaModificatoria,
									 tipo.DESCRIPCION_TIPO_SANCION  as TipoSancion ,
									 sj_sp.DIAS_SANCION_SANCION_POR_SERVIDOR_PUBLICO DiasSancion,  
									 TM.DESCRIPCION_TIPO_MONEDA AS TipoMoneda,
									 sj_sp.MONTO_MULTA_SANCION_SANCION_POR_SERVIDOR_PUBLICO MontoMultaSancion,   
									 
									 sj_sp.FECHA_NOTIFICA_SANCION_SANCION_POR_SERVIDOR_PUBLICO FechaNotificacionSancion,    
									 FORMAT(sj_sp.FECHA_NOTIFICA_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') FechaNotificacionSancionStr,    
									 CASE sj_sp.ID_TIPO_SANCION
									 WHEN 5 THEN 
										sj_sp.FECHA_INICIO_INHABILITACION
									 ELSE
										sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO
									 END AS FechaIniSancion,
									 --sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO FechaIniSancion,    
									 CASE sj_sp.ID_TIPO_SANCION
									 WHEN 5 THEN 
										FORMAT(sj_sp.FECHA_INICIO_INHABILITACION, 'dd/MM/yyyy')
									 ELSE
										FORMAT(sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy')
									 END AS FechaIniSancionStr,
									 --FORMAT(sj_sp.FECHA_INI_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') FechaIniSancionStr, 
									 CASE sj_sp.ID_TIPO_SANCION
									 WHEN 5 THEN 
										sj_sp.FECHA_FIN_INHABILITACION
									 ELSE
										sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO
									 END AS FechaFinSancion,
									 --sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO FechaFinSancion,  
									 CASE sj_sp.ID_TIPO_SANCION
									 WHEN 5 THEN 
										FORMAT(sj_sp.FECHA_FIN_INHABILITACION, 'dd/MM/yyyy')
									 ELSE
										FORMAT(sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy')
									 END AS FechaFinSancionStr,
									 --FORMAT(sj_sp.FECHA_FIN_SANCION_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') FechaFinSancionStr, 
									 FORMAT(sj_sp.FECHA_CESE_SANCION_POR_SERVIDOR_PUBLICO, 'dd/MM/yyyy') FechaCeseSancionStr,

									 ser.CODIGO_ESTADO_REGISTRO AS CODIGO_ESTADO_REGISTRO_SANCION,    
									 ser.DESCRIPCION_ESTADO_REGISTRO AS DESCRIPCION_ESTADO_REGISTRO_SANCION,
									 sj_sp.ROL_CREACION AS RolCreacion,    
									 sj_sp.USUARIO_CREACION AS UsuarioCreacion,  
									 sj_sp.ID_SANCION_POR_SERVIDOR_PUBLICO IdSancionPorServidorPublico,    
									 sj_sp.ID_TIPO_SANCION as IdTipoSancion, 
									 reg.DESCRIPCION_REGIMEN as Regimen,
									 sj_sp.ENTIDAD_EMISORA_RESOLUCION,


									tr.ABREVIATURA AS TipoResolucionAbreviado,
									sj_sp.NUMERO_RESOLUCION as NumeroResolucion ,
									FORMAT(sj_sp.FECHA_RESOLUCION, 'dd/MM/yyyy') AS FechaResolucion,
									tr.NOMBRE_TIPO_RESOLUCION as TipoResolucion,
									ser.CODIGO_ESTADO_REGISTRO as CodigoEstadoRegistro,
									ser.DESCRIPCION_ESTADO_REGISTRO AS DESCRIPCION_ESTADO_REGISTRO, 
									sj_sp.ID_SANCION_RELACIONADA,
									sj_sp.DESCRIPCION_SANCION_RELACIONADA,

									sj_sp.NOMBRE_DOCUMENTO AS NombreDocumento,
									sj_sp.NOMBRE_ARCHIVO AS NombreArchivo,
									sj_sp.FOLIOS_DOCUMENTO AS FoliosDocumento,
									sj_sp.OBSERVACION_SANCION_POR_SERVIDOR_PUBLICO AS Observacion,

									FORMAT(sj_sp.FECHA_CREACION,'dd/MM/yyyy hh:mm tt') AS FechaCrea,
									ISNULL(sj_sp.NOMBRE_USUARIO_CREA,'') AS NombreUsuarioCrea,
									ISNULL(sj_sp.AMBITO_USUARIO_CREA,'') AS AmbitoUsuarioCrea,
									ISNULL(sj_sp.ROL_USUARIO_CREA,'') AS RolUsuarioCrea,
									FORMAT(sj_sp.FECHA_MODIFICACION,'dd/MM/yyyy hh:mm tt') AS FechaModifica,
									ISNULL(sj_sp.NOMBRE_USUARIO_MODIFICA,'') AS NombreUsuarioModifica,
									ISNULL(sj_sp.AMBITO_USUARIO_MODIFICA,'') AS AmbitoUsuarioModifica,
									ISNULL(sj_sp.ROL_USUARIO_MODIFICA,'') AS RolUsuarioModifica,
									sj_sp.CANTIDAD_ANIOS_INHABILITACION AS CantidadAnios
							FROM	transaccional.sancion_por_servidor_publico sj_sp                
									INNER JOIN sistema.estado_registro ser ON ser.ID_ESTADO_REGISTRO = sj_sp.ID_ESTADO_REGISTRO_SANCION        
									INNER JOIN sistema.tipo_sancion tipo ON tipo.ID_TIPO_SANCION = sj_sp.ID_TIPO_SANCION        
									INNER JOIN sistema.etapa_sancion etapa ON etapa.ID_ETAPA_SANCION = tipo.ID_ETAPA_SANCION        
									INNER JOIN sistema.categoria_sancion categoria ON categoria.ID_CATEGORIA_SANCION = etapa.ID_CATEGORIA_SANCION 
									left join sistema.tipo_resolucion tr on sj_sp.ID_TIPO_RESOLUCION = tr.ID_TIPO_RESOLUCION
									INNER JOIN sistema.regimen reg ON reg.ID_REGIMEN = sj_sp.ID_REGIMEN 									
									INNER JOIN sistema.tipo_moneda AS TM  WITH (NOLOCK) ON TM.ID_TIPO_MONEDA = sj_sp.ID_TIPO_MONEDA	
									LEFT JOIN transaccional.accion_por_servidor_publico_modificacion AS M WITH (NOLOCK)
									ON sj_sp.ID_SANCION_POR_SERVIDOR_PUBLICO = M.ID_ACCION_POR_SERVIDOR_PUBLICO
										AND M.ES_VIGENTE = 1 AND ID_ENTIDAD = 37 --sancion_administrativa_por_servidor_publico
							WHERE	sj_sp.ES_VIGENTE=1           
									AND tipo.ID_TIPO_SANCION not in  (1,8,9)--ADMINISTRATIVA    --SE AGREGO EL 1 APELACIONES 
									AND sj_sp.ID_SERVIDOR_PUBLICO = @IdServidorPublico
					
							ORDER BY sj_sp.FECHA_RESOLUCION DESC
							OFFSET ((@pPaginaActual - 1) * @pTamanioPagina) ROWS
							FETCH NEXT @pTamanioPagina ROWS ONLY
						) r 
				 FOR XML AUTO ,TYPE)

	SELECT  @xml_sancion_administrativa AS 'SANCION'
    FOR     XML PATH('') ,ROOT('ROOT');

 END
							




USE [EducaRH_Desa]
GO
/****** Object:  StoredProcedure [dbo].[USP_ESCALAFON_ACT_DATOS_SANCION_JUDICIAL_ANULA_IOXML]    Script Date: 20/05/2022 11:57:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*
EXEC [dbo].[USP_ESCALAFON_ACT_DATOS_SANCION_JUDICIAL_ANULA_IOXML]  
'
<root>
  <sancion ID_SANCION_POR_SERVIDOR_PUBLICO="74752" />
  <usuario>
    <row USUARIO="42813053" NOMBRE_USUARIO="CHRISTIAN ABELARDO SCHEREIBER CORNELIO" ID_CENTRO_AMBITO="173" ID_TIPO_CENTRO_AMBITO="2" AMBITO="UGEL San Juan de Lurigancho 05" CODIGO_ROL="GSD002" ROL="Técnico de escalafón " FECHA_CREACION="20/05/2022 02:30 p. m." ID_USUARIO="80874" />
  </usuario>
</root>
'
*/

ALTER PROCEDURE [dbo].[USP_ESCALAFON_ACT_DATOS_SANCION_JUDICIAL_ANULA_IOXML]  
	@REQUEST_XML XML
AS  
BEGIN  
  
	DECLARE	@ID_PERSONA INT = 0,  
			@ID_SERVIDOR_PUBLICO INT = 0,  
     		@NOMBRE_USUARIO VARCHAR(50),
			@AMBITO VARCHAR(100),
			@ROL VARCHAR(100),
			@USUARIO VARCHAR(12),
			@FECHA_ACTUAL DATETIME = GETDATE(),  
			@ID_ESTADO_ANULADO INT,  
			@ID_SANCION_POR_SERVIDOR_PUBLICO INT,

			@CODIGO_TIPO_SANCION INT,
			@FECHA_INICIO DATE,
			@FECHA_FINAL DATE,
			@ES_VIGENTE BIT = 1,
			@FECHA_FUTURO DATE,
			@FECHA_HOY DATE = GETDATE(),
			@ID_SITUACION_LABORAL_NUEVA INT,
			@SITUACION_LABORAL_NUEVA VARCHAR(100),
			@ID_CONDICION_LABORAL_NUEVA INT,
			@CONDICION_LABORAL_NUEVA VARCHAR(100),

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

		SELECT @ID_ESTADO_ANULADO = ID_ESTADO_REGISTRO  
		FROM sistema.estado_registro AS ER WITH (NOLOCK)  
		WHERE ER.CODIGO_ESTADO_REGISTRO = 6 --ANULADO  
  
		SELECT	@ID_SANCION_POR_SERVIDOR_PUBLICO = c.value('@ID_SANCION_POR_SERVIDOR_PUBLICO', 'INT')
		FROM	@REQUEST_XML.nodes('/root/sancion') x(c);

		SET @FECHA_FUTURO = DATEADD(YEAR,100,@FECHA_ACTUAL)

		SELECT	@CODIGO_TIPO_SANCION = TS.CODIGO_TIPO_SANCION,
				@FECHA_INICIO = SSP.FECHA_INICIO_INHABILITACION,
				@FECHA_FINAL = SSP.FECHA_FIN_INHABILITACION,
				@ID_SERVIDOR_PUBLICO = SSP.ID_SERVIDOR_PUBLICO
		FROM	transaccional.sancion_por_servidor_publico AS SSP WITH (NOLOCK)
				INNER JOIN sistema.tipo_sancion AS TS WITH (NOLOCK)
					ON SSP.ID_TIPO_SANCION = TS.ID_TIPO_SANCION
		WHERE	ID_SANCION_POR_SERVIDOR_PUBLICO = @ID_SANCION_POR_SERVIDOR_PUBLICO

		IF @CODIGO_TIPO_SANCION IN (9) --SANCION JUDICIAL
			BEGIN		
				IF @FECHA_INICIO > @FECHA_ACTUAL --FUTURO
					SET @ES_VIGENTE = 0

				IF ISNULL(@FECHA_FINAL, @FECHA_FUTURO) < @FECHA_ACTUAL --PASADO
					SET @ES_VIGENTE = 0
				IF @ES_VIGENTE = 1
					BEGIN --SI NO EXISTE ACCION VIGENTE CON FECHA DE INICIO MAYOR Y QUE HAYA AFECTADO LA SITUACION LABORAL
						IF NOT EXISTS(
									SELECT	ASP.ID_ACCION_POR_SERVIDOR_PUBLICO
									FROM	transaccional.accion_por_servidor_publico AS ASP WITH (NOLOCK)
											INNER JOIN sistema.estado_registro AS ER WITH (NOLOCK)
												ON ASP.ID_ESTADO_REGISTRO = ER.ID_ESTADO_REGISTRO
											INNER JOIN sistema.situacion_laboral_motivo_accion AS SLMA WITH (NOLOCK)
												ON ASP.ID_MOTIVO_ACCION = SLMA.ID_MOTIVO_ACCION
									WHERE	ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
											AND SLMA.AFECTA_SITUACION_LABORAL = 1
											AND ER.CODIGO_ESTADO_REGISTRO IN (5,13,20,23) --APROBADO,EN TRASLADO,APROBADO CON SOLICITUD DE ANULACIÓN,LIMITADO
											AND CONVERT(DATE,ASP.FECHA_INICIO_ACCION_POR_SERVIDOR_PUBLICO) >= @FECHA_INICIO
											AND @FECHA_HOY BETWEEN 
												CONVERT(DATE,ASP.FECHA_INICIO_ACCION_POR_SERVIDOR_PUBLICO) AND 
												ISNULL(CONVERT(DATE,ASP.FECHA_FIN_ACCION_POR_SERVIDOR_PUBLICO), @FECHA_FUTURO)
								)
							BEGIN
								--OBTENIENDO LA SITUACION LABORAL DE LA ULTIMA ACCION PERMANENTE
								SELECT TOP 1 
										@ID_SITUACION_LABORAL_NUEVA = SL.ID_SITUACION_LABORAL,
										@SITUACION_LABORAL_NUEVA = SL.DESCRIPCION_SITUACION_LABORAL,
										@ID_CONDICION_LABORAL_NUEVA = CL.ID_CONDICION_LABORAL,
										@CONDICION_LABORAL_NUEVA = CL.DESCRIPCION_CONDICION_LABORAL
								FROM	transaccional.accion_por_servidor_publico AS ASP WITH (NOLOCK)
										INNER JOIN sistema.estado_registro AS ER WITH (NOLOCK)
											ON ASP.ID_ESTADO_REGISTRO = ER.ID_ESTADO_REGISTRO
										INNER JOIN sistema.situacion_laboral_motivo_accion AS SLMA WITH (NOLOCK)
											ON ASP.ID_MOTIVO_ACCION = SLMA.ID_MOTIVO_ACCION
										INNER JOIN sistema.situacion_laboral AS SL WITH (NOLOCK)
											ON SLMA.ID_SITUACION_LABORAL = SL.ID_SITUACION_LABORAL
										INNER JOIN sistema.condicion_laboral AS CL WITH (NOLOCK)
											ON SL.ID_CONDICION_LABORAL = CL.ID_CONDICION_LABORAL
								WHERE	ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
										AND SLMA.AFECTA_SITUACION_LABORAL = 1
										AND ER.CODIGO_ESTADO_REGISTRO IN (5,13,20,23) --APROBADO,EN TRASLADO,APROBADO CON SOLICITUD DE ANULACIÓN,LIMITADO
										AND @FECHA_HOY BETWEEN 
											CONVERT(DATE,ASP.FECHA_INICIO_ACCION_POR_SERVIDOR_PUBLICO) AND 
											ISNULL(CONVERT(DATE,ASP.FECHA_FIN_ACCION_POR_SERVIDOR_PUBLICO), @FECHA_FUTURO)
								ORDER BY ASP.FECHA_INICIO_ACCION_POR_SERVIDOR_PUBLICO DESC

								UPDATE	transaccional.accion_por_servidor_publico_registro
								SET		ID_SITUACION_LABORAL = @ID_SITUACION_LABORAL_NUEVA,
										SITUACION_LABORAL = @SITUACION_LABORAL_NUEVA,
										ID_CONDICION_LABORAL = @ID_CONDICION_LABORAL_NUEVA,
										CONDICION_LABORAL = @CONDICION_LABORAL_NUEVA
								WHERE	ID_SERVIDOR_PUBLICO = @ID_SERVIDOR_PUBLICO
							END
					END
			END

		UPDATE	DP  
		SET		DP.ID_ESTADO_REGISTRO_JUDICIAL = @ID_ESTADO_ANULADO,  
				FECHA_MODIFICACION = @FECHA_ACTUAL,  
				USUARIO_MODIFICACION = @USUARIO,
				NOMBRE_USUARIO_MODIFICA = @NOMBRE_USUARIO,
				AMBITO_USUARIO_MODIFICA = @AMBITO,
				ROL_USUARIO_MODIFICA = @ROL
		FROM transaccional.sancion_por_servidor_publico AS DP WITH (NOLOCK)  
		WHERE DP.ID_SANCION_POR_SERVIDOR_PUBLICO = @ID_SANCION_POR_SERVIDOR_PUBLICO
  
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
			([NOMBRE_SP], [INPUT_PARAMS_STR], [MENSAJE_BD], [MENSAJE_USUARIO], [FECHA_CREACION], [USUARIO_CREACION])  
			VALUES  
			('USP_ESCALAFON_ACT_DATOS_SANCION_JUDICIAL_ANULA_IOXML', CONVERT(VARCHAR,@ID_SANCION_POR_SERVIDOR_PUBLICO), @MessageReal, @Message, @FECHA_ACTUAL, @USUARIO)  
		END  
  
	SELECT	@Success AS Success,
			@Message AS [Message],
			@ID_SANCION_POR_SERVIDOR_PUBLICO AS [Value]   
  
  
END
