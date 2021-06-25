*          DATA SET DDTRNSQL3  AT LEVEL 126 AS OF 05/01/02                      
*PHASE TRNSQL1                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'TEST SQL IN A SIMPLE ASSEMBLER PROGRAM'                         
TRNSQL1  CSECT                                                                  
         PRINT GEN                                                              
***********************************************************************         
*THIS MACRO ALLOWS EXEC SQL STATEMENTS TO BE BYPASSED BY THE ASSEMBLER*         
*THE DB2 PRE-COMPLILER WILL REPLACE EXEC SQL STATEMENTS               *         
***********************************************************************         
*        MACRO                                                                  
*LABEL   EXEC                                                                   
*LABEL   DS    0H                                                               
*        MEND                                                                   
                                                                                
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY DSNHLI                                                           
         NBASE WRKX-WRKD,*TRNSQL1,REGSAVE,R9,R8,CLEAR=YES                       
         USING WRKD,RC                                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(20),=CL20'TEST SQL REQUESTS'                               
         BAS   RE,GENINIT                                                       
*                                                                               
         MVC   P(80),=CL80'BEFORE VALCARDS'                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,VALCARDS                                                      
         BNE   EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LOAD CAF LANGAUGE INTERFACE ENTRY ADDRESSES                        *          
**********************************************************************          
LOADCAF  DS    0H                                                               
*                                                                               
         MVC   P(80),=CL80'BEFORE LOADCAF'                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LOAD  EP=DSNALI           LAOD CAF SERVICE REQUEST EP                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LIALI            SAVE THIS FOR CAF SERVICE REQUEST            
         LOAD  EP=DSNHLI2          LOAD THE CAL SQL CALL ENTRY POINT            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LISQL            SAVE THIS FOR SQL CALLS                      
*                                                                               
         LOAD  EP=DSNTIAR          LAOD DSNTIAR                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LITIAR                                                        
*                                                                               
**********************************************************************          
* CONNECT TO DB2                                                     *          
**********************************************************************          
CONNREQ  DS    0H                                                               
*                                                                               
         MVC   P(80),=CL80'BEFORE CONNECT'                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     RF,LIALI            GET LANGAUGE INTERFACE ADDRESS               
         MVC   FUNCTN,CONNECT      GET FUNCTION TO CALL                         
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(FUNCTN,SSID,TECB,SECB,RIBPTR),VL,MF=(E,CAFCALL)            
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(80),=CL80'AFTER CONNECT'                                       
         GOTO1 =V(PRINTER)                                                      
         SPACE 1                                                                
**********************************************************************          
* OPEN DB2                                                           *          
**********************************************************************          
OPENREQ  DS    0H                                                               
*                                                                               
         MVC   P(80),=CL80'BEFORE OPEN'                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     RF,LIALI            GET LANGAUGE INTERFACE ADDRESS               
         MVC   FUNCTN,OPEN         GET FUNCTION TO CALL                         
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(FUNCTN,SSID,PLAN),VL,MF=(E,CAFCALL)                        
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(80),=CL80'AFTER OPEN'                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,CLIDATA          R2=A(MEDIA CLIENT EXTRACT RECORD)            
         USING MXCLID,R2                                                        
         LR    R0,R2                                                            
         LA    R1,MXCLIDL                                                       
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
LOOP     DS    0H                                                               
         MVC   P(20),C                                                          
         CLC   C(5),=C'CODE='                                                   
         BE    SINGLE                                                           
         CLC   C(5),=C'NAME='                                                   
         BE    MULTI                                                            
         CLC   C(5),=C'REPS='                                                   
         BE    REPSEL                                                           
         CLC   C(5),=C'DSQL='                                                   
         BE    DYNSQL                                                           
LOOPNEXT DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CLOSEREQ                                                         
         B     LOOP                                                             
         EJECT                                                                  
***********************************************************************         
*SQL DESCRIPTOR AREAS - MUST APPEAR BEFORE THEY ARE REFERENCED        *         
***********************************************************************         
                                                                                
         DS    0D                                                               
SQDDA1   EXEC  SQL INCLUDE SQLDA                                                
         EJECT                                                                  
***********************************************************************         
*SQL SINGLETON - GETS COLS FROM ONE ROW INTO LOCAL STORAGE            *         
***********************************************************************         
                                                                                
SINGLE   MVC   MXCLICDE,C+5        SINGLE CLIENT SELECT VIA CLI CODE            
         MVC   MXCLIAGY,=CL2'*N'                                                
         MVC   MXCLISHR,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
*                                                                               
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL                                                     X        
               SELECT ShortName,Name                                   X        
               INTO   :MXCLISHR,:MXCLINAM                              X        
               FROM   HP4DDDST.CLIENT                                  X        
               WHERE  ClientCode=:MXCLICDE                             X        
               AND    AgencyCode=:MXCLIAGY                             X        
               AND    MediaCode=0                                               
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,ZERO        CHECK SQL ERROR CODE                         
         BNE   SINGLERR                                                         
*                                                                               
         MVC   P+30(8),RCOUT       PRINT OUT RETURN DATA                        
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SINGLEX  EQU   *                                                                
         B     LOOPNEXT                                                         
*                                                                               
SINGLERR EQU   *                                                                
         MVC   P(30),=CL30'SQL ERROR CODE:'                                     
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
         EJECT                                                                  
***********************************************************************         
*SQL BROWSE - REQUIRES A CURSOR TO GET MULTIPLE ROWS                  *         
***********************************************************************         
*                                                                               
MULTI    MVC   MXCLISHR,C+5        MULTI CLIENT SELECT VIA CLI NAME             
         MVC   MXCLIAGY,=CL2'*N'                                                
         MVC   MXCLICDE,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
         MVC   APWORK,MXCLISHR                                                  
         LA    RE,APWORK                                                        
         LA    R0,L'MXCLISHR                                                    
MULTI1   CLI   0(RE),C' '                                                       
         BE    MULTI1A                                                          
         LA    RE,1(RE)                                                         
         BCT   R0,MULTI1                                                        
*        B     *+8                                                              
MULTI1A  EQU   *                                                                
*         MVI   0(RE),C'%'          SET SQL LIKE CHR AT END OF STRING           
         MVC   MXCLISHR,APWORK                                                  
*                                                                               
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL    DECLARE CLICUR CURSOR FOR                        X        
               SELECT ClientCode,ShortName,Name                        X        
               FROM   HP4DDDST.CLIENT                                           
* ??          FROM   HP4DDDST.CLIENT                                  X         
* ??          WHERE  ShortName=:MXCLISHR                              X         
* ??          AND    AgencyCode=:MXCLIAGY                             X         
* ??          AND    MediaCode=0                                                
*                                                                               
         EXEC  SQL OPEN CLICUR                                                  
*                                                                               
MULTI2   EXEC  SQL    FETCH CLICUR                                     X        
               INTO   :MXCLICDE,:MXCLISHR,:MXCLINAM                             
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,=F'100'     TEST NO MORE DATA                            
         BE    MULTIX                                                           
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BNE   MULTIERR                                                         
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         B     MULTI2                                                           
*                                                                               
MULTIX   EXEC  SQL CLOSE CLICUR                                                 
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
*                                                                               
MULTIERR EQU   *                                                                
         MVC   P(30),=CL30'SQL ERROR CODE:'                                     
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
         EJECT                                                                  
***********************************************************************         
*SQL BROWSE FOR REP - REQUIRES A CURSOR TO GET MULTIPLE ROWS          *         
***********************************************************************         
*                                                                               
REPSEL   EQU   *                                                                
*                                                                               
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL    DECLARE REPCUR CURSOR FOR                        X        
               SELECT *                                                X        
               FROM   REPNTSVR.JDONDDNY.REP                                     
*                                                                               
         EXEC  SQL OPEN REPCUR                                                  
*                                                                               
REPS2    EXEC  SQL    FETCH REPCUR                                     X        
               INTO   :HVSBRCDE, :HVREPNAM                                      
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,=F'100'     TEST NO MORE DATA                            
         BE    REPSX                                                            
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BNE   REPSERR                                                          
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         MVC   P+40(L'HVSBRCDE),HVSBRCDE                                        
         MVC   P+50(L'HVREPNAM),HVREPNAM                                        
         GOTO1 =V(PRINTER)                                                      
         B     REPS2                                                            
*                                                                               
REPSX    EXEC  SQL CLOSE REPCUR                                                 
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
*                                                                               
REPSERR EQU    *                                                                
         MVC   P(30),=CL30'SQL ERROR CODE:'                                     
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
*                                                                               
         DS    0D                                                               
STMT1    DS    HL2,CL1000                                                       
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* DYNAMIC SQL REQUEST                                                 *         
***********************************************************************         
                                                                                
DYNSQL   EQU   *                                                                
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         LA    R4,WSSQLD                                                        
         USING SQLDA,R4                                                         
         MVC   SQLDAID,=CL8'SQLDA'                                              
         MVC   SQLDABC,=AL4(L'WSSQLD)                                           
         LH    RF,NUMSQLV                                                       
         STH   RF,SQLN                                                          
         MVC   SQLD,=H'0'                                                       
         LA    R1,SQLVAR                                                        
         USING SQLVARN,R1                                                       
*                                                                               
DYNM010  EQU   *                                                                
         LTR   RF,RF                                                            
         BZ    DYNM020                                                          
         MVC   SQLTYPE,=H'0'                                                    
         MVC   SQLLEN,=H'0'                                                     
         MVC   SQLNAME,SPACES                                                   
         BCT   RF,DYNM010                                                       
         DROP  R1                                                               
*                                                                               
DYNM020  EQU   *                                                                
*                                                                               
         EXEC  SQL DESCRIBE TABLE :TABNAME INTO :WSSQLD                         
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
         MVC   STMT1,=H'132'                                                    
         MVC   STMT1+2(132),=CL132'SELECT * FROM HP4DDDST.CLIENT'               
*                                                                               
         EXEC  SQL DECLARE C1 CURSOR FOR S1                                     
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         EXEC  SQL PREPARE S1 FROM :STMT1                                       
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         EXEC  SQL DESCRIBE S1 INTO :WSSQLD                                     
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         MVC   P+30(8),RCOUT                                                    
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         B     LOOPNEXT                                                         
         EJECT                                                                  
OUTCODE  MVI   RCOUT,C' '                                                       
         L     RF,SQLCODE                                                       
         CVD   RF,DUB                                                           
         UNPK  RCOUT+1(7),DUB                                                   
         OI    RCOUT+7,C'0'                                                     
         LTR   RF,RF                                                            
         BNMR  RE                                                               
         MVI   RCOUT,C'-'                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* CLOSE DB2                                                          *          
**********************************************************************          
CLOSEREQ DS    0H                                                               
         L     RF,LIALI            GET LANGAUGE INTERFACE ADDRESS               
         MVC   FUNCTN,CLOSE        GET FUNCTION TO CALL                         
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(FUNCTN,TRMOP),VL,MF=(E,CAFCALL)                            
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BE    *+6                                                              
         DC    H'0'                                                             
**********************************************************************          
* DISCONNECT FROM DB2                                                *          
**********************************************************************          
DISCREQ  DS    0H                                                               
         L     RF,LIALI            GET LANGAUGE INTERFACE ADDRESS               
         MVC   FUNCTN,DISCON       GET FUNCTION TO CALL                         
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(FUNCTN),VL,MF=(E,CAFCALL)                                  
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELCAF                                                           
**********************************************************************          
* TRANSLATE                                                          *          
**********************************************************************          
XLATEREQ DS    0H                                                               
         LA    R5,SQLCA            SQL COMMUNICATION AREA                       
*??      MVC   CALLPA(CAFLEN),CAFCALL                                           
*                                                                               
**********************************************************************          
* DELETE CAF LANGAUGE INTERFACE LOAD MODULES                         *          
**********************************************************************          
DELCAF   DS    0H                                                               
         DELETE EP=DSNALI          CORRECTLY MAINTAIN USE COUNT                 
         DELETE EP=DSNHLI2         CORRECTLY MAINTAIN USE COUNT                 
*                                                                               
EXIT     DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK CAF RETURN CODES                                             *          
**********************************************************************          
CHEKCODE NTR1                                                                   
         MVC   CONTROL,CONTINUE                                                 
         ST    RF,RETCODE         SAVE THE RETURN CODE                          
         ST    R0,REASCODE                                                      
*        ********************* HUNT FOR FORCE OR ABTERM ***********             
         TM    TECB,X'40'         SEE IF TECB WAS POSTED (POSTBIT??)            
         BZ    DOCHECKS           BRANCH IF TECB WAS NOT POSTED                 
         CLC   TECBCODE(3),QUIESCE   IS THIS "STOP DB2 MODE=FORCE"              
         BE    DOCHECKS           IF NOT QUIESCE, WAS FORCE OR ABTE             
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
         MVC   P(80),=CL80'FOUND FORCE OR ABTERM, SHUTTING DOWN'                
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           GO TO THE END OF CHEKCODE                     
DOCHECKS DS    0H                 EXAMINE RETCODE AND REASCODE                  
*        ********************* HUNT FOR 0 *************************             
         CLC   RETCODE,ZERO       WAS IT A ZERO?                                
         BE    ENDCCODE           NOTHING TO DO IN CHEKCODE FOR ZER             
*        ********************* HUNT FOR 4 *************************             
         CLC   RETCODE,FOUR       WAS IT A 4?                                   
         BNE   HUNT8              IF NOT A 4, HUNT EIGHTS                       
         CLC   REASCODE,C10823    WAS IT A RELEASE LEVEL MISMATCH?              
         BNE   HUNT824            BRANCH IF NOT AN 823                          
         MVC   P(80),=CL80'MISMATCH BETWEEN DB2/CAF RELEASE LEVELS'             
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
HUNT824  DS    0H                 NOW LOOK FOR 'CAF RESET' REASON C             
         CLC   REASCODE,C10824    WAS IT 4? ARE WE READY TO RESTART             
         BNE   UNRECOG            IF NOT 824, GOT UNKNOWN CODE                  
         MVC   P(80),=CL80'CAF IS NOW READY FOR MORE INPUT'                     
         GOTO1 =V(PRINTER)                                                      
         MVC   CONTROL,RESTART    INDICATE THAT WE SHOULD RE-CONNEC             
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
UNRECOG  DS    0H                                                               
         MVC   P(80),=CL80'GOT RC=4 AND AN UNRECOGNIZED REASON CODE'            
         GOTO1 =V(PRINTER)                                                      
         MVC   CONTROL,SHUTDOWN   SHUTDOWN, SERIOUS PROBLEM                     
         B     ENDCCODE           WE ARE DONE. GO TO END OF CH                  
*        ********************* HUNT FOR 8 ********************                  
HUNT8    DS    0H                                                               
         CLC   RETCODE,EIGHT      HUNT RETURN CODE OF 8                         
         BE    GOT8OR12                                                         
         CLC   RETCODE,TWELVE     HUNT RETURN CODE OF 12                        
         BNE   HUNT200                                                          
GOT8OR12 DS    0H                 FOUND RETURN CODE OF 8 OR 12                  
         MVC   P(80),=CL80'FOUND RETCODE OF 8 OR 12'                            
         GOTO1 =V(PRINTER)                                                      
         CLC   REASCODE,F30002    HUNT FOR X'00F30002'                          
         BE    DB2DOWN                                                          
         CLC   REASCODE,F30012    HUNT FOR X'00F30012'                          
         BE    DB2DOWN                                                          
         MVC   P(80),=CL80'DB2 CONNECT FAIL WITH AN UNRECOG. REASON'            
         GOTO1 =V(PRINTER)                                                      
         CLC   SQLCODE,ZERO       SEE IF WE NEED TRANSLATE                      
         BNE   A4TRANS            IF NOT BLANK, SKIP TRANSLATE                  
*        ********************* TRANSLATE UNRECOGNIZED RETCODES ****             
         MVC   P(80),=CL80'SQLCODE 0 BUT RF NOT, SO TRANS. FOR SQLCODE'         
         GOTO1 =V(PRINTER)                                                      
         L     RF,LIALI           GET THE LANGUAGE INTERFACE ADDRES             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(TRANSLAT,SQLCA),VL,MF=(E,CAFCALL)                          
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         C     R0,C10205          DID THE TRANSLATE WORK?                       
         BNE   A4TRANS            IF NOT C10205, SQLERRM NOW FILLED             
         MVC   P(80),=CL80'NOT ABLE TO TRANSLT THE CONNECTION FAILURE'          
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           GO TO END OF CHEKCODE                         
A4TRANS  DS    0H                 SQLERRM MUST BE FILLED IN TO GET              
*        NOTE: YOUR CODE SHOULD PROBABLY REMOVE THE X'FF'                       
*        SEPARATORS AND FORMAT THE SQLERRM FEEDBACK AREA.                       
*        ALTERNATIVELY, USE DB2 SAMPLE APPLICATION DSNTIAR                      
*        TO FORMAT A MESSAGE.               WRITE 'SQLERRM IS:' SQL             
         MVC   P(80),=CL80'SQLERRM IS: ????'                                    
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
DB2DOWN  DS    0H                 HUNT RETURN CODE OF 200                       
         MVC   P(80),=CL80'DB2 IS DOWN - WILL TELL WHEN IT COMES UP'            
         GOTO1 =V(PRINTER)                                                      
         WAIT  ECB=SECB           WAIT FOR DB2 TO COME UP                       
         MVC   P(80),=CL80'DB2 IS NOW AVAILABLE'                                
         GOTO1 =V(PRINTER)                                                      
         MVC   CONTROL,RESTART    INDICATE THAT WE SHOULD RE-CONNEC             
         B     ENDCCODE                                                         
*        ********************* HUNT FOR 200 ***********************             
HUNT200  DS    0H                 HUNT RETURN CODE OF 200                       
         CLC   RETCODE,NUM200     HUNT 200               BNE   HUNT             
         MVC   P(80),=CL80'CAF FOUND USER ERROR, SEE DSNTRACE DATA SET'         
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
*        ********************* HUNT FOR 204 ***********************             
HUNT204  DS    0H                 HUNT RETURN CODE OF 204                       
         CLC   RETCODE,NUM204     HUNT 204                                      
         BNE   WASSAT             IF NOT 204, GOT STRANGE CODE                  
         MVC   P(80),=CL80'CAF SYSTEM ERROR, SEE DSNTRACE DATA SET'             
         GOTO1 =V(PRINTER)                                                      
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
*        ********************* UNRECOGNIZED RETCODE ***************             
WASSAT   DS    0H                                                               
         MVC   P(80),=CL80'GOT AN UN RECOGNISED RETCODE'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
         BE    ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
ENDCCODE DS    0H                 SHOULD WE SHUT DOWN?                          
         L     R4,RETCODE         GET A COPY OF THE RETCODE                     
         C     R4,FOUR            HAVE A LOOK AT THE RETCODE                    
         BNH   BYEBYE             IF RETCODE <= 4 THEN LEAVE CHEKCO             
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
BYEBYE   DS    0H                 WRAP UP AND LEAVE CHEKCODE                    
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
         MVC   SSID,SPACES                                                      
         MVC   PLAN,SPACES                                                      
         MVC   TRMOP,SPACES                                                     
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'/*',C            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
         CLC   =C'CODE=',C         OR FIRST INPUT PARAMETER                     
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
         CLC   =C'NAME=',C                                                      
         BE    VCEND                                                            
         CLC   =C'DSQL=',C                                                      
         BE    VCEND                                                            
         CLC   =C'REPS=',C                                                      
         BE    VCEND                                                            
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   C(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VSSID                                                            
         B     VPLAN                                                            
         B     VTRMOP                                                           
*                                  CARD DATA ERROR CONDITIONS                   
VCEND    B     VCYES                                                            
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 =V(PRINTER)         INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCYES    B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
         SPACE 1                                                                
VSSID    EQU   *                   SSID=                                        
         MVC   SSID,C+5                                                         
         B     VCLP1                                                            
*                                                                               
VPLAN    EQU   *                   PLAN=                                        
         MVC   PLAN,C+5                                                         
         B     VCLP1                                                            
*                                                                               
VTRMOP   EQU   *                   TRMOP=                                       
         MVC   TRMOP,C+6                                                        
         B     VCLP1                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1                                                                   
         LA    RE,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRMSG0),0(RE)                                            
         GOTO1 =V(PRINTER)                                                      
         MVI   ERROR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE DSNHLI INTERCEPTS CALLS TO LI EP=DSNHLI                            
*******************************************************************             
         DS    0D                                                               
*?? DSNHLI   CSECT                                                              
DSNHLI   DS    0H                                                               
         STM   RE,RC,12(RD)       PROLOGUE                                      
         LA    RF,SAVEHLI         GET SAVE AREA ADDRESS                         
         ST    RD,4(,RF)          CHAIN THE SAVE AREAS                          
         ST    RF,8(,RD)          CHAIN THE SAVE AREAS                          
         LR    RD,RF              PUT SAVE AREA ADDRESS IN RD                   
         L     RF,LISQL           GET THE ADDRESS OF REAL DSNHLI                
         BASSM RE,RF              BRANCH TO DSNALI TO DO AN SQL CALL            
*                                 DSNALI IS IN 31-BIT MODE, SO USE              
*                                 BASSM TO ASSURE THAT THE ADDRESSING           
*                                 MODE IS PRESERVED.                            
         L     RD,4(,RD)          RESTORE RD (CALLER'S SAVE AREA)               
         L     RE,12(,RD)         RESTORE RE (RETURN ADDRESS)                   
         RETURN (1,12)            RESTORE R1-12, NOT R0 AND RF                  
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE CALLS DSNTIAR                                                      
*******************************************************************             
         DS    0D                                                               
CALLTIAR DS    0H                                                               
         STM   RE,RC,12(RD)       PROLOGUE                                      
         LA    RF,SAVETIAR        GET SAVE AREA ADDRESS                         
         ST    RD,4(,RF)          CHAIN THE SAVE AREAS                          
         ST    RF,8(,RD)          CHAIN THE SAVE AREAS                          
         LR    RD,RF              PUT SAVE AREA ADDRESS IN RD                   
         L     RF,LITIAR          GET THE ADDRESS OF DSNTIAR                    
         XC    PARM,PARM                                                        
         LA    R1,PARM                                                          
         LA    RE,SQLCA                                                         
         ST    RE,0(R1)                                                         
         LA    RE,MESSAGE                                                       
         ST    RE,4(R1)                                                         
         LA    RE,LRECL                                                         
         ST    RE,8(R1)                                                         
         OI    8(R1),X'80'                                                      
*                                                                               
         BASSM RE,RF              BRANCH TO DSNTIAR TO DO AN SQL CALL           
*                                 DSNTIARIN 31-BIT MODE, SO USE                 
*                                 BASSM TO ASSURE THAT THE ADDRESSING           
*                                 MODE IS PRESERVED.                            
         L     RD,4(,RD)          RESTORE RD (CALLER'S SAVE AREA)               
         L     RE,12(,RD)         RESTORE RE (RETURN ADDRESS)                   
         RETURN (1,12)            RESTORE R1-12, NOT R0 AND RF                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* PROGRAM CONSTANTS DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
TABNAME  DC    CL30'HP4DDDST.CLIENT'                                            
*                                                                               
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(05,01),X'00',CL11'SSID='                                     
         DC    AL1(05,02),X'00',CL11'PLAN='                                     
         DC    AL1(06,03),X'00',CL11'TRMOP='                                    
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
         SPACE 2                                                                
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'INVALID INPUT FILE LINE'                                    
         SPACE 1                                                                
*                                                                               
SSB      DC    F'0',F'0'                                                        
UTL      DC    F'0',AL1(04),XL3'00'                                             
*                                                                               
NUMSQLV  DC    H'100'                                                           
*                                                                               
C        DC    CL80' '                                                          
*                                                                               
SHUTDOWN DC    CL8'SHUTDOWN'       CONTROL VALUE: SHUTDOWN EXECUTION            
RESTART  DC    CL8'RESTART '       CONTROL VALUE: RESTART EXECUTION             
CONTINUE DC    CL8'CONTINUE'       CONTROL VALUE: EVERYTHING OK, CONT.          
CODE0    DC    F'0'                SQLCODE OF 0                                 
CODE100  DC    F'100'              SQLCODE OF 100                               
QUIESCE  DC    XL3'000008'         TECB POSTCODE: STOP DB2 MODE=QUIE            
CONNECT  DC    CL12'CONNECT     '  NAME OF A CAF SERVICE.                       
OPEN     DC    CL12'OPEN        '  NAME OF A CAF SERVICE.                       
CLOSE    DC    CL12'CLOSE       '  NAME OF A CAF SERVICE.                       
DISCON   DC    CL12'DISCONNECT  '  DISCONNECT FROM DB2                          
TRANSLAT DC    CL12'TRANSLATE   '  TRANSLATE OPEN ERRORS                        
*                                                                               
SYNC     DC    CL14'SYNC'          TERMINATION OPTION (COMMIT)                  
ABRT     DC    CL14'ABRT'          TREMINATION OPTION (ROLLBACK)                
*                                                                               
*                                  RETURN CODES (RF) FROM CALL ATTACH           
ZERO     DC    F'0'                0                                            
FOUR     DC    F'4'                4                                            
EIGHT    DC    F'8'                8                                            
TWELVE   DC    F'12'               12  (CALL ATTACH RETURN CODE IN RF)          
NUM200   DC    F'200'              200 (USER ERROR)                             
NUM204   DC    F'204'              204 (CALL ATTACH SYSTEM ERROR)               
*                                                                               
*                                  REASON CODES (R0) FROM CALL ATTACH           
C10205   DC    CL4'00C10205'       CALL ATTACH COULD NOT TRANSLATE              
C10823   DC    CL4'00C10823'       CALL ATTACH FOUND A RELEAS MISSMATCH         
C10824   DC    CL4'00C10824'       CALL ATTACH READY FOR MORE INPUT             
F30002   DC    CL4'00F30002'       DB2 SUBSYSTEM NOT UP                         
F30011   DC    CL4'00F30011'       DB2 SUBSYSTEM NOT UP                         
F30012   DC    CL4'00F30012'       DB2 SUBSYSTEM NOT UP                         
F30025   DC    CL4'00F30025'       DB2 IS STOPPOING (REASCODE)                  
XTAB     DC    XL240'00'           TRANSLATE TABLE FOR BINARY TO HEX            
         DC    C'0123456789ABCDEF'                                              
*                                                                               
***********************************************************************         
*SQL COMMUNICATIONS AREA - MUST BE IN A SEPARATE AREA AND COVERED BY  *         
*A DSECT FOR A RE-ENTRANT PROGRAM                                     *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
         EXEC  SQL INCLUDE SQLCA                                                
         DS    0D                                                               
         DSNDRIB                   GET THE DB2 RELEASE INFORMATION BLCK         
*                                                                               
*                                  CALL MACRO PARAMTER LIST                     
CAFCALL  CALL  ,(*,*,*,*,*,*,*,*,*),VL,MF=L                                     
*                                                                               
LINES    EQU   10                                                               
LRECL    EQU   132                                                              
MESSAGE  DS    H,CL(LINES*LRECL)                                                
         ORG   MESSAGE                                                          
MESSAGEL DC    AL2(LINES*LRECL)                                                 
MESSAGE1 DS    CL(LRECL)                                                        
MESSAGE2 DS    CL(LRECL)                                                        
MESSAGE3 DS    CL(LRECL)                                                        
MESSAGE4 DS    CL(LRECL)                                                        
MESSAGE5 DS    CL(LRECL)                                                        
MESSAGE6 DS    CL(LRECL)                                                        
MESSAGE7 DS    CL(LRECL)                                                        
MESSAGE8 DS    CL(LRECL)                                                        
MESSAGE9 DS    CL(LRECL)                                                        
MESSAGEA DS    CL(LRECL)                                                        
*                                                                               
REGSAVE  DS    20000D                                                           
*                                                                               
**********************************************************************          
* PROGRAM VARIABLES DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
WRKD     DSECT                                                                  
SECB     DS    F                   DB2 START-UP ECB                             
TECB     DS    F                   DB2 TERMINATION ECB                          
         ORG   TECB+1              DB2 TERMINATION ECB                          
TECBCODE DS    XL3                                                              
LIALI    DS    F                   DSNALI ENTRY POINT ADDRESS                   
LISQL    DS    F                   DSNHLI2 ENTRY POINT ADDRESS                  
LITIAR   DS    F                   DSNTIAR ENTRY POINT ADDRESS                  
SSID     DS    CL4                 DB2 SUBSYSTEM ID. CONNECT PARAMETER          
PLAN     DS    CL8                 DB2 PAN NAME. OPEN PARAMETER                 
TRMOP    DS    CL4                 CLOSE TERMINATION OPTION (SYNC/ABRT)         
FUNCTN   DS    CL12                CAF FUNCTION TO BE CALLED                    
RIBPTR   DS    F                   DB2 PUTS RELEASE INFO BLOCK ADR HERE         
RETCODE  DS    F                   CHEKCODE SAVES RF HERE                       
REASCODE DS    F                   CHEKCODE SAVES R0 HERE                       
CONTROL  DS    CL8                 GO, SHUTDOWN, OR RESTART                     
SAVEAREA DS    18F                 SAVE AREA FOR CHEKCODE                       
SAVEHLI  DS    18F                 SAVE AREA FOR DSNHLI ENTRY ROUTINE           
SAVETIAR DS    18F                 SAVE AREA FOR DSNTIAR ENTRY ROUTINE          
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6A                                                               
FULL     DS    F                                                                
RCOUT    DS    XL8                                                              
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
ERROR    DS    XL1                                                              
APWORK   DS    CL8                                                              
*                                                                               
         DS    0D                                                               
HVSBRCDE DS    CL2                                                              
HVREPNAM DS    CL20                                                             
*                                                                               
         DS    0D                                                               
WSSQL    DS    (SQLDLEN)C                                                       
*                                                                               
         DS    0D                                                               
CLIDATA  DS    CL250                                                            
*                                                                               
         DS    0D                                                               
WSSQLD   DS    CL10000                                                          
*                                                                               
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE MXCLI2D                                                        
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINTL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126DDTRNSQL3 05/01/02'                                      
         END                                                                    
