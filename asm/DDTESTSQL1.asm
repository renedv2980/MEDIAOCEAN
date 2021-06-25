*          DATA SET DDTESTSQL1 AT LEVEL 002 AS OF 05/01/02                      
*PHASE TESTSQL                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'TEST SQL IN A SIMPLE ASSEMBLER PROGRAM'                         
TESTSQL  CSECT                                                                  
                                                                                
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
         NBASE 0,*TESTSQL,REGSAVE                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(20),=CL20'TEST SQL REQUESTS'                               
         EJECT                                                                  
**********************************************************************          
*LOAD CAF LANGAUGE INTERFACE ENTRY ADDRESSES                         *          
**********************************************************************          
LOADCAF  DS    0H                                                               
         LOAD  EP=DSNALI           LOAD CAF SERVICE REQUEST EP                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LIALI            SAVE THIS FOR CAF SERVICE REQUEST            
*                                                                               
         LOAD  EP=DSNHLI2          LOAD THE CAL SQL CALL ENTRY POINT            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LISQL            SAVE THIS FOR SQL CALLS                      
*                                                                               
         LOAD  EP=DSNTIAR          LOAD DSNTIAR                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R0,LITIAR                                                        
                                                                                
**********************************************************************          
*CONNECT TO DB2                                                      *          
**********************************************************************          
                                                                                
CONNREQ  DS    0H                                                               
         L     RF,LIALI            GET LANGAUGE INTERFACE ADDRESS               
         MVC   FUNCTN,CONNECT      GET FUNCTION TO CALL                         
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
                                                                                
**********************************************************************          
*OPEN DB2                                                            *          
**********************************************************************          
                                                                                
OPENREQ  DS    0H                                                               
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
         BE    LOOP                                                             
         DC    H'0'                                                             
                                                                                
**********************************************************************          
*CLOSE DB2                                                           *          
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
*DISCONNECT FROM DB2                                                 *          
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
*READ CARDS TO INVOKE DB2 FUNCTION                                    *         
***********************************************************************         
                                                                                
LOOP     LA    R2,CLIDATA          R2=A(MEDIA CLIENT EXTRACT RECORD)            
         USING MXCLID,R2                                                        
         MVI   MXCLID,C' '                                                      
         MVC   MXCLID+1(MXCLIDX-MXCLID),MXCLID                                  
*                                                                               
LOOP1    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
         MVC   P(20),C                                                          
         CLC   C(5),=C'CODE='                                                   
         BE    SINGLE                                                           
         CLC   C(5),=C'NAME='                                                   
         BE    MULTI                                                            
         B     LOOP1     '                                                      
         EJECT                                                                  
***********************************************************************         
*CLIENT TABLE DEFINITION                                              *         
*IF PLACED AT THE END OF THE PROGRAM WE GET WARNINGS ABOUT TABLE      *         
*PREVIOUSLY DECLARED OR REFERENCED.                                   *         
*IF OMMITTED WE GET MESSAGE "WARNINGS HAVE BEEN SUPRESSED DUE TO LACK *         
*OF TABLE DEFINITIONS".                                               *         
***********************************************************************         
                                                                                
         EXEC  SQL DECLARE CLIENT TABLE                 (              X        
               ActionLast         CHAR(1)       NOT NULL,              X        
               ActionDate         DATE          NOT NULL,              X        
               ActionTime         TIME          NOT NULL,              X        
               AgencyCode         CHAR(2)       NOT NULL,              X        
               MediaCode          SMALLINT      NOT NULL,              X        
               ClientCode         CHAR(5)       NOT NULL,              X        
               ShortName          CHAR(8)       NOT NULL,              X        
               Filter1            CHAR(1)       NOT NULL,              X        
               Filter2            CHAR(1)       NOT NULL,              X        
               Filter3            CHAR(1)       NOT NULL,              X        
               CreativeAgencyCode SMALLINT              ,              X        
               BuyingAgencyCode   SMALLINT              ,              X        
               BillingGroup       CHAR(2)               ,              X        
               SchemeCode         CHAR(4)               ,              X        
               Name               CHAR(30)      NOT NULL,              X        
               Address1           CHAR(30)              ,              X        
               Address2           CHAR(30)              ,              X        
               Address3           CHAR(30)              ,              X        
               Address4           CHAR(30)              ,              X        
               InvoicableCommRate CHAR(20)              ,              X        
               VATType            CHAR(8)               ,              X        
               FormulaSchemeCode  CHAR(10)              )                       
         EJECT                                                                  
***********************************************************************         
*SQL DESCRIPTOR AREAS - MUST APPEAR BEFORE THEY ARE REFERENCED        *         
***********************************************************************         
                                                                                
SQDDA1   EXEC  SQL INCLUDE SQLDA                                                
         EJECT                                                                  
***********************************************************************         
*SQL SINGLETON - GETS COLS FROM ONE ROW INTO LOCAL STORAGE            *         
***********************************************************************         
                                                                                
SINGLE   MVC   MXCLICDE,C+5        SINGLE CLIENT SELECT VIA CLI CODE            
         MVC   MXCLISHR,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL                                                     X        
               SELECT Shortname,Name                                   X        
               INTO   :MXCLISHR,:MXCLINAM                              X        
               FROM   CLIENT                                           X        
               WHERE  Mediacode=0                                      X        
               AND    ClientCode=:MXCLICDE                                      
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(4),FULL                                                     
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         B     LOOP                                                             
         EJECT                                                                  
***********************************************************************         
*SQL BROWSE - REQUIRES A CURSOR TO GET MULTIPLE ROWS                  *         
*NOTE THAT :WORK IN THE LIKE CLAUSE GIVES AN ERROR UNLESS IT IS       *         
*DEFINED BEFORE ITS USE IN THE DECLARE STATEMENT                      *         
*NOTE ALSO THAT THE DECLARE CURSOR STATEMENT CAN BE PLACED AT THE END *         
*OF THE PROGRAM (AFTER THE VARIABLES IT USES)                         *         
***********************************************************************         
                                                                                
WORK     DS    CL8                 USED BY LIKE CLAUSE IN DECLARE               
*                                                                               
         EXEC  SQL    DECLARE CLICUR CURSOR FOR                        X        
               SELECT ClientCode,ShortName,Name                        X        
               FROM   CLIENT                                           X        
               WHERE  MediaCode=1                                      X        
               AND    ShortName LIKE :WORK                                      
*                                                                               
MULTI    MVC   MXCLISHR,C+5        MULTI CLIENT SELECT VIA CLI NAME             
         MVC   MXCLICDE,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
         MVC   WORK,MXCLISHR                                                    
         LA    RE,WORK                                                          
         LA    R0,L'MXCLISHR                                                    
MULTI1   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,MULTI1                                                        
         MVI   0(RE),C'%'          SET SQL LIKE CHR AT END OF STRING            
*                                                                               
         EXEC  SQL OPEN CLICUR                                                  
*                                                                               
MULTI2   EXEC  SQL    FETCH CLICUR                                     X        
               INTO   :MXCLICDE,:MXCLISHR,:MXCLINAM                             
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(4),FULL                                                     
         CLC   SQLCODE,=H'100'     TEST NO MORE DATA                            
         BE    MULTIX                                                           
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   SQLCODE,=H'100'     *TEMP* SIMULATE SQL EOF                      
         B     MULTI2                                                           
*                                                                               
MULTIX   EXEC  SQL CLOSE CLICUR                                                 
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     LOOP                                                             
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
         MVC   STMT1,=H'132'                                                    
         MVC   STMT1+2(132),=CL132'SELECT * FROM HP4DDDST.CLIENT'               
*                                                                               
         EXEC  SQL DECLARE C1 CURSOR FOR S1                                     
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         EXEC  SQL PREPARE S1 FROM :STMT1                                       
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         EXEC  SQL DESCRIBE S1 INTO :WSSQLD                                     
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(8),RCOUT                                                    
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
* ??     BAS   RE,CALLTIAR                                                      
*                                                                               
* ??     BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
* ??     CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
* ??     BE    *+6                                                              
* ??     DC    H'0'                                                             
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
OUTCODE  MVI   FULL,C' '                                                        
         LH    RF,SQLCODE                                                       
         CVD   RF,DUB                                                           
         UNPK  FULL+1(3),DUB                                                    
         OI    FULL+3,C'0'                                                      
         LTR   RF,RF                                                            
         BNMR  RE                                                               
         MVI   FULL,C'-'                                                        
         BR    RE                                                               
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*SQL COMMUNICATIONS AREA - MUST BE IN A SEPARATE AREA AND COVERED BY  *         
*A DSECT FOR A RE-ENTRANT PROGRAM                                     *         
***********************************************************************         
                                                                                
         EXEC  SQL INCLUDE SQLCA                                                
*                                                                               
SSB      DC    F'0',F'0'                                                        
UTL      DC    F'0',AL1(04),XL3'00'                                             
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
DMCB     DS    6F                                                               
C        DC    CL80' '                                                          
*                                                                               
WSSQL    DS    CL80                                                             
*                                                                               
CLIDATA  DS    CL250                                                            
*                                                                               
REGSAVE  DS    1000D                                                            
         EJECT                                                                  
       ++INCLUDE MXCLID                                                         
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINTL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDTESTSQL105/01/02'                                      
         END                                                                    
