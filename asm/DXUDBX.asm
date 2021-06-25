*          DATA SET DXUDBX     AT LEVEL 099 AS OF 06/20/02                      
*PHASE DXUDB                                                                    
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'UPDATE DB2 DATABASE FROM SQL EXTRACT FILE'                      
**********************************************************************          
* DXUDB - UPDATE DB2 DATABASE FROM DDS DATA SQL EXTRACT RECORDS      *          
*                                                                    *          
* PARAMETERS:                                                        *          
*                                                                    *          
* AL4 INPUT -                                                        *          
*     A(SQL EXTRACT RECORD - LENGTH IN FIRST 2 OF 4 BYTES)           *          
*     RETURN -                                                       *          
*     A(SQL STATEMENT BUFFER)                                        *          
*                                                                    *          
* AL4 A(DATA EXTRACT CONTROL BLOCK - COVERED BY DXDSECTS)            *          
*                                                                    *          
* AL4 INPUT - INPUT CONTROL CODES                                    *          
*     RETURN - RETURN STATUS CODES                                   *          
*                                                                    *          
* AL4 A(STATUS RETURN MESSAGE BLOCK)                                 *          
*       1ST 2 BYTES - INPUT MAX LENGTH/RETURN RESULT LENGTH          *          
*       FOLLOWED BY RETURNED MESSAGE DATA                            *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         PRINT NOGEN                                                            
*                                                                               
DXUDB    CSECT                                                                  
         PRINT GEN                                                              
***********************************************************************         
* THIS MACRO ALLOWS EXEC SQL STATEMENTS TO BE BYPASSED BY THE         *         
* ASSEMBLER THE DB2 PRE-COMPILER WILL REPLACE EXEC SQL STATEMENTS     *         
***********************************************************************         
*        MACRO                                                                  
*LABEL   EXEC                                                                   
*LABEL   DS    0H                                                               
*        MEND                                                                   
         ENTRY DSNHLI                                                           
         NMOD1 WRKX-WRKD,**DXUDB*,R9,R8,CLEAR=YES                               
         USING WRKD,RC                                                          
         L     R7,=A(DXUDBC)                                                    
         USING DXUDBC,R7                                                        
*                                                                               
         ST    R1,APLIST           SAVE INPUT PARAMTERS                         
         MVC   DXUDBPAR,0(R1)                                                   
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   COLSMAX,132                                                      
         B     MAIN                                                             
*                                                                               
EXIT     EQU   *                                                                
         L     RF,=A(STMTBUFF-2)                                                
         SR    R1,R1                                                            
         ICM   R1,3,2(RF)                                                       
         LA    R1,4(R1)                                                         
         STCM  R1,3,0(RF)                                                       
         XC    2(2,RF),2(RF)                                                    
         STCM  RF,15,DXUPASQL                                                   
         ICM   RF,15,DXUPSBLK                                                   
         BZ    EXIT010                                                          
         ICM   RE,15,RETBPTR                                                    
         SR    RE,RF                                                            
         SHI   RE,2                                                             
         STCM  RE,3,0(RF)                                                       
EXIT010  EQU   *                                                                
         MVC   DXUPRCDS(2),ERRCODE                                              
         XC    DXUPRCDS+2(2),DXUPRCDS+2                                         
         ICM   RF,15,DXUPADXB                                                   
         BZ    EXIT020                                                          
         USING DXBLOCKD,RF                                                      
         MVC   DXDBEC,ERRCODE                                                   
         MVC   DXDBRC,RETCODE                                                   
         MVC   DXDBRS,REASCODE                                                  
         OC    DXDBEC,DXDBEC                                                    
         BZ    EXIT020                                                          
         B     EXIT020                                                          
         DROP  RF                                                               
         GOTO1 =A(RBCKSQL)                                                      
         BNE   EXIT020                                                          
         BAS   RE,FORMXLOG                                                      
         BNE   EXIT020                                                          
*                                                                               
         GOTO1 =A(CONRESET)        CONNECT RESET TO LOCAL SERVER                
*                                                                               
         GOTO1 =A(INSXLOG)                                                      
         BNE   EXIT020                                                          
*                                                                               
         GOTO1 =A(CONLOCN)         RECONNECT TO REMOTE DB2 LOCATION             
*                                                                               
         GOTO1 =A(COMMSQL)                                                      
         BNE   EXIT020                                                          
EXIT020  EQU   *                                                                
         L     R1,APLIST           EXIT WITH UPDATED PARAMETER LIST             
         MVC   0(L'DXUDBPAR,R1),DXUDBPAR                                        
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,VALINP           VALIDATE INPUT DATA                          
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,PROCACT          PROCESS INPUT ACTION                         
         BNE   EXIT                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
         MVC   P,SPACES                                                         
         MVC   SSID,SPACES                                                      
         MVC   PLAN,SPACES                                                      
         MVC   TRMOP,SPACES                                                     
*                                                                               
         L     RE,=A(COLDSC)                                                    
         STCM  RE,15,ACOLDSC                                                    
         STCM  RE,15,ACOLDSCP                                                   
         L     RE,=A(COLDSCX)                                                   
         STCM  RE,15,ACOLDSCX                                                   
*                                                                               
         L     RE,=A(TABDSC)                                                    
         STCM  RE,15,ATABDSC                                                    
         STCM  RE,15,ATABDSCP                                                   
         L     RE,=A(TABDSCX)                                                   
         STCM  RE,15,ATABDSCX                                                   
*                                                                               
         ICM   R3,15,DXUPADXB                                                   
         USING DXBLOCKD,R3                                                      
         MVC   THISDBID,DXDBID                                                  
         MVC   THISAUTH,DXDBAUTH                                                
         MVC   THISLOCN,DXDBLOCN                                                
         MVC   SSID,DXDBSSYS                                                    
         ICM   R4,15,DXSTPTR                                                    
         USING SXDTABD,R4                                                       
         MVC   COUNTRY,SXDTCTRY                                                 
         MVI   CUNTSQL,0                                                        
         MVC   CUNTSQL+1(1),COUNTRY                                             
         DROP  R3,R4                                                            
         MVC   PLAN,=CL8'DXUDB'                                                 
         MVC   TRMOP,=CL4'SYNC'                                                 
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE INPUT DATA PASSED IN PARAMETER LIST                       *          
**********************************************************************          
VALINP   NTR1                                                                   
         ICM   RF,15,DXUPSBLK                                                   
         BZ    VINP010                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(RF)                                                       
         STCM  RE,3,RETBLEFT                                                    
         LA    RF,2(RF)                                                         
         STCM  RF,15,RETBPTR                                                    
*                                                                               
VINP010  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIUPQ   TEST INPUT FUNCTION                          
         BNE   VINPOK                                                           
*                                                                               
         L     RF,DXUPAREC         VALIDATE EXTRACT RECORD INPUT                
         LA    RF,4(RF)                                                         
         STCM  RF,15,FLDPOS                                                     
*                                                                               
         BAS   RE,VALXID           VALIDATE EXTRACT RECORD ID                   
         BNE   VINPNO                                                           
         CLC   THISXID,=CL5'00000' IGNORE DXHDRD RECORD                         
         BE    VINPOK                                                           
         CLC   THISXID,=CL5'99999' IGNORE DXTRLD RECORD                         
         BE    VINPOK                                                           
*                                                                               
         MVC   FLDNUM,=YL2(2)      GET EXTRACT RECORD ACTION CODE               
         MVC   FLDTYPE,=YL2(1000)                                               
         BAS   RE,GETFLD                                                        
         BNE   VINPNO                                                           
         CLC   FLDLEN,=YL2(1)                                                   
         BNE   VINPERR2                                                         
         MVC   THISXACT,FLDVAL     SAVE EXTRACT RECORD ACTION CODE              
         MVC   FLDCNT,=YL2(1)                                                   
*&&US                                                                           
         MVC   FLDNUM,=YL2(4)                                                   
         MVC   FLDTYPE,=YL2(1000)                                               
         BAS   RE,GETFLD                                                        
         BNE   VINPNO                                                           
         MVC   FLDCNT,=YL2(1)                                                   
*&&                                                                             
         MVI   RECTERM,C'N'                                                     
*                                                                               
VINP110  EQU   *                   VALIDATE EXTRACT RECORD FIELD COUNT          
         CLI   FLDLAST,C'Y'                                                     
         BE    VINP120                                                          
         MVC   FLDTYPE,=YL2(1000)                                               
         BAS   RE,GETFNXT                                                       
         BNE   VINPERR4                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FLDCNT         SAVE EXTRACT RECORD FIELD COUNT              
         LA    RF,1(RF)                                                         
         STCM  RF,3,FLDCNT                                                      
         B     VINP110                                                          
*                                                                               
VINP120  EQU   *                                                                
         CLC   THISXID,=CL5'00001'                                              
         BE    VINPOK                                                           
         OC    FLDLEN,FLDLEN                                                    
         BNZ   *+8                                                              
         MVI   RECTERM,C'Y'                                                     
         ICM   RF,15,ATABDSCP      CHECK SQL TABLE COLUMN NUMBER                
         USING TABDSCD,RF                                                       
         MVC   HALF,TDSCNUM                                                     
         CLC   FLDCNT,TDSCNUM                                                   
         BNE   VINPERR5                                                         
         B     VINPOK                                                           
*                                                                               
VINPERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(1)                                                  
         BAS   RE,ERROUT                                                        
*                                                                               
VINPERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(2)                                                  
         BAS   RE,ERROUT                                                        
*                                                                               
VINPERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(3)                                                  
         BAS   RE,ERROUT                                                        
*                                                                               
VINPERR4 EQU   *                                                                
         MVC   ERRCODE,=YL2(4)                                                  
         BAS   RE,ERROUT                                                        
*                                                                               
VINPERR5 EQU   *                                                                
         MVC   P(40),=CL40'COLUMNS SHOULD BE: '                                 
         GOTO1 =V(HEXOUT),PARM,HALF,P+20,2,=C'TOG'                              
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'IS: '                                                
         GOTO1 =V(HEXOUT),PARM,FLDCNT,P+20,2,=C'TOG'                            
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(5)                                                  
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
VINPNO   B     NO                                                               
VINPOK   B     YES                                                              
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE EXTRACT RECORD ID - THISXID                               *          
**********************************************************************          
VALXID   NTR1                                                                   
         MVC   FLDNUM,=YL2(1)      VALIDATE EXTRACT RECORD ID FIELD             
         MVC   FLDTYPE,=YL2(1000)                                               
         BAS   RE,GETFLD                                                        
         BNE   VINPNO                                                           
         CLC   FLDLEN,=YL2(5)                                                   
         BNE   VXIDERR2                                                         
         MVC   THISXID,FLDVAL      SAVE EXTRACT RECORD ID                       
         CLC   THISXID,=CL5'00000' IGNORE DXHDRD RECORD                         
         BE    VXIDOK                                                           
         CLC   THISXID,=CL5'99999' IGNORE DXTRLD RECORD                         
         BE    VXIDOK                                                           
         CLC   THISXID,=CL5'00001' SAVE EXTRACT RECORD ID                       
         BE    VXIDOK                                                           
         BAS   RE,GETTDSC          GET SQL TABLE DESCRIBE INFO                  
         BNE   VXIDNO                                                           
         B     VXIDOK                                                           
*                                                                               
VXIDERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(11)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
VXIDERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(12)                                                 
         DC    H'0'                                                             
         BAS   RE,ERROUT                                                        
*                                                                               
VXIDNO   B     NO                                                               
VXIDOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS INPUT ACTION                                               *          
**********************************************************************          
PROCACT  NTR1                                                                   
         CLI   DXUPIFCD,DXUPIUPQ                                                
         BNE   PACT010                                                          
         BAS   RE,PROCUPD                                                       
         B     PACT100                                                          
*                                                                               
PACT010  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIOPQ                                                
         BNE   PACT020                                                          
         BAS   RE,PROCOPN                                                       
         B     PACT100                                                          
*                                                                               
PACT020  EQU   *                                                                
         CLI   DXUPIFCD,DXUPICLQ                                                
         BNE   PACT030                                                          
         BAS   RE,PROCCLS                                                       
         B     PACT100                                                          
*                                                                               
PACT030  EQU   *                                                                
         CLI   DXUPIFCD,DXUPICMQ                                                
         BNE   PACT040                                                          
         BAS   RE,PROCCOM                                                       
         B     PACT100                                                          
*                                                                               
PACT040  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIXLQ                                                
         BNE   PACT042                                                          
         BAS   RE,PROCXLG                                                       
         B     PACT100                                                          
*                                                                               
PACT042  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIULQ                                                
         BNE   PACT050                                                          
         BAS   RE,PROCULG                                                       
         B     PACT100                                                          
*                                                                               
PACT050  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIRBQ                                                
         BNE   PACT060                                                          
         BAS   RE,PROCRBK                                                       
         B     PACT100                                                          
*                                                                               
PACT060  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIBRQ                                                
         BNE   PACT070                                                          
         GOTO1 =A(PROCBRC)                                                      
         B     PACT100                                                          
*                                                                               
PACT070  EQU   *                                                                
         CLI   DXUPIFCD,DXUPIWRQ                                                
         BNE   PACT200                                                          
         GOTO1 =A(PROCWRC)                                                      
         B     PACT100                                                          
*                                                                               
PACT100  EQU   *                                                                
         B     PACTOK                                                           
*                                                                               
PACT200  EQU   *                                                                
         B     PACTERR1                                                         
*                                                                               
PACTERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(501)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PACTNO   B     NO                                                               
PACTOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS OPEN DB2 CONNECTION ACTION                                 *          
**********************************************************************          
PROCOPN  NTR1                                                                   
*                                                                               
         BAS   RE,INITCAF          INITIALISE CAF DB2 CONNECTION                
*                                                                               
         GOTO1 =A(CONLOCN)         CONNECT TO REMOTE DB2 LOCATION               
*                                                                               
         GOTO1 =A(SETPACK)         CONNECT TO REMOTE DB2 LOCATION               
*                                                                               
         B     POPNOK                                                           
*                                                                               
POPNNO   B     NO                                                               
POPNOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS CLOSE DB2 CONNECTION ACTION                                *          
**********************************************************************          
PROCCLS  NTR1                                                                   
*                                                                               
         BAS   RE,DISCCAF          DISCONNECT CAF DB2                           
*                                                                               
         B     PCLSOK                                                           
*                                                                               
PCLSNO   B     NO                                                               
PCLSOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS COMMIT DB2 UPDATES                                         *          
**********************************************************************          
PROCCOM  NTR1                                                                   
         GOTO1 =A(COMMSQL)                                                      
         BNE   PCOMNO                                                           
         B     PCOMOK                                                           
*                                                                               
PCOMNO   B     NO                                                               
PCOMOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS ROLLBACK DB2 UPDATES                                       *          
**********************************************************************          
PROCRBK  NTR1                                                                   
         GOTO1 =A(RBCKSQL)                                                      
         BNE   PRBKNO                                                           
         B     PRBKOK                                                           
*                                                                               
PRBKNO   B     NO                                                               
PRBKOK   B     YES                                                              
**********************************************************************          
* PROCESS EXTRACT DATA LOG                                           *          
**********************************************************************          
PROCXLG  NTR1                                                                   
         BAS   RE,FORMXLOG                                                      
         BNE   PXLGNO                                                           
*                                                                               
         GOTO1 =A(CONRESET)        CONNECT RESET TO LOCAL SERVER                
*                                                                               
         GOTO1 =A(INSXLOG)                                                      
         BNE   PXLGNO                                                           
*                                                                               
         GOTO1 =A(CONLOCN)         RECONNECT TO REMOTE DB2 LOCATION             
*                                                                               
         B     PXLGOK                                                           
*                                                                               
PXLGNO   B     NO                                                               
PXLGOK   B     YES                                                              
**********************************************************************          
* PROCESS UPDATE EVENT LOG                                           *          
**********************************************************************          
PROCULG  NTR1                                                                   
         BAS   RE,FORMULOG                                                      
         BNE   PULGNO                                                           
*                                                                               
         GOTO1 =A(CONRESET)        CONNECT RESET TO LOCAL SERVER                
*                                                                               
         GOTO1 =A(INSULOG)                                                      
         BNE   PULGNO                                                           
*                                                                               
         GOTO1 =A(CONLOCN)         RECONNECT TO REMOTE DB2 LOCATION             
*                                                                               
         B     PULGOK                                                           
*                                                                               
PULGNO   B     NO                                                               
PULGOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL UPDATIVE ACTION                                        *          
**********************************************************************          
PROCUPD  NTR1                                                                   
         CLC   THISXID,=CL5'00000' IGNORE DXHDRD RECORD                         
         BE    PUPD100                                                          
         CLC   THISXID,=CL5'99999' IGNORE DXTRLD RECORD                         
         BE    PUPD100                                                          
         CLI   THISXACT,C'A'                                                    
         BNE   PUPD010                                                          
         BAS   RE,PROCADD                                                       
         B     PUPD100                                                          
*                                                                               
PUPD010  EQU   *                                                                
         CLI   THISXACT,C'C'                                                    
         BNE   PUPD020                                                          
         BAS   RE,PROCCHG                                                       
         B     PUPD100                                                          
*                                                                               
PUPD020  EQU   *                                                                
         CLI   THISXACT,C'D'                                                    
         BNE   PUPD030                                                          
         BAS   RE,PROCDEL                                                       
         B     PUPD100                                                          
*                                                                               
PUPD030  EQU   *                                                                
         CLI   THISXACT,C'K'                                                    
         BNE   PUPD040                                                          
         BAS   RE,PROCDEL                                                       
         B     PUPD100                                                          
*                                                                               
PUPD040  EQU   *                                                                
         CLI   THISXACT,C'L'                                                    
         BNE   PUPD050                                                          
         BAS   RE,PROCLOAD                                                      
         B     PUPD100                                                          
*                                                                               
PUPD050  EQU   *                                                                
         CLI   THISXACT,C'S'                                                    
         BNE   PUPD200                                                          
         BAS   RE,PROCSTM                                                       
         B     PUPD100                                                          
*                                                                               
PUPD100  EQU   *                                                                
         B     PUPDOK                                                           
*                                                                               
PUPD200  EQU   *                                                                
         B     PUPDERR1                                                         
*                                                                               
PUPDERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(601)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PUPDNO   B     NO                                                               
PUPDOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL ADD ACTION                                             *          
**********************************************************************          
PROCADD  NTR1                                                                   
         BAS   RE,CLRSTMT                                                       
         L     R3,=A(STMTBUFF)                                                  
         LA    R3,2(R3)                                                         
         MVC   0(40,R3),=CL40'INSERT INTO'                                      
         LA    R3,12(R3)                                                        
         ICM   R4,15,=AL4(STMTBLEN)                                             
         SHI   R4,12                                                            
         GOTO1 LOADTEXT,PARM,TABFNAME,L'TABFNAME,(R3),(R4)                      
         BNE   PADDNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVC   0(9,R3),=CL9' VALUES ('                                          
         LA    R3,9(R3)                                                         
         SHI   R4,9                                                             
         ICM   R5,15,ATABDSCP                                                   
         USING TABDSCD,R5                                                       
         USING COLDSCD,R6                                                       
         ICM   R6,15,TDSCASTR                                                   
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         MVC   FLDNUM,=YL2(2)                                                   
*&&US*&& MVC   FLDNUM,=YL2(4)                                                   
         BAS   RE,GETFLD                                                        
         BNE   PADDNO                                                           
*                                                                               
PADD010  EQU   *                                                                
         CLM   R6,15,TDSCAEND                                                   
         BH    PADDERR4                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FLDLEN                                                      
         GOTO1 LOADTEXT,PARM,FLDVAL,(X'80',(RF)),(R3),(R4)                      
         BNE   PADDNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         CLI   FLDLAST,C'Y'                                                     
         BE    PADD020                                                          
         LA    R6,CDSCLQ(R6)                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         BAS   RE,GETFNXT                                                       
         BNE   PADDERR2                                                         
         GOTO1 LOADTEXT,PARM,=C',',1,(R3),(R4)                                  
         BNE   PADDNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         B     PADD010                                                          
*                                                                               
PADD020  EQU   *                                                                
         CHI   R4,1                                                             
         BL    PADDERR3                                                         
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
         SHI   R4,1                                                             
         ICM   RF,15,=AL4(STMTBLEN)                                             
         SR    RF,R4                                                            
         L     RE,=A(STMTBUFF)                                                  
         STCM  RF,3,0(RE)                                                       
*                                                                               
         MVI   NOTFOUND,C'N'                                                    
         GOTO1 =A(EXECSQL)                                                      
         BNE   PADDNO                                                           
         B     PADDOK                                                           
*                                                                               
PADDERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(61)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
PADDERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(62)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
PADDERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(63)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
PADDERR4 EQU   *                                                                
         MVC   ERRCODE,=YL2(64)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
PADDNO   B     NO                                                               
PADDOK   B     YES                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL CHANGE ACTION                                          *          
**********************************************************************          
PROCCHG  NTR1                                                                   
         BAS   RE,CLRSTMT                                                       
         L     R3,=A(STMTBUFF)                                                  
         LA    R3,2(R3)                                                         
         MVC   0(40,R3),=CL40'UPDATE '                                          
         LA    R3,7(R3)                                                         
         ICM   R4,15,=AL4(STMTBLEN)                                             
         SHI   R4,7                                                             
         GOTO1 LOADTEXT,PARM,TABFNAME,L'TABFNAME,(R3),(R4)                      
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVC   0(5,R3),=CL5' SET '                                              
         LA    R3,5(R3)                                                         
         SHI   R4,5                                                             
         ICM   R5,15,ATABDSCP                                                   
         USING TABDSCD,R5                                                       
         ICM   R6,15,TDSCASTR                                                   
         USING COLDSCD,R6                                                       
         SR    R2,R2                                                            
         ICM   R2,3,TDSCKNUM                                                    
         LTR   R2,R2                                                            
         BZ    PCHGOK                                                           
*&&UK*&& AHI   R2,4                                                             
*&&US*&& AHI   R2,3                                                             
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         MVC   FLDNUM,=YL2(2)                                                   
*&&US*&& MVC   FLDNUM,=YL2(4)                                                   
         BAS   RE,GETFLD                                                        
         BNE   PCHGNO                                                           
*                                                                               
PCHG010  EQU   *                                                                
         CLM   R6,15,TDSCAEND                                                   
         BH    PCHGERR5                                                         
*&&UK*&& CLC   FLDNUM,=YL2(5)                                                   
*&&US*&& CLC   FLDNUM,=YL2(4)                                                   
         BL    PCHG012                                                          
         CLM   R2,3,FLDNUM                                                      
         BL    PCHG012                                                          
         CLI   FLDLAST,C'Y'                                                     
         BE    PCHG020                                                          
         LA    R6,CDSCLQ(R6)                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         BAS   RE,GETFNXT                                                       
         BNE   PCHGERR2                                                         
         B     PCHG010                                                          
*                                                                               
PCHG012  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,CDSCNLEN                                                    
         GOTO1 LOADTEXT,PARM,CDSCNAME,(X'80',(RF)),(R3),(R4)                    
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         SHI   R4,1                                                             
         SR    RF,RF                                                            
         ICM   RF,3,FLDLEN                                                      
         GOTO1 LOADTEXT,PARM,FLDVAL,(X'80',(RF)),(R3),(R4)                      
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         CLI   FLDLAST,C'Y'                                                     
         BE    PCHG020                                                          
         LA    R6,CDSCLQ(R6)                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         BAS   RE,GETFNXT                                                       
         BNE   PCHGERR2                                                         
         GOTO1 LOADTEXT,PARM,=C',',1,(R3),(R4)                                  
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         B     PCHG010                                                          
*                                                                               
PCHG020  EQU   *                                                                
         MVC   0(7,R3),=CL7' WHERE '                                            
         LA    R3,7(R3)                                                         
         SHI   R4,7                                                             
         ICM   R5,15,ATABDSCP                                                   
         USING TABDSCD,R5                                                       
         ICM   R6,15,TDSCASTR                                                   
*&&UK*&& LA    R6,CDSCLQ*3(R6)                                                  
         USING COLDSCD,R6                                                       
         SR    R2,R2                                                            
         ICM   R2,3,TDSCKNUM                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
*&&UK*&& MVC   FLDNUM,=YL2(5)                                                   
*&&US*&& MVC   FLDNUM,=YL2(4)                                                   
         BAS   RE,GETFLD                                                        
         BNE   PCHGNO                                                           
*                                                                               
PCHG030  EQU   *                                                                
         CLM   R6,15,TDSCAEND                                                   
         BH    PCHGERR6                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CDSCNLEN                                                    
         GOTO1 LOADTEXT,PARM,CDSCNAME,(X'80',(RF)),(R3),(R4)                    
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         SHI   R4,1                                                             
         SR    RF,RF                                                            
         ICM   RF,3,FLDLEN                                                      
         GOTO1 LOADTEXT,PARM,FLDVAL,(X'80',(RF)),(R3),(R4)                      
         BNE   PCHGNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         CLI   FLDLAST,C'Y'                                                     
         BE    PCHG040                                                          
         BCT   R2,*+8                                                           
         B     PCHG040                                                          
         LA    R6,CDSCLQ(R6)                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         BAS   RE,GETFNXT                                                       
         BNE   PCHGERR3                                                         
         MVC   0(5,R3),=CL5' AND '                                              
         LA    R3,5(R3)                                                         
         SHI   R4,5                                                             
         B     PCHG030                                                          
*                                                                               
PCHG040  EQU   *                                                                
*                                                                               
         ICM   RF,15,=AL4(STMTBLEN)                                             
         SR    RF,R4                                                            
         L     RE,=A(STMTBUFF)                                                  
         STCM  RF,3,0(RE)                                                       
*                                                                               
         MVI   NOTFOUND,C'N'                                                    
         GOTO1 =A(EXECSQL)                                                      
         BNE   PCHGNO                                                           
         B     PCHGOK                                                           
*                                                                               
PCHGERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(121)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(122)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(123)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGERR4 EQU   *                                                                
         MVC   ERRCODE,=YL2(124)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGERR5 EQU   *                                                                
         MVC   ERRCODE,=YL2(125)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGERR6 EQU   *                                                                
         MVC   ERRCODE,=YL2(126)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PCHGNO   B     NO                                                               
PCHGOK   B     YES                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL DELETE ACTION                                          *          
**********************************************************************          
PROCDEL  NTR1                                                                   
         BAS   RE,CLRSTMT                                                       
         L     R3,=A(STMTBUFF)                                                  
         LA    R3,2(R3)                                                         
         MVC   0(40,R3),=CL40'DELETE FROM '                                     
         LA    R3,12(R3)                                                        
         ICM   R4,15,=AL4(STMTBLEN)                                             
         SHI   R4,12                                                            
         GOTO1 LOADTEXT,PARM,TABFNAME,L'TABFNAME,(R3),(R4)                      
         BNE   PDELNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVC   0(7,R3),=CL7' WHERE '                                            
         LA    R3,7(R3)                                                         
         SHI   R4,7                                                             
         ICM   R5,15,ATABDSCP                                                   
         USING TABDSCD,R5                                                       
         ICM   R6,15,TDSCASTR                                                   
*&&UK*&& LA    R6,CDSCLQ*3(R6)                                                  
         USING COLDSCD,R6                                                       
         SR    R2,R2                                                            
         ICM   R2,3,TDSCKNUM                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
*&&UK*&& MVC   FLDNUM,=YL2(5)                                                   
*&&US*&& MVC   FLDNUM,=YL2(4)                                                   
         BAS   RE,GETFLD                                                        
         BNE   PDELNO                                                           
         OC    FLDLEN,FLDLEN                                                    
         BZ    PDELERR5                                                         
*                                                                               
PDEL030  EQU   *                                                                
         CLM   R6,15,TDSCAEND                                                   
         BH    PDELERR6                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CDSCNLEN                                                    
         GOTO1 LOADTEXT,PARM,CDSCNAME,(X'80',(RF)),(R3),(R4)                    
         BNE   PDELNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         SHI   R4,1                                                             
         SR    RF,RF                                                            
         ICM   RF,3,FLDLEN                                                      
         GOTO1 LOADTEXT,PARM,FLDVAL,(X'80',(RF)),(R3),(R4)                      
         BNE   PDELNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
PDEL031  EQU   *                                                                
         CLI   FLDLAST,C'Y'                                                     
         BE    PDEL040                                                          
         BCT   R2,*+8                                                           
         B     PDEL040                                                          
         LA    R6,CDSCLQ(R6)                                                    
         MVC   FLDTYPE,CDSCTYPE                                                 
         MVC   FLDSQLD,CDSCSQLD                                                 
         BAS   RE,GETFNXT                                                       
         BNE   PDELERR3                                                         
         CLI   THISXACT,C'K'                                                    
         BNE   PDEL032                                                          
         OC    FLDLEN,FLDLEN                                                    
         BZ    PDEL031                                                          
         CLI   FLDSPACS,C'Y'                                                    
         BE    PDEL031                                                          
         CLI   FLDNULL,C'Y'                                                     
         BE    PDEL031                                                          
PDEL032  EQU   *                                                                
         MVC   0(5,R3),=CL5' AND '                                              
         LA    R3,5(R3)                                                         
         SHI   R4,5                                                             
         B     PDEL030                                                          
*                                                                               
PDEL040  EQU   *                                                                
*                                                                               
         ICM   RF,15,=AL4(STMTBLEN)                                             
         SR    RF,R4                                                            
         L     RE,=A(STMTBUFF)                                                  
         STCM  RF,3,0(RE)                                                       
*                                                                               
         CLI   THISXACT,C'K'                                                    
         BE    *+12                                                             
         MVI   NOTFOUND,C'N'                                                    
         B     *+8                                                              
         MVI   NOTFOUND,C'Y'                                                    
         GOTO1 =A(EXECSQL)                                                      
         BNE   PDELNO                                                           
         B     PDELOK                                                           
*                                                                               
PDELERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(401)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(402)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(403)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELERR4 EQU   *                                                                
         MVC   ERRCODE,=YL2(404)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELERR5 EQU   *                                                                
         MVC   ERRCODE,=YL2(405)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELERR6 EQU   *                                                                
         MVC   ERRCODE,=YL2(406)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PDELNO   B     NO                                                               
PDELOK   B     YES                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL LOAD ACTION                                            *          
**********************************************************************          
PROCLOAD NTR1                                                                   
         ICM   R5,15,ATABDSCP                                                   
         USING TABDSCD,R5                                                       
         TM    TDSCFLG1,TDSCFCTQ                                                
         BNZ   PLOA010                                                          
         GOTO1 =A(CLRTABLE)                                                     
         BNE   PLOANO                                                           
         OI    TDSCFLG1,TDSCFCTQ                                                
*                                                                               
PLOA010  EQU   *                                                                
         BAS   RE,PROCADD                                                       
         BNE   PLOANO                                                           
         B     PLOAOK                                                           
*                                                                               
PLOANO   B     NO                                                               
PLOAOK   B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL STATEMENT EMBEDDED IN EXTRACT RECORD                   *          
**********************************************************************          
PROCSTM  NTR1                                                                   
         BAS   RE,CLRSTMT                                                       
         L     R3,=A(STMTBUFF)                                                  
         LA    R3,2(R3)                                                         
         ICM   R4,15,=AL4(STMTBLEN)                                             
         MVC   FLDTYPE,=YL2(1000)                                               
         MVC   FLDNUM,=YL2(3)                                                   
         BAS   RE,GETFLD                                                        
         BNE   PSTMNO                                                           
         SR    RF,RF                                                            
         ICM   RF,3,FLDLEN                                                      
         GOTO1 LOADTEXT,PARM,FLDVAL,(X'80',(RF)),(R3),(R4)                      
         BNE   PSTMNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         ICM   RF,15,=AL4(STMTBLEN)                                             
         SR    RF,R4                                                            
         L     RE,=A(STMTBUFF)                                                  
         STCM  RF,3,0(RE)                                                       
*                                                                               
         MVI   NOTFOUND,C'Y'                                                    
         GOTO1 =A(EXECSQL)                                                      
         BNE   PSTMNO                                                           
         B     PSTMOK                                                           
*                                                                               
PSTMERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(151)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PSTMERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(152)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PSTMNO   B     NO                                                               
PSTMOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* GET DATABASE TABLE DESCRIPTOR LIST ENTRY, BUILD IF NOT PRESENT     *          
**********************************************************************          
GETTDSC  NTR1                                                                   
         ICM   R2,15,ATABDSC       R2=A(DATABSE TABLE DESCRIBE LIST)            
         USING TABDSCD,R2                                                       
GTDS010  EQU   *                                                                
         CLI   TDSCID,0                                                         
         BE    GTDS100                                                          
         CLC   TDSCID,THISXID                                                   
         BE    GTDS020                                                          
         LA    R2,TDSCLQ(R2)                                                    
         B     GTDS010                                                          
GTDS020  EQU   *                   HERE IF ENTRY FOUND                          
         STCM  R2,15,ATABDSCP      SAVE ADDRESS OF EXISTING ENTRY               
         MVC   THISTBNM,TDSCTABN   SAVE SQL TABLE NAME                          
         MVC   THISNDXN,TDSCNDXN   SAVE SQL TABLE INDEX NAME                    
         MVC   THISKNUM,TDSCKNUM   SAVE NUMBER OF KEY FIELDS                    
         BAS   RE,BLDTNAM          BUILD SQL TABLE NAME                         
         BNE   GTDSNO                                                           
         BAS   RE,BLDINAM          BUILD SQL TABLE INDEX NAME                   
         BNE   GTDSNO                                                           
         B     GTDSOK                                                           
*                                                                               
GTDS100  EQU   *                   HERE IF ENTRY NOT FOUND                      
         STCM  R2,15,ATABDSCP      SAVE ADDRESS AND BUILD NEW ENTRY             
         BAS   RE,BLDCDSC                                                       
         BNE   GTDSNO                                                           
         B     GTDSOK                                                           
*                                                                               
GTDSERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(21)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GTDSNO   B     NO                                                               
GTDSOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD TABLE AND COLUMN DESCRIPTOR LIST ENTRIES                     *          
**********************************************************************          
BLDCDSC  NTR1                                                                   
         MVC   P(40),=CL40'PROCESSING RECORD ID: '                              
         MVC   P+22(L'THISXID),THISXID                                          
         GOTO1 DXUPRTL                                                          
*                                                                               
         GOTO1 =A(CONRESET)        CONNECT RESET TO LOCAL SERVER                
*                                                                               
         GOTO1 =A(GETXID)          GET EXTRACT RECORD ID TABLE INFO             
         BNE   BCDSNO                                                           
         BAS   RE,BLDTNAM          BUILD SQL TABLE NAME                         
         BNE   BCDSNO                                                           
         BAS   RE,BLDINAM          BUILD SQL TABLE INDEX NAME                   
         BNE   BCDSNO                                                           
*                                                                               
         MVC   P(40),=CL40'DESCRIBING TABLE: '                                  
         MVC   P+22(L'TABFNAME),TABFNAME                                        
         GOTO1 DXUPRTL                                                          
         GOTO1 =A(NUMKEYS)         GET NUMBER OF KEY FIELDS FROM SYSIBM         
         BNE   BCDSNO                                                           
         MVC   P(40),=CL40'NUMBER OF KEYS: '                                    
         GOTO1 =V(HEXOUT),PARM,THISKNUM,P+16,2,=C'TOG'                          
         GOTO1 DXUPRTL                                                          
         GOTO1 =A(DESCTAB)         EXEC SQL DESCRIBE CALL ON TABLE              
         BNE   BCDSNO                                                           
*                                                                               
         GOTO1 =A(CONLOCN)         RECONNECT TO REMOTE DB2 LOCATION             
*                                                                               
         ICM   R2,15,COLDSCN       BUILD SQL TABLE COLUMN DESCRIBE LIST         
         USING COLDSCD,R2                                                       
         STCM  R2,15,ACOLDSCP                                                   
*                                                                               
         ICM   R3,15,ATABDSCP                                                   
         USING TABDSCD,R3                                                       
         MVC   TDSCID,THISXID                                                   
         OI    TDSCFLG1,TDSCFCTQ                                                
         MVC   TDSCTABN,THISTBNM   SAVE SQL TABLE NAME                          
         MVC   TDSCNDXN,THISNDXN   SAVE SQL TABLE INDEX NAME                    
         MVC   TDSCKNUM,THISKNUM                                                
*                                                                               
         L     R4,=A(WSSQLD)         NEED W/S TO BUILD SQL PARAM LIST           
         USING SQLDA,R4                                                         
         MVC   TDSCNUM,SQLD                                                     
         OC    TDSCNUM,TDSCNUM                                                  
         BZ    BCDSOK                                                           
         STCM  R2,15,TDSCASTR                                                   
         STCM  R2,15,TDSCAEND                                                   
         SR    RF,RF                                                            
         ICM   RF,3,TDSCNUM                                                     
         LA    R5,SQLVAR                                                        
         USING SQLVARN,R5                                                       
BCDS100  EQU   *                                                                
         MVC   CDSCTYPE,SQLTYPE                                                 
         MVC   CDSCSQLD,SQLDATA                                                 
         MVC   CDSCLEN,SQLLEN                                                   
         MVC   CDSCNLEN,SQLNAME                                                 
         MVC   CDSCNAME,SQLNAME+2                                               
         BCT   RF,*+8                                                           
         B     BCDS110                                                          
         LA    R2,CDSCLQ(R2)                                                    
         LA    R5,SQLSIZV(R5)                                                   
         B     BCDS100                                                          
BCDS110  STCM  R2,15,TDSCAEND                                                   
         LA    R2,CDSCLQ(R2)                                                    
         STCM  R2,15,COLDSCN                                                    
         B     BCDSOK                                                           
*                                                                               
BCDSERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(31)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
BCDSNO   B     NO                                                               
BCDSOK   B     YES                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
**********************************************************************          
* BUILD TABLE NAME INTO TABFNAME                                     *          
**********************************************************************          
BLDTNAM  NTR1                                                                   
         MVC   TABFNAME,SPACES                                                  
         LA    R3,TABFNAME                                                      
         LA    R4,L'TABFNAME                                                    
         GOTO1 LOADTEXT,PARM,THISAUTH,L'THISAUTH,(R3),(R4)                      
         BNE   BTNANO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         GOTO1 LOADTEXT,PARM,=C'.',1,(R3),(R4)                                  
         BNE   BTNANO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         GOTO1 LOADTEXT,PARM,THISTBNM,L'THISTBNM,(R3),(R4)                      
         BNE   BTNANO                                                           
         B     BTNAOK                                                           
*                                                                               
BTNAERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(41)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
BTNANO   B     NO                                                               
BTNAOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* BUILD TABLE INDEX NAME INTO TNDXNAME                               *          
**********************************************************************          
BLDINAM  NTR1                                                                   
         MVC   TNDXNAME,SPACES                                                  
         LA    R3,TNDXNAME                                                      
         LA    R4,L'TNDXNAME                                                    
         GOTO1 LOADTEXT,PARM,THISAUTH,L'THISAUTH,(R3),(R4)                      
         BNE   BINANO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         GOTO1 LOADTEXT,PARM,=C'.',1,(R3),(R4)                                  
         BNE   BINANO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         GOTO1 LOADTEXT,PARM,THISNDXN,L'THISNDXN,(R3),(R4)                      
         BNE   BINANO                                                           
         B     BINAOK                                                           
*                                                                               
BINAERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(141)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
BINANO   B     NO                                                               
BINAOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* INITIALISE CAF DB2 CONNECTION                                      *          
**********************************************************************          
INITCAF  NTR1                                                                   
*                                  LOAD CAF LANGAUGE INTERFACE                  
*                                  ENTRY ADDRESSES                              
LOADCAF  DS    0H                                                               
*                                                                               
         LOAD  EP=DSNALI           LAOD CAF SERVICE REQUEST EP                  
         LTR   RF,RF                                                            
         BNZ   ICAFERR3                                                         
         ST    R0,LIALI            SAVE THIS FOR CAF SERVICE REQUEST            
*                                                                               
         LOAD  EP=DSNHLI2          LOAD THE CAL SQL CALL ENTRY POINT            
         LTR   RF,RF                                                            
         BNZ   ICAFERR3                                                         
         ST    R0,LISQL            SAVE THIS FOR SQL CALLS                      
*                                                                               
         LOAD  EP=DSNTIAR          LAOD DSNTIAR                                 
         LTR   RF,RF                                                            
         BNZ   ICAFERR3                                                         
         ST    R0,LITIAR                                                        
*                                                                               
*                                  CONNECT TO DB2                               
CONNREQ  DS    0H                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
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
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   ICAFERR1                                                         
*                                                                               
         MVC   P(40),=CL40'AFTER CONNECT TO DB2'                                
         GOTO1 DXUPRTL                                                          
         SPACE 1                                                                
*                                  OPEN DB2                                     
OPENREQ  DS    0H                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
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
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   ICAFERR2                                                         
*                                                                               
         MVC   P(40),=CL40'AFTER OPEN DB2 PLAN FOR DXUDB'                       
         GOTO1 DXUPRTL                                                          
*                                                                               
         B     ICAFOK                                                           
*                                                                               
ICAFERR1 EQU   *                                                                
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(801)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
ICAFERR2 EQU   *                                                                
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(802)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
ICAFERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(803)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
ICAFNO   B     NO                                                               
ICAFOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* DISCONNECT CAF DB2                                                 *          
**********************************************************************          
DISCCAF  NTR1                                                                   
*                                  CLOSE DB2                                    
CLOSEREQ DS    0H                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
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
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   DCAFERR1                                                         
*                                  DISCONNECT FROM DB2                          
DISCREQ  DS    0H                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
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
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   DCAFERR2                                                         
         B     DELCAF                                                           
*                                  TRANSLATE                                    
XLATEREQ DS    0H                                                               
         LA    R5,SQLCA            SQL COMMUNICATION AREA                       
*??      MVC   CALLPA(CAFLEN),CAFCALL                                           
*                                                                               
*                                  DELETE CAF LANGAUGE INTERFACE                
*                                  LOAD MODULES                                 
DELCAF   DS    0H                                                               
         DELETE EP=DSNALI          CORRECTLY MAINTAIN USE COUNT                 
         DELETE EP=DSNHLI2         CORRECTLY MAINTAIN USE COUNT                 
*                                                                               
         B     DCAFOK                                                           
*                                                                               
DCAFERR1 EQU   *                                                                
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(811)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
DCAFERR2 EQU   *                                                                
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(812)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
DCAFNO   B     NO                                                               
DCAFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* FORMAT EXTRACT DATA LOG HOST VARIABLES                              *         
***********************************************************************         
         SPACE 1                                                                
FORMXLOG NTR1                                                                   
         ICM   R3,15,DXUPADXB                                                   
         BZ    FXLOERR1                                                         
         USING DXBLOCKD,R3                                                      
         GOTO1 FORMDATE,PARM,DXDATEN,HVXDATE                                    
         BNE   FXLONO                                                           
         GOTO1 FORMTIME,PARM,DXTIMEN,HVXTIME                                    
         BNE   FXLONO                                                           
         MVC   HVXJOB,DXJOBNAM                                                  
         GOTO1 FORMDATE,PARM,DXJDATE,HVXJDATE                                   
         BNE   FXLONO                                                           
         GOTO1 FORMTIME,PARM,DXJTIME,HVXJTIME                                   
         BNE   FXLONO                                                           
         MVC   HVXMODE,DXMODE                                                   
         MVC   HVXFDA,DXFDA                                                     
         MVC   HVXTDA,DXTDA                                                     
         ICM   R4,15,DXSTPTR                                                    
         USING SXDTABD,R4                                                       
         BZ    FXLOERR2                                                         
         MVC   HVXAGY,SXDTAGY                                                   
         GOTO1 FORMSYS,PARM,SXDTSYS,HVXSYS                                      
         BNE   FXLONO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   FXLONO                                                           
         MVC   HVXSNUM,SXDTGNUM                                                 
         MVC   HVXTYPE,SXDTTYP                                                  
         MVC   HVXRCNT,SXDTRNUM                                                 
         MVC   HVXBCNT,SXDTBNUM                                                 
         MVC   HVXMCNT,SXDTMTOT                                                 
*                                                                               
FXLO100  EQU   *                                                                
         B     FXLOOK                                                           
*                                                                               
FXLOERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(781)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FXLOERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(782)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FXLONO   B     NO                                                               
FXLOOK   B     YES                                                              
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FORMAT UPDATE EVENT LOG HOST VARIABLES                              *         
***********************************************************************         
         SPACE 1                                                                
FORMULOG NTR1                                                                   
         ICM   R3,15,DXUPADXB                                                   
         BZ    FULOERR1                                                         
         USING DXBLOCKD,R3                                                      
         MVC   HVXDBID,THISDBID                                                 
         MVC   HVXAUTH,THISAUTH                                                 
         MVC   HVXLOCN,THISLOCN                                                 
         GOTO1 FORMDATE,PARM,DXDATEN,HVXUDATE                                   
         BNE   FULONO                                                           
         GOTO1 FORMTIME,PARM,DXTIMEN,HVXUTIME                                   
         BNE   FULONO                                                           
         GOTO1 FORMDATE,PARM,DXDATEN,HVXDATE                                    
         BNE   FULONO                                                           
         GOTO1 FORMTIME,PARM,DXTIMEN,HVXTIME                                    
         BNE   FULONO                                                           
         MVC   HVXJOB,DXJOBNAM                                                  
         GOTO1 FORMDATE,PARM,DXJDATE,HVXJDATE                                   
         BNE   FULONO                                                           
         GOTO1 FORMTIME,PARM,DXJTIME,HVXJTIME                                   
         BNE   FULONO                                                           
         MVC   HVXMODE,DXMODE                                                   
         ICM   R4,15,DXSTPTR                                                    
         USING SXDTABD,R4                                                       
         BZ    FULOERR2                                                         
         MVC   HVXAGY,SXDTAGY                                                   
         GOTO1 FORMSYS,PARM,SXDTSYS,HVXSYS                                      
         BNE   FULONO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   FULONO                                                           
         MVC   HVXEC,DXDBEC                                                     
         MVC   HVXRC,DXDBRC                                                     
         MVC   HVXRS,DXDBRS                                                     
*                                                                               
FULO100  EQU   *                                                                
         B     FULOOK                                                           
*                                                                               
FULOERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(781)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FULOERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(782)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FULONO   B     NO                                                               
FULOOK   B     YES                                                              
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PROCESS SQL ERROR                                                  *          
**********************************************************************          
PSQLERR  NTR1                                                                   
         MVC   P(40),=CL40'SQL ERROR - RETURN CODE: HEX'                        
         MVC   P+38(40),=CL40'REASON CODE: '                                    
         GOTO1 =V(HEXOUT),PARM,RETCODE,P+28,4,=C'TOG'                           
         GOTO1 =V(HEXOUT),PARM,REASCODE,P+51,4,=C'TOG'                          
         B     PSEROK                                                           
*                                                                               
PSERNO   B     NO                                                               
PSEROK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* LOAD TEXT INTO STORAGE AREA                                        *          
* P1 - A(TEXT) / RETURN NEXT POSITION IN STORAGE                     *          
* P2 - TOP BIT SET - RAD TO END OF INPUT - ELSE READ TO FIRST SPACE  *          
*      MAX LENGTH TEXT / RETURN ACTUAL LENGTH LOADED                 *          
* P3 - A(STORAGE AREA)                                               *          
* P4 - MAX LENGTH STORAGE AREA                                       *          
**********************************************************************          
LOADTEXT NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LTR   R3,R3                                                            
         BZ    LTEX020                                                          
         BNM   *+14                                                             
         SLL   R3,1                                                             
         SRL   R3,1                                                             
         BCTR  RF,0                                                             
         LTR   R3,R3                                                            
         BZ    LTEX020                                                          
         LTR   R5,R5                                                            
         BZ    LTEXERR1                                                         
LTEX010  EQU   *                                                                
         LTR   RF,RF                                                            
         BNZ   *+12                                                             
         CLI   0(R2),C' '                                                       
         BE    LTEX020                                                          
         MVC   0(1,R4),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         BCT   R5,*+8                                                           
         B     LTEXERR2                                                         
         BCT   R3,LTEX010                                                       
LTEX020  EQU   *                                                                
         STCM  R4,15,0(R1)                                                      
         STCM  RE,15,4(R1)                                                      
         B     LTEXOK                                                           
*                                                                               
LTEXERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(51)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
LTEXERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(52)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
LTEXNO   B     NO                                                               
LTEXOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* GET FIELD FROM SQL EXTRACT RECORD                                  *          
* FLDNUM - CURRENT FIELD NUMBER                                      *          
* FLDPOS - ADDRESS CURRENT POSITION IN RECORD                        *          
* FLDLEN - LENGTH OF FIELD                                           *          
* FLDVAL - TEXT VALUE OF FIELD                                       *          
* FLDLAST - LAST FIELD FLAG = 'Y'                                    *          
**********************************************************************          
GETFLD   NTR1                                                                   
         ICM   R3,15,DXUPAREC                                                   
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         SHI   RF,4                                                             
         LA    R3,4(R3)                                                         
         LR    R2,R3                                                            
         AR    R3,RF                                                            
         BCTR  R3,0                                                             
         CLI   RECTERM,C'Y'                                                     
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVI   FLDLAST,C'N'                                                     
         SR    R1,R1                                                            
         ICM   R1,3,FLDNUM                                                      
         BZ    GFLDERR1                                                         
         BCT   R1,*+8                                                           
         B     GFLD030                                                          
GFLD010  EQU   *                                                                
         CR    R2,R3                                                            
         BNL   GFLDERR2                                                         
         CLI   0(R2),C';'                                                       
         BNE   GFLD020                                                          
         BCT   R1,*+12                                                          
         LA    R2,1(R2)                                                         
         B     GFLD030                                                          
GFLD020  LA    R2,1(R2)                                                         
         B     GFLD010                                                          
GFLD030  EQU   *                                                                
         STCM  R2,15,FLDPOS                                                     
         BAS   RE,GETFVAL                                                       
         BNE   GFLDNO                                                           
         B     GFLDOK                                                           
*                                                                               
GFLD200  EQU   *                                                                
         B     GFLDNO                                                           
*                                                                               
GFLDERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(81)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFLDERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(82)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFLDNO   B     NO                                                               
GFLDOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* GET FIELD VALUE FROM CURRENT POSITION IN SQL EXTRACT RECORD        *          
* FLDNUM - CURRENT FIELD NUMBER                                      *          
* FLDPOS - ADDRESS CURRENT POSITION IN RECORD                        *          
* FLDLEN - LENGTH OF FIELD                                           *          
* FLDVAL - TEXT VALUE OF FIELD                                       *          
* FLDLAST - LAST FIELD FLAG = 'Y'                                    *          
**********************************************************************          
GETFVAL  NTR1                                                                   
         MVI   FLDSPACS,C'Y'                                                    
         MVI   FLDNULL,C'N'                                                     
         MVI   FLDHEX,C'N'                                                      
         CLC   FLDTYPE,=YL2(480)                                                
         BNL   GFVA002                                                          
         CLC   FLDSQLD+2(2),=XL2'FFFF'                                          
         BNE   GFVA002                                                          
         MVI   FLDHEX,C'Y'                                                      
*                                                                               
GFVA002  EQU   *                                                                
         ICM   R2,15,FLDPOS                                                     
         ICM   R3,15,DXUPAREC                                                   
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         SHI   RF,4                                                             
         LA    R3,4(R3)                                                         
         AR    R3,RF                                                            
         BCTR  R3,0                                                             
         CLI   RECTERM,C'Y'                                                     
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         LA    R0,FLDVAL                                                        
         LA    R1,L'FLDVAL                                                      
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,=CL4' '                                                     
         MVCL  R0,RE                                                            
         LA    RF,FLDVAL                                                        
         LA    R0,L'FLDVAL                                                      
         SR    RE,RE                                                            
         CLI   0(R2),C';'                                                       
         BE    GFVA040                                                          
         CLI   FLDHEX,C'N'                                                      
         BE    GFVA008                                                          
         MVI   0(RF),C'X'                                                       
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
GFVA008  EQU   *                                                                
         CLC   FLDTYPE,=YL2(480)                                                
         BNL   GFVA010                                                          
         MVI   0(RF),C''''                                                      
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
GFVA010  EQU   *                                                                
         CLI   0(R2),C';'                                                       
         BE    GFVA020                                                          
         CR    R2,R3                                                            
         BNL   GFVAERR2                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         MVI   FLDSPACS,C'N'                                                    
         CLC   FLDTYPE,=YL2(480)                                                
         BNL   GFVA012                                                          
         CLI   0(R2),C''''                                                      
         BNE   GFVA012                                                          
         MVI   0(RF),ESCHAR                                                     
* ??     B     GFVA014            ??                                            
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GFVA012                                                       
         B     GFVAERR4                                                         
GFVA012  EQU   *                                                                
         MVC   0(1,RF),0(R2)                                                    
GFVA014  EQU   *                                                                
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GFVA010                                                       
         B     GFVAERR1                                                         
GFVA020  EQU   *                                                                
         CLI   FLDSPACS,C'N'                                                    
         BE    GFVA030                                                          
         TM    FLDTYPE+1,X'01'                                                  
         BZ    GFVA030                                                          
         MVC   FLDVAL(4),=CL4'NULL'                                             
         LA    RE,4                                                             
         MVI   FLDNULL,C'Y'                                                     
         B     GFVA040                                                          
*                                                                               
GFVA030  EQU   *                                                                
         CLC   FLDTYPE,=YL2(480)                                                
         BNL   GFVA040                                                          
         BCT   R0,*+8                                                           
         B     GFVAERR3                                                         
         MVI   0(RF),C''''                                                      
         LA    RE,1(RE)                                                         
*&&US                                                                           
         CHI   RE,6                                                             
         BNE   GFVA040                                                          
         CLC   FLDVAL+1(4),=CL4'NULL'                                           
         BNE   GFVA040                                                          
         MVC   FLDVAL(6),=CL6'NULL  '                                           
         LA    RE,4                                                             
         MVI   FLDNULL,C'Y'                                                     
*&&                                                                             
*                                                                               
GFVA040  EQU   *                                                                
         STCM  RE,3,FLDLEN                                                      
         CR    R2,R3                                                            
         BNE   *+8                                                              
         MVI   FLDLAST,C'Y'                                                     
         CLC   FLDTYPE,=YL2(384)                                                
         BE    GFVA050                                                          
         CLC   FLDTYPE,=YL2(385)                                                
         BE    GFVA050                                                          
         B     GFVAOK                                                           
GFVA050  EQU   *                                                                
*        BAS   RE,REFDATE                                                       
         B     GFVAOK                                                           
*                                                                               
GFVA200  EQU   *                                                                
         B     GFVANO                                                           
*                                                                               
GFVAERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(91)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFVAERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(92)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFVAERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(93)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFVAERR4 EQU   *                                                                
         MVC   ERRCODE,=YL2(94)                                                 
         BAS   RE,ERROUT                                                        
*                                                                               
GFVANO   B     NO                                                               
GFVAOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* GET FIELD VALUE FROM NEXT POSITION IN SQL EXTRACT RECORD           *          
* FLDNUM - CURRENT FIELD NUMBER                                      *          
* FLDPOS - ADDRESS CURRENT POSITION IN RECORD                        *          
* FLDLEN - LENGTH OF FIELD                                           *          
* FLDVAL - TEXT VALUE OF FIELD                                       *          
* FLDLAST - LAST FIELD FLAG = 'Y'                                    *          
**********************************************************************          
GETFNXT  NTR1                                                                   
         ICM   R2,15,FLDPOS                                                     
         ICM   R3,15,DXUPAREC                                                   
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         SHI   RF,4                                                             
         LA    R3,4(R3)                                                         
         AR    R3,RF                                                            
         BCTR  R3,0                                                             
         CLI   RECTERM,C'Y'                                                     
         BNE   *+6                                                              
         BCTR  R3,0                                                             
GFNX010  EQU   *                                                                
         CR    R2,R3                                                            
         BNL   GFNXERR1                                                         
         CLI   0(R2),C';'                                                       
         BE    GFNX030                                                          
         LA    R2,1(R2)                                                         
         B     GFNX010                                                          
GFNX030  EQU   *                                                                
         LA    R2,1(R2)                                                         
         STCM  R2,15,FLDPOS                                                     
         BAS   RE,GETFVAL                                                       
         BNE   GFNXNO                                                           
         ICM   RF,3,FLDNUM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,FLDNUM                                                      
         B     GFNXOK                                                           
*                                                                               
GFNX200  EQU   *                                                                
         B     GFNXNO                                                           
*                                                                               
GFNXERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(101)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
GFNXNO   B     NO                                                               
GFNXOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* WRAP CURETN FIELD IN DINKS ('')                                    *          
* FLDNUM - CURRENT FIELD NUMBER                                      *          
* FLDPOS - ADDRESS CURRENT POSITION IN RECORD                        *          
* FLDLEN - LENGTH OF FIELD                                           *          
* FLDVAL - TEXT VALUE OF FIELD                                       *          
* FLDLAST - LAST FIELD FLAG = 'Y'                                    *          
**********************************************************************          
DINKFLD  NTR1                                                                   
         LA    R2,FLDVAL                                                        
         SR    RE,RE                                                            
         ICM   RE,3,FLDLEN                                                      
         BZ    DFLDERR1                                                         
         MVC   CHAR1,0(R2)                                                      
         MVI   0(R2),C''''                                                      
DFLD010  EQU   *                                                                
         LA    R2,1(R2)                                                         
         MVC   CHAR2,0(R2)                                                      
         CLI   CHAR1,C''''                                                      
         BNE   DFLD012                                                          
         MVC   CHAR1,C'*'                                                       
DFLD012  EQU   *                                                                
         MVC   0(1,R2),CHAR1                                                    
         MVC   CHAR1,CHAR2                                                      
         BCT   RE,DFLD010                                                       
         MVI   1(R2),C''''                                                      
         SR    RE,RE                                                            
         ICM   RE,3,FLDLEN                                                      
         LA    RE,2(RE)                                                         
         STCM  RE,3,FLDLEN                                                      
         B     DFLDOK                                                           
*                                                                               
DFLDERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(111)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
DFLDNO   B     NO                                                               
DFLDOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* CLEAR SQL STATEMENT BUFFER TO SPACES                               *          
**********************************************************************          
CLRSTMT  NTR1                                                                   
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         XC    STMTBUFF(2),STMTBUFF                                             
         LA    R0,STMTBUFF+2                                                    
         L     R1,=A(STMTBLEN)                                                  
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,=CL4' '                                                     
         MVCL  R0,RE                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT DSNTIAR MESSAGE BUFFER                                                  
**********************************************************************          
         SPACE 1                                                                
PRNTMSG  NTR1                                                                   
*                                                                               
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'SQL ERROR CODE: '                                    
         MVC   P+16(8),RCOUT                                                    
         GOTO1 DXUPRTL                                                          
*                                                                               
         MVC   P(40),=CL40'<<START OF SQLCA ERROR MESSAGE>>'                    
         GOTO1 DXUPRTL                                                          
         LA    R2,MESSAGE1                                                      
         SR    R3,R3                                                            
         ICM   R3,3,MESSAGEL                                                    
         AR    R3,R2                                                            
         ICM   R4,15,ARECL                                                      
PMSG010  EQU   *                                                                
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES                                                   
         BE    PMSG020                                                          
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R2)                                                       
         GOTO1 DXUPRTL                                                          
         AR    R2,R4                                                            
         CR    R3,R2                                                            
         BH    PMSG010                                                          
PMSG020  EQU   *                                                                
         MVC   P(40),=CL40'<<END OF SQLCA ERROR MESSAGE>>'                      
         GOTO1 DXUPRTL                                                          
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT DXUDB INPUT RECORD                                                      
*******************************************************************             
         SPACE 1                                                                
PRNTXREC NTR1                                                                   
         CLI   DXUPIFCD,DXUPIUPQ                                                
         BNE   PXRE030                                                          
         ICM   R3,15,DXUPAREC                                                   
         BZ    PXRE030                                                          
         SR    R2,R2                                                            
         ICM   R2,3,0(R3)                                                       
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'<<DXUDB INPUT RECORD REPORT>>'                       
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'DXUDB INPUT RECORD LENGTH='                          
         EDIT  (R2),(5,P+26),ZERO=NOBLANK,FILL=0,ALIGN=LEFT                     
         GOTO1 DXUPRTL                                                          
         LTR   R2,R2                                                            
         BZ    PXRE030                                                          
         SHI   R2,4                                                             
         LTR   R2,R2                                                            
         BZ    PXRE030                                                          
         LA    R3,4(R3)                                                         
         LR    R4,R3                                                            
         AR    R4,R2                                                            
         BCTR  R4,0                                                             
         LA    R5,20                                                            
PXRE010  EQU   *                                                                
         CR    R3,R4                                                            
         BH    PXRE030                                                          
         CHI   R2,132                                                           
         BL    PXRE020                                                          
         MVC   P(132),0(R3)                                                     
         GOTO1 DXUPRTL                                                          
         LA    R3,132(R3)                                                       
         SHI   R2,132                                                           
         BCT   R5,PXRE010                                                       
         B     PXRE030                                                          
PXRE020  EQU   *                                                                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         GOTO1 DXUPRTL                                                          
PXRE030  EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT SQL STATEMENT BUFFER                                                    
*******************************************************************             
         SPACE 1                                                                
PRNTSTMT NTR1                                                                   
         L     R3,=A(STMTBUFF)                                                  
         USING STMTBUFF,R3                                                      
         SR    R2,R2                                                            
         ICM   R2,3,0(R3)                                                       
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'<<DXUDB SQL STATEMENT REPORT>>'                      
         GOTO1 DXUPRTL                                                          
         MVC   P(40),=CL40'SQL STATEMENT LENGTH='                               
         EDIT  (R2),(5,P+21),ZERO=NOBLANK,FILL=0,ALIGN=LEFT                     
         GOTO1 DXUPRTL                                                          
         LTR   R2,R2                                                            
         BZ    PSTM030                                                          
         LA    R3,2(R3)                                                         
         LR    R4,R3                                                            
         AR    R4,R2                                                            
         BCTR  R4,0                                                             
         LA    R5,20                                                            
PSTM010  EQU   *                                                                
         CR    R3,R4                                                            
         BH    PSTM030                                                          
         CHI   RF,R2                                                            
         BL    PSTM020                                                          
         MVC   P(132),0(R3)                                                     
         GOTO1 DXUPRTL                                                          
         LA    R3,132(R3)                                                       
         SHI   R2,132                                                           
         BCT   R5,PSTM010                                                       
         B     PSTM030                                                          
PSTM020  EQU   *                                                                
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         GOTO1 DXUPRTL                                                          
PSTM030  EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* FORMAT DATE FIELD FROM DXDSECTS INTERNAL TO SQL COMPATIBLE         *          
* P1 - A(DATE FIELD 8 CHARACTER YYYYMMDD)                            *          
* P2 - A(OUTPUT FIELD ISO YYYY-MM-DD)                                *          
**********************************************************************          
FORMDATE NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(10,R3),SPACES                                                  
         MVC   0(4,R3),0(R2)                                                    
         MVI   4(R3),C'-'                                                       
         MVC   5(2,R3),4(R2)                                                    
         MVI   7(R3),C'-'                                                       
         MVC   8(2,R3),6(R2)                                                    
         B     FDATOK                                                           
*                                                                               
FDATNO   B     NO                                                               
FDATOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* REFORMAT DATE FIELD TYPE FOR DB2                                   *          
**********************************************************************          
REFDATE  NTR1                                                                   
         CLI   FLDNULL,C'Y'                                                     
         BE    RDATOK                                                           
         MVC   WORK(10),FLDVAL+1                                                
         MVC   FLDVAL+1(10),=CL10'2000-01-01'                                   
         MVC   FLDVAL+3(2),WORK+6                                               
         MVC   FLDVAL+6(2),WORK+3                                               
         MVC   FLDVAL+9(2),WORK                                                 
         MVI   FLDVAL,C''''                                                     
         MVI   FLDVAL+11,C''''                                                  
         LA    RE,12                                                            
         STCM  RE,3,FLDLEN                                                      
         B     RDATOK                                                           
*                                                                               
RDATNO   B     NO                                                               
RDATOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* FORMAT TIME FIELD FROM DXDSECTS INTERNAL TO SQL COMPATIBLE         *          
* P1 - A(TIME FIELD 8 CHARACTER HHMMSSTH)                            *          
* P2 - A(OUTPUT FIELD HH:MM:SS)                                      *          
**********************************************************************          
FORMTIME NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(8,R3),SPACES                                                   
         MVC   0(2,R3),0(R2)                                                    
         MVI   2(R3),C':'                                                       
         MVC   3(2,R3),2(R2)                                                    
         MVI   5(R3),C':'                                                       
         MVC   6(2,R3),4(R2)                                                    
         B     FTIMOK                                                           
*                                                                               
FTIMNO   B     NO                                                               
FTIMOK   B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* FORMAT SYSTEM FIELD FROM DXDSECTS INTERNAL TO SQL COMPATIBLE       *          
* P1 - A(SYSTEM NUMBER 1 BYTE)                                       *          
* P2 - A(SYSTEM NAME 7 CHARACTERS)                                   *          
**********************************************************************          
FORMSYS  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(7,R3),SPACES                                                   
         L     R4,=A(SYSLST)                                                    
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
*                                                                               
FSYS010  CLI   0(R4),0             TEST E-O-T                                   
         BE    FSYSERR1                                                         
         CLC   SYSLNUM,0(R2)                                                    
         BE    FSYS030                                                          
FSYS020  LA    R4,SYSLLEN(R4)                                                   
         B     FSYS010                                                          
*                                                                               
FSYS030  MVC   0(7,R3),SYSLNAME                                                 
         B     FSYSOK                                                           
*                                                                               
FSYSERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(951)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FSYSNO   B     NO                                                               
FSYSOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* FORMAT SUBSYSTEM FIELD FROM DXDSECTS INTERNAL TO SQL COMPATIBLE    *          
* P1 - A(SUB SYSTEM NUMBER 1 BYTE)                                   *          
* P2 - A(SUB SYSTEM NAME 7 CHARACTERS)                               *          
**********************************************************************          
FORMSUB  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(7,R3),SPACES                                                   
         L     R4,=A(SUBLST)                                                    
         USING SUBLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
*                                                                               
FSUB010  CLI   SUBLNUM,0           TEST E-O-T                                   
         BE    FSUBERR1                                                         
         CLC   SUBLNUM,0(R2)                                                    
         BE    FSUB030                                                          
FSUB020  LA    R4,SUBLLEN(R4)                                                   
         B     FSUB010                                                          
*                                                                               
FSUB030  MVC   0(7,R3),SUBLNAME                                                 
         B     FSUBOK                                                           
*                                                                               
FSUBERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(961)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
FSUBNO   B     NO                                                               
FSUBOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* CONVERT SQL RETURN CODES FOR OUTPUT                                *          
**********************************************************************          
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
* CHECK CAF RETURN CODES                                             *          
**********************************************************************          
CHEKCODE NTR1                                                                   
         MVC   CONTROL,CONTINUE                                                 
*        ********************* HUNT FOR FORCE OR ABTERM ***********             
         TM    TECB,X'40'         SEE IF TECB WAS POSTED (POSTBIT??)            
         BZ    DOCHECKS           BRANCH IF TECB WAS NOT POSTED                 
         CLC   TECBCODE(3),QUIESCE   IS THIS "STOP DB2 MODE=FORCE"              
         BE    DOCHECKS           IF NOT QUIESCE, WAS FORCE OR ABTE             
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
         MVC   P(40),=CL40'FOUND FORCE OR ABTERM, SHUTTING DOWN'                
         GOTO1 DXUPRTL                                                          
         B     BYEBYE             GO TO THE END OF CHEKCODE                     
DOCHECKS DS    0H                 EXAMINE RETCODE AND REASCODE                  
*        ********************* HUNT FOR 0 *************************             
         CLC   RETCODE,ZERO       WAS IT A ZERO?                                
         BE    BYEBYE             NOTHING TO DO IN CHEKCODE FOR ZER             
*        ********************* HUNT FOR 4 *************************             
         CLC   RETCODE,FOUR       WAS IT A 4?                                   
         BNE   HUNT8              IF NOT A 4, HUNT EIGHTS                       
         CLC   REASCODE,C10823    WAS IT A RELEASE LEVEL MISMATCH?              
         BNE   HUNT824            BRANCH IF NOT AN 823                          
         MVC   P(40),=CL40'MISMATCH BETWEEN DB2/CAF RELEASE LEVELS'             
         GOTO1 DXUPRTL                                                          
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
HUNT824  DS    0H                 NOW LOOK FOR 'CAF RESET' REASON C             
         CLC   REASCODE,C10824    WAS IT 4? ARE WE READY TO RESTART             
         BNE   UNRECOG            IF NOT 824, GOT UNKNOWN CODE                  
         MVC   P(40),=CL40'CAF IS NOW READY FOR MORE INPUT'                     
         GOTO1 DXUPRTL                                                          
         MVC   CONTROL,RESTART    INDICATE THAT WE SHOULD RE-CONNEC             
         B     BYEBYE             WE ARE DONE. GO TO END OF CHEKCOD             
UNRECOG  DS    0H                                                               
         MVC   P(40),=CL40'GOT RC=4 AND AN UNRECOGNIZED REASON CODE'            
         GOTO1 DXUPRTL                                                          
         MVC   CONTROL,SHUTDOWN   SHUTDOWN, SERIOUS PROBLEM                     
         B     BYEBYE             WE ARE DONE. GO TO END OF CH                  
*        ********************* HUNT FOR 8 ********************                  
HUNT8    DS    0H                                                               
         CLC   RETCODE,EIGHT      HUNT RETURN CODE OF 8                         
         BE    GOT8OR12                                                         
         CLC   RETCODE,TWELVE     HUNT RETURN CODE OF 12                        
         BNE   HUNT200                                                          
GOT8OR12 DS    0H                 FOUND RETURN CODE OF 8 OR 12                  
         MVC   P(40),=CL40'FOUND RETCODE OF 8 OR 12'                            
         GOTO1 DXUPRTL                                                          
         CLC   REASCODE,F30002    HUNT FOR X'00F30002'                          
         BE    DB2DOWN                                                          
         CLC   REASCODE,F30012    HUNT FOR X'00F30012'                          
         BE    DB2DOWN                                                          
         CLC   REASCODE,F30006    HUNT FOR X'00F30006'                          
         BE    BADSSYS                                                          
         MVC   P(40),=CL40'DB2 CONNECT FAIL WITH AN UNRECOG. REASON'            
         GOTO1 DXUPRTL                                                          
         CLC   SQLCODE,ZERO       SEE IF WE NEED TRANSLATE                      
         BNE   A4TRANS            IF NOT BLANK, SKIP TRANSLATE                  
*        ********************* TRANSLATE UNRECOGNIZED RETCODES ****             
         MVC   P(40),=CL40'SQLCODE 0 BUT RF NOT, SO TRANS. FOR SQLCODE'         
         GOTO1 DXUPRTL                                                          
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
         MVC   P(40),=CL40'NOT ABLE TO TRANSLT THE CONNECTION FAILURE'          
         GOTO1 DXUPRTL                                                          
         B     ENDCCODE           GO TO END OF CHEKCODE                         
A4TRANS  DS    0H                 SQLERRM MUST BE FILLED IN TO GET              
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
DB2DOWN  DS    0H                 HUNT RETURN CODE OF 200                       
         MVC   P(40),=CL40'DB2 IS DOWN - WILL TELL WHEN IT COMES UP'            
         GOTO1 DXUPRTL                                                          
         WAIT  ECB=SECB           WAIT FOR DB2 TO COME UP                       
         MVC   P(40),=CL40'DB2 IS NOW AVAILABLE'                                
         GOTO1 DXUPRTL                                                          
         MVC   CONTROL,RESTART    INDICATE THAT WE SHOULD RE-CONNEC             
         B     ENDCCODE                                                         
BADSSYS  DS    0H                  BAD DB2 SUB SYSTEM NAME IN OPEN              
         MVC   P(40),=CL40'DB2 SUB SYSTEM NAME IS INVALID'                      
         GOTO1 DXUPRTL                                                          
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
*        ********************* HUNT FOR 200 ***********************             
HUNT200  DS    0H                 HUNT RETURN CODE OF 200                       
         CLC   RETCODE,NUM200     HUNT 200                                      
         BNE   HUNT204                                                          
         MVC   P(40),=CL40'CAF FOUND USER ERROR, SEE DSNTRACE DATA SET'         
         GOTO1 DXUPRTL                                                          
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
*        ********************* HUNT FOR 204 ***********************             
HUNT204  DS    0H                 HUNT RETURN CODE OF 204                       
         CLC   RETCODE,NUM204     HUNT 204                                      
         BNE   WASSAT             IF NOT 204, GOT STRANGE CODE                  
         MVC   P(40),=CL40'CAF SYSTEM ERROR, SEE DSNTRACE DATA SET'             
         GOTO1 DXUPRTL                                                          
         B     ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
*        ********************* UNRECOGNIZED RETCODE ***************             
WASSAT   DS    0H                                                               
         MVC   P(40),=CL40'NON ZERO DB2 RETCODE=        /'                      
         GOTO1 =V(HEXOUT),PARM,RETCODE,P+21,4,=C'TOG'                           
         EDIT  RETCODE,(8,P+30),ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-                 
         GOTO1 DXUPRTL                                                          
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
         BE    ENDCCODE           WE ARE DONE. GO TO END OF CHEKCOD             
ENDCCODE DS    0H                 SHOULD WE SHUT DOWN?                          
         L     R4,RETCODE         GET A COPY OF THE RETCODE                     
         C     R4,FOUR            HAVE A LOOK AT THE RETCODE                    
         BNH   BYEBYE             IF RETCODE <= 4 THEN LEAVE CHEKCO             
         MVC   CONTROL,SHUTDOWN   SHUTDOWN                                      
         B     BYEBYE             IF RETCODE <= 4 THEN LEAVE CHEKCO             
*        ********************* UNRECOGNIZED RETCODE ***************             
BYEBYE   DS    0H                 WRAP UP AND LEAVE CHEKCODE                    
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE CALLS DSNTIAR                                                      
*******************************************************************             
         SPACE 1                                                                
CALLTIA1 NTR1                                                                   
*                                                                               
         L     RF,LITIAR                                                        
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),(SQLCA,MESSAGE,ARECL),MF=(E,PARM)                           
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE DSNHLI INTERCEPTS CALLS TO LI EP=DSNHLI                            
*******************************************************************             
         DS    0D                                                               
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
***********************************************************************         
* RUTINE TO DEAL WITH ERROR CONDITION                                 *         
***********************************************************************         
         SPACE 1                                                                
ERROUT   EQU   *                                                                
         LA    RF,ERRTAB                                                        
         MVC   ERRMSG,=CL132'DXUDB PROCESSING ERROR CODE: '                     
         EDIT  ERRCODE,(5,ERRMSG+30),ZERO=NOBLANK,FILL=0                        
EOUT010  EQU   *                                                                
         CLC   0(2,RF),=YL2(0)                                                  
         BE    EOUT030                                                          
         CLC   0(2,RF),ERRCODE                                                  
         BE    EOUT020                                                          
         LA    RF,L'ERRTAB(RF)                                                  
         B     EOUT010                                                          
EOUT020  EQU   *                                                                
         MVC   ERRMSG,2(RF)                                                     
EOUT030  EQU   *                                                                
         GOTO1 DXUPRTL                                                          
         MVC   P(L'ERRMSG),ERRMSG                                               
         GOTO1 DXUPRTL                                                          
         BAS   RE,PRNTXREC                                                      
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* DXUDB PRINT LINE ROUTINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DXUPRTL  NTR1                                                                   
         CLC   RETBLEFT,=AL2(132)                                               
         BL    DXCP100                                                          
         SR    RF,RF                                                            
         ICM   RF,3,RETBLEFT                                                    
         ICM   RE,15,RETBPTR                                                    
         MVC   0(132,RE),P                                                      
         LA    RE,132(RE)                                                       
         STCM  RE,15,RETBPTR                                                    
         SHI   RF,132                                                           
         STCM  RF,3,RETBLEFT                                                    
DXCP100  EQU   *                                                                
*        GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         XIT1                                                                   
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
         LA    RE,ARECL                                                         
         ST    RE,8(R1)                                                         
*                                                                               
         BASSM RE,RF              BRANCH TO DSNTIAR TO DO AN SQL CALL           
*                                 DSNTIARIN 31-BIT MODE, SO USE                 
*                                 BASSM TO ASSURE THAT THE ADDRESSING           
*                                 MODE IS PRESERVED.                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RD,4(,RD)          RESTORE RD (CALLER'S SAVE AREA)               
         L     RE,12(,RD)         RESTORE RE (RETURN ADDRESS)                   
         RETURN (1,12)            RESTORE R1-12, NOT R0 AND RF                  
         EJECT                                                                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REDEFINE ADDRESSABILTY BEYOND HERE                                  *         
***********************************************************************         
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*SQL DESCRIPTOR AREAS - MUST APPEAR BEFORE THEY ARE REFERENCED        *         
***********************************************************************         
                                                                                
         DS    0D                                                               
SQDDA1   EXEC  SQL INCLUDE SQLDA                                                
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE TABLE RELOAD BY DELETING ALL TABLE ENTRIES               *         
***********************************************************************         
         DS    0D                                                               
CLRTABLE NTR1  BASE=*                                                           
         BAS   RE,CLRSTMT                                                       
         L     R3,=A(STMTBUFF)                                                  
         LA    R3,2(R3)                                                         
         MVC   0(40,R3),=CL40'DELETE FROM '                                     
         LA    R3,12(R3)                                                        
         ICM   R4,15,=AL4(STMTBLEN)                                             
         SHI   R4,12                                                            
         GOTO1 LOADTEXT,PARM,TABFNAME,L'TABFNAME,(R3),(R4)                      
         BNE   CTABNO                                                           
         ICM   R3,15,0(R1)                                                      
         ICM   RF,15,4(R1)                                                      
         SR    R4,RF                                                            
         ICM   RF,15,=AL4(STMTBLEN)                                             
         SR    RF,R4                                                            
         L     RE,=A(STMTBUFF)                                                  
         STCM  RF,3,0(RE)                                                       
         MVI   NOTFOUND,C'Y'                                                    
         GOTO1 =A(EXECSQL)                                                      
         BNE   CTABNO                                                           
         B     CTABOK                                                           
*                                                                               
CTABNO   B     NO                                                               
CTABOK   B     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET EXTRACT RECORD ID INFO FROM DXCAUTH.RECORD TABLE               *          
**********************************************************************          
         DS    0D                                                               
GETXID   NTR1  BASE=*                                                           
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
         MVC   2(28,R2),=CL28'SELECT TABLENAME, INDEXNAME '                     
         MVC   30(20,R2),=CL20'FROM DXCAUTH.RECORD '                            
         MVC   50(17,R2),=CL17'WHERE RECORDID=? '                               
         MVC   67(17,R2),=CL17'AND COUNTRYCODE=?'                               
         LA    RF,84                                                            
         STCM  RF,3,STMTBUFF                                                    
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL DECLARE CGI CURSOR FOR STMTGI                                
         CLC   SQLCODE,ZERO                                                     
         BNE   GXIDERR2                                                         
*                                                                               
         EXEC  SQL PREPARE STMTGI FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BNE   GXIDERR3                                                         
*                                                                               
         EXEC  SQL OPEN CGI USING :THISXID, :CUNTSQL                            
         CLC   SQLCODE,ZERO                                                     
         BNE   GXIDERR4                                                         
*                                                                               
         EXEC  SQL FETCH CGI INTO :THISTBNM, :THISNDXN                          
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   GXIDERR1                                                         
*                                                                               
         EXEC  SQL CLOSE CGI                                                    
         B     GXIDOK                                                           
*                                                                               
GXIDERR1 EQU   *                                                                
         EXEC  SQL CLOSE CGI                                                    
         MVC   P(40),=CL40'GETXID ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(181)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
GXIDERR2 EQU   *                                                                
         MVC   P(40),=CL40'GETXID ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(901)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
GXIDERR3 EQU   *                                                                
         MVC   P(80),=CL80'GETXID ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(902)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
GXIDERR4 EQU   *                                                                
         MVC   P(80),=CL80'GETXID ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(903)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
GXIDNO   B     NO                                                               
GXIDOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INSERT INTO EXTRACT DATA LOG                                       *          
**********************************************************************          
         DS    0D                                                               
INSXLOG  NTR1  BASE=*                                                           
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
         MVC   2(35,R2),=CL35'INSERT INTO DXCAUTH.EXTRACT VALUES '              
         MVC   37(33,R2),=CL33'(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'               
         LA    RF,70                                                            
         STCM  RF,3,STMTBUFF                                                    
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL PREPARE STMTIX FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BNE   INXLERR2                                                         
*                                                                               
         EXEC  SQL EXECUTE STMTIX USING                                X        
               :HVXAGY,                                                X        
               :HVXSYS,                                                X        
               :HVXSUB,                                                X        
               :HVXDATE,                                               X        
               :HVXTIME,                                               X        
               :HVXSNUM,                                               X        
               :HVXJOB,                                                X        
               :HVXJDATE,                                              X        
               :HVXJTIME,                                              X        
               :HVXTYPE,                                               X        
               :HVXMODE,                                               X        
               :HVXRCNT,                                               X        
               :HVXBCNT,                                               X        
               :HVXMCNT,                                               X        
               :HVXFDA,                                                X        
               :HVXTDA                                                          
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   INXLERR1                                                         
         B     INXLOK                                                           
*                                                                               
INXLERR1 EQU   *                                                                
         MVC   P(80),=CL80'INSDXL ERROR'                                        
         GOTO1 DXUPRTL                                                          
         LA    RE,L'HVXDSN+(HVXDSN-HVXAGY)                                      
         LA    RF,132                                                           
         CR    RF,RE                                                            
         BL    *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HVXAGY                                                      
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(451)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
INXLERR2 EQU   *                                                                
         MVC   P(80),=CL80'INSDXL ERROR'                                        
         GOTO1 DXUPRTL                                                          
         LA    RE,L'HVXDSN+(HVXDSN-HVXAGY)                                      
         LA    RF,132                                                           
         CR    RF,RE                                                            
         BL    *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HVXAGY                                                      
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(452)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
INXLNO   B     NO                                                               
INXLOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INSERT INTO UPDATE EVENT LOG VALUES                                *          
**********************************************************************          
         DS    0D                                                               
INSULOG  NTR1  BASE=*                                                           
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
         MVC   2(34,R2),=CL34'INSERT INTO DXCAUTH.UPDATE VALUES '               
         MVC   36(31,R2),=CL31'(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'                 
         LA    RF,67                                                            
         STCM  RF,3,STMTBUFF                                                    
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL PREPARE STMTIU FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BNE   INULERR2                                                         
*                                                                               
         EXEC  SQL EXECUTE STMTIU USING                                X        
               :HVXLOCN,                                               X        
               :HVXAUTH,                                               X        
               :HVXUDATE,                                              X        
               :HVXUTIME,                                              X        
               :HVXAGY,                                                X        
               :HVXSYS,                                                X        
               :HVXSUB,                                                X        
               :HVXDATE,                                               X        
               :HVXTIME,                                               X        
               :HVXJOB,                                                X        
               :HVXJDATE,                                              X        
               :HVXJTIME,                                              X        
               :HVXEC,                                                 X        
               :HVXRC,                                                 X        
               :HVXRS                                                           
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   INULERR1                                                         
         B     INULOK                                                           
*                                                                               
INULERR1 EQU   *                                                                
         MVC   P(80),=CL80'INSDXL ERROR'                                        
         GOTO1 DXUPRTL                                                          
         LA    RE,L'HVXDSN+(HVXDSN-HVXAGY)                                      
         LA    RF,132                                                           
         CR    RF,RE                                                            
         BL    *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HVXAGY                                                      
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(451)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
INULERR2 EQU   *                                                                
         MVC   P(80),=CL80'INSDXL ERROR'                                        
         GOTO1 DXUPRTL                                                          
         LA    RE,L'HVXDSN+(HVXDSN-HVXAGY)                                      
         LA    RF,132                                                           
         CR    RF,RE                                                            
         BL    *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HVXAGY                                                      
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(452)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
INULNO   B     NO                                                               
INULOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET NUMBER OF KEY FIELDS FOR THIS TABLE INDEX FROM SYSIBM.SYSINDEX *          
**********************************************************************          
         DS    0D                                                               
NUMKEYS  NTR1  BASE=*                                                           
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
         MVC   2(39,R2),=CL39'SELECT COLCOUNT FROM SYSIBM.SYSINDEXES '          
         MVC   41(13,R2),=CL13'WHERE NAME=? '                                   
         MVC   54(13,R2),=CL13'AND TBNAME=? '                                   
         MVC   67(13,R2),=CL13'AND CREATOR=?'                                   
         LA    RF,78                                                            
         STCM  RF,3,STMTBUFF                                                    
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL DECLARE CNK CURSOR FOR STMTNK                                
         CLC   SQLCODE,ZERO                                                     
         BNE   NKEYERR2                                                         
*                                                                               
         EXEC  SQL PREPARE STMTNK FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BNE   NKEYERR3                                                         
*                                                                               
         EXEC  SQL OPEN CNK USING :THISNDXN, :THISTBNM, :THISAUTH               
         CLC   SQLCODE,ZERO                                                     
         BNE   NKEYERR4                                                         
*                                                                               
         EXEC  SQL FETCH CNK INTO :THISKNUM                                     
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
*        CLC   SQLCODE,NUM100      SEE IF SQLCODE=100                           
*        BE    NKEYOK                                                           
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   NKEYERR1                                                         
*                                                                               
         EXEC  SQL CLOSE CNK                                                    
*                                                                               
         B     NKEYOK                                                           
*                                                                               
NKEYERR1 EQU   *                                                                
         EXEC  SQL CLOSE CNK                                                    
         MVC   P(80),=CL80'NUMKEY ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(171)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
NKEYERR2 EQU   *                                                                
         MVC   P(80),=CL80'NUMKEY ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(172)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
NKEYERR3 EQU   *                                                                
         MVC   P(80),=CL80'NUMKEY ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(173)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
NKEYERR4 EQU   *                                                                
         MVC   P(80),=CL80'NUMKEY ERROR ON RECORD ID: '                         
         MVC   P+27(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(174)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
NKEYNO   B     NO                                                               
NKEYOK   B     YES                                                              
         DROP  R2                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXECUTE SQL STATEMENT IN STMTBUFF                                   *         
***********************************************************************         
         DS    0D                                                               
EXECSQL  NTR1  BASE=*                                                           
*                                                                               
         L     R3,=A(WSSQL)        NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
*                                                                               
         EXEC  SQL EXECUTE IMMEDIATE :STMTBUFF                                  
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLI   NOTFOUND,C'N'                                                    
         BE    *+14                                                             
         CLC   SQLCODE,NUM100      SEE IF SQLCODE=100                           
         BE    ESQLOK                                                           
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   ESQLERR1                                                         
*                                                                               
         B     ESQLOK                                                           
*                                                                               
ESQLERR1 EQU   *                                                                
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         BAS   RE,PRNTSTMT                                                      
         MVC   ERRCODE,=YL2(999)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
ESQLNO   B     NO                                                               
ESQLOK   B     YES                                                              
         DROP  R3,R2                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DESCRIBE TABLE INTO LOCAL SQLDA                                     *         
***********************************************************************         
         DS    0D                                                               
DESCTAB  NTR1  BASE=*                                                           
         L     R3,=A(WSSQL)         NEED W/S TO BUILD SQL PARAM LIST            
         USING SQLDSECT,R3                                                      
*                                                                               
         L     R4,=A(WSSQLD)                                                    
         USING SQLDA,R4                                                         
         MVC   SQLDAID,=CL8'SQLDA'                                              
         MVC   SQLDABC,=AL4(L'WSSQLD)                                           
         LH    RF,NUMSQLV                                                       
         STH   RF,SQLN                                                          
         MVC   SQLD,=H'0'                                                       
         LA    R1,SQLVAR                                                        
         USING SQLVARN,R1                                                       
*                                                                               
DTAB010  EQU   *                                                                
         LTR   RF,RF                                                            
         BZ    DTAB020                                                          
         MVC   SQLTYPE,=H'0'                                                    
         MVC   SQLLEN,=H'0'                                                     
         MVC   SQLNAME,SPACES                                                   
         LA    R1,SQLSIZV(R1)                                                   
         BCT   RF,DTAB010                                                       
         DROP  R1                                                               
*                                                                               
DTAB020  EQU   *                                                                
*                                                                               
         DROP  R4                                                               
         USING WSSQLD,R4                                                        
*                                                                               
         EXEC  SQL DESCRIBE TABLE :TABFNAME INTO :WSSQLD                        
*                                                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   DTABERR1                                                         
*                                                                               
         B     DTABOK                                                           
*                                                                               
DTABERR1 EQU   *                                                                
         MVC   P(80),=CL80'DESCTAB ERROR ON RECORD ID: '                        
         MVC   P+28(5),THISXID                                                  
         GOTO1 DXUPRTL                                                          
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(201)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
DTABNO   B     NO                                                               
DTABOK   B     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EXECUTE SQL COMMIT STATEMENT                                       *          
**********************************************************************          
         DS    0D                                                               
COMMSQL  NTR1  BASE=*                                                           
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL COMMIT                                                       
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   COMMERR1                                                         
         B     COMMOK                                                           
*                                                                               
COMMERR1 EQU   *                                                                
         MVC   P(80),=CL80'EXEC COMMIT SQL ERROR'                               
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(191)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
COMMNO   B     NO                                                               
COMMOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EXECUTE SQL SET PACKAGE STATEMENT FOR DXUDB                        *          
**********************************************************************          
         DS    0D                                                               
SETPACK  NTR1  BASE=*                                                           
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL SET CURRENT PACKAGESET='DXUDB'                               
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   SPACERR1                                                         
         B     SPACOK                                                           
*                                                                               
SPACERR1 EQU   *                                                                
         MVC   P(80),=CL80'EXEC SETPACK SQL ERROR'                              
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(954)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
SPACNO   B     NO                                                               
SPACOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EXECUTE SQL CONNECT TO LOCATION STATEMENT FOR REMOTE SERVER IF ANY *          
**********************************************************************          
         DS    0D                                                               
CONLOCN  NTR1  BASE=*                                                           
*                                                                               
         CLC   THISLOCN,SPACES                                                  
         BE    CONLOK                                                           
         OC    THISLOCN,THISLOCN                                                
         BZ    CONLOK                                                           
         MVC   HVXLOCN,THISLOCN                                                 
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL CONNECT TO :HVXLOCN                                          
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   CONLERR1                                                         
         B     CONLOK                                                           
*                                                                               
CONLERR1 EQU   *                                                                
         MVC   P(80),=CL80'EXEC CONNECT LOCATION SQL ERROR'                     
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(765)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
CONLNO   B     NO                                                               
CONLOK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EXECUTE SQL CONNECT RESET STATEMENT TO CONNECT TO LOCAL SERVER     *          
**********************************************************************          
         DS    0D                                                               
CONRESET NTR1  BASE=*                                                           
*                                                                               
         CLC   THISLOCN,SPACES                                                  
         BE    CONROK                                                           
         OC    THISLOCN,THISLOCN                                                
         BZ    CONROK                                                           
         MVC   HVXLOCN,THISLOCN                                                 
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL CONNECT RESET                                                
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   CONRERR1                                                         
         B     CONROK                                                           
*                                                                               
CONRERR1 EQU   *                                                                
         MVC   P(80),=CL80'EXEC CONNECT RESET ERROR'                            
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(764)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
CONRNO   B     NO                                                               
CONROK   B     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* EXECUTE SQL ROLLBACK STATEMENT                                     *          
**********************************************************************          
         DS    0D                                                               
RBCKSQL  NTR1  BASE=*                                                           
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL ROLLBACK                                                     
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   RBCKERR1                                                         
         B     RBCKOK                                                           
*                                                                               
RBCKERR1 EQU   *                                                                
         MVC   P(80),=CL80'EXEC ROLLBACK SQL ERROR'                             
         GOTO1 DXUPRTL                                                          
         MVC   ERRCODE,=YL2(791)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
RBCKNO   B     NO                                                               
RBCKOK   B     YES                                                              
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS BUILD DXTRACT RECOVERY LOG INFORMATION                     *          
**********************************************************************          
PROCBRC  NTR1  BASE=*                                                           
         ICM   R3,15,DXUPADXB                                                   
         BZ    PBRCERR1                                                         
         USING DXBLOCKD,R3                                                      
         ICM   R4,15,DXSTPTR                                                    
         USING SXDTABD,R4                                                       
         BZ    PBRCERR2                                                         
*                                                                               
         MVC   HVXAGY,SXDTAGY                                                   
         GOTO1 FORMSYS,PARM,SXDTSYS,HVXSYS                                      
         BNE   PBRCNO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   PBRCNO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   PBRCNO                                                           
         XC    HVXRDATE,HVXRDATE                                                
         XC    HVXRTIME,HVXRTIME                                                
         XC    HVXRDA,HVXRDA                                                    
*                                                                               
         GOTO1 =A(CONRESET)        CONNECT RESET TO LOCAL SERVER                
*                                                                               
         BAS   RE,GETXRC                                                        
         BNE   PBRCNO                                                           
*                                                                               
         GOTO1 =A(CONLOCN)         RECONNECT TO REMOTE DB2 LOCATION             
*                                                                               
*                                                                               
*        CONVERT HVXRDAT/TIM/DA INTO BINARY                                     
*                                                                               
         ICM   R5,15,SXDTDBXT                                                   
         USING SXDBEXTD,R5                                                      
         MVC   SXDBRDA,HVXRDA                                                   
*        MVC   SXDBRDAT,HVXRDATE                                                
*        MVC   SXDBRTIM,HVXRTIME                                                
         B     PBRCOK                                                           
*                                                                               
PBRCERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(551)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PBRCERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(552)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PBRCERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(553)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PBRCOK   SR    RC,RC                                                            
PBRCNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* GET EXTRACT RECOVERY LOG INFO FROM DXCAUTH.SOURCE TABLE            *          
**********************************************************************          
         DS    0D                                                               
GETXRC   NTR1                                                                   
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
   MVC 2(46,R2),=CL46'SELECT RECOVERYDATE, RECOVERYTIME, RECOVERYDA '           
         MVC   30(20,R2),=CL20'FROM DXCAUTH.SOURCE '                            
         MVC   50(19,R2),=CL19'WHERE AGENCYCODE=? '                             
         MVC   69(13,R2),=CL13'AND SYSTEM=? '                                   
         MVC   82(15,R2),=CL15'AND SUBSYSTEM=?'                                 
         LA    RF,97                                                            
         STCM  RF,3,STMTBUFF                                                    
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
*                                                                               
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL DECLARE CGR CURSOR FOR STMTGR                                
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EXEC  SQL PREPARE STMTGR FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EXEC  SQL OPEN CGR USING :HVXAGY, :HVXSYS, :HVXSUB                     
         CLC   SQLCODE,ZERO                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EXEC  SQL FETCH CGR INTO :HVXRDATE, :HVXRTIME, :HVXRDA                 
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   SQLCODE,NUM100      SEE IF SQLCODE=100                           
         BE    GXRCOK                                                           
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   GXRCERR1                                                         
         EXEC  SQL CLOSE CGR                                                    
         B     GXRCOK                                                           
*                                                                               
GXRCERR1 EQU   *                                                                
         EXEC  SQL CLOSE CGR                                                    
         MVC   P(80),=CL80'GETXRC ERROR ON SUBSYSTEM: '                         
         MVC   P+27(2),HVXAGY                                                   
         MVC   P+30(7),HVXSYS                                                   
         MVC   P+38(7),HVXSUB                                                   
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(461)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
GXRCOK   SR    RC,RC                                                            
GXRCNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PROCESS WRITE DXTRACT RECOVERY LOG INFORMATION                     *          
**********************************************************************          
PROCWRC  NTR1  BASE=*                                                           
         ICM   R3,15,DXUPADXB                                                   
         BZ    PWRCERR1                                                         
         USING DXBLOCKD,R3                                                      
         ICM   R4,15,DXSTPTR                                                    
         USING SXDTABD,R4                                                       
         BZ    PWRCERR2                                                         
         MVC   HVXAGY,SXDTAGY                                                   
         GOTO1 FORMSYS,PARM,SXDTSYS,HVXSYS                                      
         BNE   PWRCNO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   PWRCNO                                                           
         GOTO1 FORMSUB,PARM,SXDTSUB,HVXSUB                                      
         BNE   PWRCNO                                                           
*                                                                               
         ICM   R5,15,SXDTDBXT                                                   
         USING SXDBEXTD,R5                                                      
         MVC   HVXRDA,SXDBRDA                                                   
         MVC   HVXRDATE,=CL10'2000-01-01'                                       
         MVC   HVXRTIME,=CL8'01:01:01'                                          
*        MVC   HVXRTIME,SXDBRTIM                                                
*        MVC   HVXRDATE,SXDBRDAT                                                
*                                                                               
         BAS   RE,UPDXRC                                                        
         BNE   PWRCNO                                                           
         B     PWRCOK                                                           
*                                                                               
PWRCERR1 EQU   *                                                                
         MVC   ERRCODE,=YL2(561)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PWRCERR2 EQU   *                                                                
         MVC   ERRCODE,=YL2(562)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PWRCERR3 EQU   *                                                                
         MVC   ERRCODE,=YL2(563)                                                
         BAS   RE,ERROUT                                                        
*                                                                               
PWRCOK   SR    RC,RC                                                            
PWRCNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* UPDATE EXTRACT RECOVERY LOG INFO IN DXCAUTH.SOURCE TABLE           *          
**********************************************************************          
         DS    0D                                                               
UPDXRC   NTR1                                                                   
         L     R2,=A(STMTBUFF)                                                  
         USING STMTBUFF,R2                                                      
         BAS   RE,CLRSTMT                                                       
         MVC   2(22,R2),=CL22'UPDATE DXCAUTH.SOURCE '                           
         MVC   24(19,R2),=CL19'SET RECOVERYDATE=? '                             
         MVC   43(15,R2),=CL15'RECOVERYTIME=? '                                 
         MVC   58(13,R2),=CL13'RECOVERYDA=? '                                   
         MVC   71(19,R2),=CL19'WHERE AGENCYCODE=? '                             
         MVC   90(13,R2),=CL13'AND SYSTEM=? '                                   
         MVC   103(15,R2),=CL15'AND SUBSYSTEM=?'                                
         LA    RF,118                                                           
         STCM  RF,3,STMTBUFF                                                    
*                                                                               
         L     R3,=A(WSSQL)                                                     
         USING SQLDSECT,R3                                                      
         MVC   SQLCODE,ZERO        CLEAR SQLCODE FOR ERROR CHECKING             
*                                                                               
         EXEC  SQL PREPARE STMTUR FROM :STMTBUFF                                
         CLC   SQLCODE,ZERO                                                     
         BNE   UXRCERR2                                                         
*                                                                               
         EXEC  SQL EXECUTE STMTUR USING                                X        
               :HVXRDATE,                                              X        
               :HVXRTIME,                                              X        
               :HVXRDA,                                                X        
               :HVXAGY,                                                X        
               :HVXSYS,                                                X        
               :HVXSUB                                                          
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
*                                                                               
         ST    RF,RETCODE          SAVE THE RETURN CODES                        
         ST    R0,REASCODE                                                      
         BAS   RE,CHEKCODE         CHECK THE RETURN AND REASON CODES            
*                                                                               
         CLC   SQLCODE,NUM100      SEE IF SQLCODE=100                           
         BE    UXRCOK                                                           
*                                                                               
         CLC   CONTROL,CONTINUE    EVERYTHING OK?                               
         BNE   UXRCERR1                                                         
         B     UXRCOK                                                           
*                                                                               
UXRCERR1 EQU   *                                                                
         MVC   P(80),=CL80'UPDXRC ERROR ON SUBSYSTEM: '                         
         MVC   P+27(2),HVXAGY                                                   
         MVC   P+30(7),HVXSYS                                                   
         MVC   P+38(7),HVXSUB                                                   
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(671)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
UXRCERR2 EQU   *                                                                
         MVC   P(80),=CL80'UPDXRC ERROR ON SUBSYSTEM: '                         
         MVC   P+27(2),HVXAGY                                                   
         MVC   P+30(7),HVXSYS                                                   
         MVC   P+38(7),HVXSUB                                                   
         GOTO1 DXUPRTL                                                          
         BAS   RE,PSQLERR                                                       
         BAS   RE,CALLTIAR                                                      
         BAS   RE,PRNTMSG                                                       
         MVC   ERRCODE,=YL2(672)                                                
         BAS   RE,ERROUT                                                        
         DC    H'0'                                                             
*                                                                               
UXRCOK   SR    RC,RC                                                            
UXRCNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PROGRAM CONSTANTS DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
DXUDBC   EQU   *                                                                
       ++INCLUDE FASYSLST                                                       
         SPACE  1                                                               
       ++INCLUDE DXSUBLST                                                       
         DS    0D                                                               
ESCHAR   EQU   C''''               DB2 STRING ESCAPE CHARACTER                  
         DS    0D                                                               
COLDSCN  DC    A(CDSCEN)                                                        
*                                                                               
SECB     DC    F'0'                DB2 START-UP ECB                             
TECB     DC    F'0'                DB2 TERMINATION ECB                          
         ORG   TECB+1              DB2 TERMINATION ECB                          
TECBCODE DS    XL3                                                              
LIALI    DC    A(0)                DSNALI ENTRY POINT ADDRESS                   
LISQL    DC    A(0)                DSNHLI2 ENTRY POINT ADDRESS                  
LITIAR   DC    A(0)                DSNTIAR ENTRY POINT ADDRESS                  
SSID     DS    CL4                 DB2 SUBSYSTEM ID. CONNECT PARAMETER          
PLAN     DS    CL8                 DB2 PAN NAME. OPEN PARAMETER                 
TRMOP    DS    CL4                 CLOSE TERMINATION OPTION (SYNC/ABRT)         
         SPACE 1                                                                
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
ERRTAB   DS    0CL134              ERROR REPORT STRINGS                         
         DC    AL2(999),CL132'SQL ERROR RETURN FROM EXEC STATEMENT'             
         DC    AL2(803),CL132'CAF LOAD MODULE ERROR'                            
         DC    AL2(0)                                                           
         SPACE 1                                                                
*                                                                               
NUMSQLV  DC    H'200'                                                           
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
NUM100   DC    F'100'              100 (SQL ERROR - ROW NOT FOUND)              
NUM200   DC    F'200'              200 (USER ERROR)                             
NUM204   DC    F'204'              204 (CALL ATTACH SYSTEM ERROR)               
NUMN104  DC    F'-104'             -104 (EXEC SQL SYNTAX ERROR)                 
*                                                                               
*                                  REASON CODES (R0) FROM CALL ATTACH           
C10205   DC    XL4'00C10205'       CALL ATTACH COULD NOT TRANSLATE              
C10823   DC    XL4'00C10823'       CALL ATTACH FOUND A RELEAS MISSMATCH         
C10824   DC    XL4'00C10824'       CALL ATTACH READY FOR MORE INPUT             
F30002   DC    XL4'00F30002'       DB2 SUBSYSTEM NOT UP                         
F30006   DC    XL4'00F30006'       DB2 SUBSYSTEM NAME INVALID                   
F30011   DC    XL4'00F30011'       DB2 SUBSYSTEM NOT UP                         
F30012   DC    XL4'00F30012'       DB2 SUBSYSTEM NOT UP                         
F30025   DC    XL4'00F30025'       DB2 IS STOPPOING (REASCODE)                  
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
*                                  SQL ERROR FORMATTER "DSNTIAR"                
*                                  MESSAGE WORK AREAS                           
LINES    EQU   10                                                               
ARECL    DC    AL4(LRECL)                                                       
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
         EJECT                                                                  
**********************************************************************          
* PROGRAM VARIABLES DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
WRKD     DSECT                                                                  
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
PARM     DS    6A                                                               
*                                                                               
APLIST   DS    A                   A(PARAMETER LIST)                            
*                                                                               
DXUDBPAR DS    0XL(DXUDBPLQ)       DXUDB PARAMETER BLOCK                        
       ++INCLUDE DXUDBP                                                         
*                                                                               
FULL     DS    F                                                                
RCOUT    DS    XL8                                                              
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
R0SAVE   DS    F                                                                
RESAVE   DS    F                                                                
RFSAVE   DS    F                                                                
*                                                                               
ERRCODE  DS    XL2                 ERROR CODE NUMBER                            
ERRCTRL  DS    XL1                 ERROR CONTROL FLAGS                          
ERRMSG   DS    CL132               ERROR MESSAGE WORK AREA                      
WORK     DS    CL255               GENERAL WORK AREA                            
CHAR1    DS    C                   DINKFLD ROUTINE SAVE CHARACTER 1             
CHAR2    DS    C                   DINKFLD ROUTINE SAVE CHARACTER 2             
*                                                                               
COUNTRY  DS    XL1                 COUNTRY CODE                                 
CUNTSQL  DS    HL2                 COUNTRY CODE FOR SQL SMALLINT FIELD          
*                                                                               
*                                  HOST VARIABLES FOR DXDSECTS FIELDS           
HVXVARS  DS    0D                                                               
HVXDBID  DS    CL30                                                             
HVXAUTH  DS    CL8                                                              
HVXLOCN  DS    CL16                                                             
HVXAGY   DS    CL2                                                              
HVXSYS   DS    CL7                                                              
HVXSUB   DS    CL7                                                              
HVXSEN   DS    CL7                                                              
HVXDATE  DS    CL10                                                             
HVXTIME  DS    CL8                                                              
HVXUDATE DS    CL10                                                             
HVXUTIME DS    CL8                                                              
         DS    0H                                                               
HVXSNUM  DS    HL2                                                              
HVXJOB   DS    CL8                                                              
HVXJDATE DS    CL10                                                             
HVXJTIME DS    CL8                                                              
HVXTYPE  DS    CL3                                                              
HVXMODE  DS    CL1                                                              
HVXRCNT  DS    PL8                                                              
HVXBCNT  DS    PL8                                                              
HVXMCNT  DS    PL8                                                              
         DS    0F                                                               
HVXFDA   DS    FL4                                                              
HVXTDA   DS    FL4                                                              
         DS    0H                                                               
HVXEC    DS    HL2                                                              
         DS    0F                                                               
HVXRC    DS    FL4                                                              
HVXRS    DS    FL4                                                              
HVXDSN   DS    CL44                                                             
HVXRDATE DS    CL10                                                             
HVXRTIME DS    CL8                                                              
HVXRDA   DS    FL4                                                              
*                                  EXTRACT RECORD FIELD VALUES                  
HVXSTLEN EQU   *-HVXVARS                                                        
*                                  EXTRACT RECORD FIELD VALUES                  
FLDNUM   DS    XL2                 FIELD NUMBER                                 
FLDCNT   DS    XL2                 FIELD COUNT                                  
FLDPOS   DS    AL4                 ADDRESS FIELD POSITION IN RECORD             
FLDLEN   DS    XL2                 FIELD LENGTH                                 
FLDVAL   DS    CL1000              FIELD VALUE DATA BUFFER                      
FLDLAST  DS    CL1                 LAST FIELD IN RECORD FLAG=Y                  
FLDTYPE  DS    XL2                 FILED DATA TYPE CODE FROM SQLCA              
FLDSPACS DS    C                   FILED CONTAINS SPACES FLAG=Y                 
FLDNULL  DS    C                   FIELD IS NULL FLAG=Y                         
FLDSQLD  DS    XL4                 SQLDATA VALUE                                
FLDHEX   DS    C                   FIELD IS CHAR HEX BIT TYPE                   
RECTERM  DS    C                   RECORD TERMINATOR PRESENT                    
*                                                                               
THISXID  DS    CL5                 THIS EXTRACT RECORD ID                       
THISXACT DS    CL1                 THIS EXTRACT RECORD ACTION CODE              
THISTBNM DS    CL18                THIS TABLE BASE NAME                         
THISDBID DS    CL30                THIS DATABASE CONNECT ID                     
THISAUTH DS    CL8                 THIS DATABASE AUTH (SQL) ID                  
THISLOCN DS    CL16                THIS DATABASE LOCATION                       
THISNDXN DS    CL18                THIS TABLE INDEX NAME                        
THISKNUM DS    HL2                 NUMBER OF KEYS IN CURRENT TABLE              
NOTFOUND DS    CL1                 FLAG IGNORE SQL "NOT FOUND" WARNING          
*                                                                               
TABFNAME DS    CL30                DATABASE TABLE FULLY QUALIFIED NAME          
TNDXNAME DS    CL30                DB TABLE INDEX FULLY QUALIFIED NAME          
*                                                                               
RETBPTR  DS    A                   A(CURRENT END OF RETURN STATE BLOCK)         
RETBLEFT DS    XL2                 LENGTH OF RETURN STATE BLOCK LEFT            
*                                                                               
ACOLDSC  DS    A                   A(COLDSC)                                    
ACOLDSCX DS    A                   A(COLDSCX)                                   
ACOLDSCP DS    A                   A(COLDSCP)                                   
*                                                                               
ATABDSC  DS    A                   A(TABDSC)                                    
ATABDSCX DS    A                   A(TABDSCX)                                   
ATABDSCP DS    A                   A(TABDSCP)                                   
*                                                                               
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
       ++INCLUDE DXDSECTS                                                       
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         SPACE 2                                                                
       ++INCLUDE DXSUBLSTD                                                      
         SPACE 2                                                                
       ++INCLUDE GEGENXTR                                                       
         SPACE 2                                                                
         PRINT ON                                                               
*                                                                               
         CSECT                                                                  
         DS    0D                  EXEC SQL STATEMENT BUFFER                    
         DS    HL2                 FOR CALLERS PUT LENGTH                       
STMTBUFF DS    HL2,CL20000                                                      
STMTBLEN EQU   20000                                                            
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
WSSQLD   DS    CL20000             SQL DESCRIPTOR AREA (SQLDA)                  
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
WSSQL1D  DS    CL20000             SQL DESCRIPTOR AREA (SQLDA)                  
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
WSSQL    DS    (SQLDLEN)C          EXEC SQL PARAMETER LIST (SQLDSECT)           
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
         DC    C'**TABDSC'                                                      
         CNOP  2,4                                                              
TABDSC   DS    0H                  DATABASE TABLE DESCRIPTOR LIST               
TDSCEN   DC    (TDSCMAX*TDSCLQ)X'00'                                            
TABDSCX  EQU   *                                                                
TDSCMAX  EQU   100                                                              
*                                                                               
TABDSCD  DSECT                     DSECT FOR TABDSC                             
TDSCID   DS    CL5                 EXTRACT RECORD TABLE ID                      
TDSCTABN DS    CL18                DB2 TABLE BASE NAME                          
TDSCNDXN DS    CL18                DB2 TABLE INDEX BASE NAME                    
TDSCKNUM DS    HL2                 NUMBER OF COLUMN KEY FIELDS                  
TDSCNUM  DS    XL2                 NUMBER OF COLUMN DESCRIPTOR ENTRIES          
TDSCFLG1 DS    XL1                 CONTROL FLAG BYTE 1                          
TDSCFCTQ EQU   X'01'               CLEARED TABLE ON RELOAD                      
TDSCASTR DS    AL4                 A(START OF COLUMN DESCRIPTOR ENTRY)          
TDSCAEND DS    AL4                 A(END OF COLUMN DEXCRIPTOR ENTRY)            
TDSCLQ   EQU   *-TABDSCD                                                        
         SPACE 1                                                                
         CSECT                                                                  
         DS    0D                                                               
         DC    C'**COLDSC'                                                      
         CNOP  2,4                                                              
COLDSC   DS    0H                  DATABASE COLUMN DESCRIPTOR LIST              
CDSCEN   DC    (CDSCMAX*CDSCLQ)X'00'                                            
COLDSCX  EQU   *                                                                
CDSCMAX  EQU   1000                                                             
*                                                                               
COLDSCD  DSECT                     DSECT FOR COLDSC                             
CDSCTYPE DS    XL2                 COLUMN TYPE - FROM SQLTYPE                   
CDSCSQLD DS    XL4                 COLUMN DATA CODE - FROM SQLDATA              
CDSCLEN  DS    XL2                 COLUMN CONTROL LENGTH - FROM SQLLEN          
CDSCNLEN DS    XL2                 COLUMN NAME LENGTH                           
CDSCNAME DS    CL30                COLUMN NAME                                  
CDSCLQ   EQU   *-COLDSCD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099DXUDBX    06/20/02'                                      
         END                                                                    
