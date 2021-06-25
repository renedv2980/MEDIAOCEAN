*          DATA SET REREPRR02  AT LEVEL 002 AS OF 02/15/00                      
*PHASE RERR02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPRR02 (RERR02) --- REP REPORT SPEC CONVERSION'              
*                                                                               
***********************************************************************         
*                                                                     *         
*        REREPRR02K--- REP REPORT SPECS CONVERTED TO GENDIR/GENFIL    *         
*                      RECORDS.                                       *         
* *******NOTE**********                                               *         
*                                                                     *         
* IF A NEW RRG REPORT IS CREATED, IT MUST BE ADDED TO THE LIST OF     *         
* REPORTS SO THE SPEC CAN BE LOADED.  THE LIST IS IN PANBOOK          *         
* REGENRRG.  THIS MODULE MUST THEN BE RELINKED AND MADE LIVE.         *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*-DATE----LVL--BY----CHANGE-------------------------------------------*         
*                                                                     *         
* 31AUG98  01  NRK   INITIAL DEVELOPMENT                              *         
*                                                                     *         
* 15FEB00      BU    ADD REPORTS TO LIST                              *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
RERR02   CSECT                                                                  
         NMOD1 MYWORKX-MYWORKD,**RERR**,CLEAR=YES                               
         USING MYWORKD,RC                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE                                   
         JNE   MAIN0000            NO NO - SO CONTINUE                          
*                                                                               
         MVC   P(10),=C'I''M HERE!!!'                                           
         GOTO1 REPORT                                                           
*                                                                               
MAIN0000 EQU   *                                                                
*                                                                               
         CLI   MODE,REQFRST        RIGHT MODE?                                  
         BNE   MAINEXIT            NO - SO JUST EXIT                            
*                                                                               
         BRAS  RE,INIT             DO THE INITIALIZATION STUFF                  
         CLI   QOPTION1,C'Y'       TRACE MODE                                   
         JNE   MAIN0100            NO NO - SO CONTINUE                          
*                                                                               
         MVC   P(14),=C'BACK FROM INIT'                                         
         GOTO1 REPORT                                                           
*                                                                               
MAIN0100 EQU   *                                                                
*                                                                               
         BRAS  RE,READ             READ THE SPEC                                
         BNZ   MAIN0200            NONE LEFT - ALL DONE                         
*                                                                               
         BRAS  RE,WRITESPC         WRITE THE SPEC RECORDS                       
         B     MAIN0100            AND GET NEXT SPEC                            
*                                                                               
MAIN0200 EQU   *                                                                
*                                                                               
         MVC   P(18),=C'TOTAL SPECS READ: '                                     
         EDIT  TOTSPECS,(5,P+19),ALIGN=LEFT                                     
         MVC   P+25(23),=C'TOTAL RECORDS WRITTEN: '                             
         EDIT  TOTRECS,(5,P+49),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE FILOUTA                                                          
*                                                                               
MAINEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DOES ALL THE INITIALIZATION STUFF (OF WHICH THERE                
* AIN'T MUCH RIGHT NOW).                                                        
*                                                                               
INIT     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,WORK),(0,TODAY) GET TODAY'S DATE                  
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE GETS THE REPORT SPEC TWO CHARACTER ID AND THEN CALLS             
* LOADER TO LOAD UP THE REPORT.                                                 
*                                                                               
READ     NTR1                                                                   
         OC    AENTRY,AENTRY       FIRST PASS (NO ADDRESS SET YET)?             
         BZ    READ0100            YES - SO CONTINUE                            
*                                                                               
         L     R2,AENTRY           ELSE - SET A(CURRENT ENTRY)                  
         B     READ0200            AND CONTINUE                                 
*                                                                               
READ0100 EQU   *                                                                
*                                                                               
         LA    R2,RRGRPLST         GET A(LIST OF REPORTS)                       
*                                                                               
READ0200 EQU   *                                                                
*                                                                               
         CLC   0(2,R2),=H'00'      AT E.O.L.?                                   
         BE    READDONE            YES - SO ALL DONE                            
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   READ0220            NO - SO CONTINUE                             
*                                                                               
         MVC   P(17),=C'READING THE SPECS'                                      
         GOTO1 REPORT                                                           
*                                                                               
READ0220 EQU   *                                                                
*                                                                               
         MVC   SPECID(8),SPACES    INITIALIZE FOR LOADER CALL                   
         MVC   SPECID(4),=C'RERG'  SET UP SPEC LOAD PHASE NAME                  
         MVC   SPECID+4(2),0(R2)   MOVE IN PROGRAM NUMBER                       
         GOTO1 LOADER,DMCB,SPECID,0  LOAD THE SPECS                             
         OC    DMCB+4(4),DMCB+4    SPECS PHASE FOUND?                           
         BNZ   READ0300            YES - SO CONTINUE                            
*                                                                               
         LA    R2,RRGENTLN(R2)     ELSE - INC TO NEXT LIST ENTRY                
         B     READ0200            AND CHECK FOR THAT SPEC                      
*                                                                               
READ0300 EQU   *                                                                
*                                                                               
         MVC   LSPECS(4),DMCB      SAVE L(SPECS)                                
         MVC   ASPECS(4),DMCB+4    SAVE A(SPECS)                                
         LA    R2,RRGENTLN(R2)     INC TO NEXT LIST ENTRY                       
         ST    R2,AENTRY           AND SAVE IT FOR THE NEXT PASS                
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTSPECS         GET TOTAL REPORT SPECS READ                  
         LA    R5,1(R5)            INC COUNT OF REPORT SPECS READ               
         STH   R5,TOTSPECS         AND STORE IT                                 
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   READ0400            NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'LEAVING READ ROUTINE'                                   
         GOTO1 REPORT                                                           
*                                                                               
READ0400 EQU   *                                                                
*                                                                               
         SR    R0,R0               SET 'FOUND A SPEC' CC                        
*                                                                               
READEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
READDONE EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   READDON5            NO - SO CONTINUE                             
*                                                                               
         MVC   P(21),=C'NO MORE SPECS TO READ'                                  
         GOTO1 REPORT                                                           
*                                                                               
READDON5 EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET 'NO MORE SPECS' CC                       
         B     READEXIT            AND EXIT                                     
         EJECT                                                                  
*                                                                               
* THIS ROUTINE TAKES THE REPORT SPEC AND BREAKS IT INTO 900 (OR LESS)           
* BYTE CHUNKS WHICH ARE THEN TURNED INTO GSPECREC RECORDS AND WRITTEN           
* OUT TO A TAPE TO BE MERGED INTO GENDIR/GENFIL DURING THE WEEKLY DUMP          
* AND LOAD.                                                                     
*                                                                               
WRITESPC NTR1                                                                   
         LA    R7,1                INITIAL SEQUENCE NUMBER                      
         L     R2,ASPECS           GET A(SPECS)                                 
         L     R3,LSPECS           GET L(SPECS)                                 
*                                                                               
WSPC0100 EQU   *                                                                
*                                                                               
         XCEF  REC,1000            CLEAR REC                                    
         LA    R1,REC+4                                                         
         USING GSPCRECD,R1                                                      
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
         MVC   GSPCKTYP,=X'0070'   RECORD TYPE                                  
         MVC   GSPCKID,SPECID      SPEC ID                                      
         STCM  R7,3,GSPCKSEQ       SEQUENCE NUMBER                              
         MVC   GSPCKDAT,TODAY      TODAY'S DATE                                 
*                                                                               
* BUILD THE RECORD                                                              
*                                                                               
         LA    R4,GSPCDATA         A(DATA PORTION OF TAPE RECORD)               
*                                                                               
WSPC0200 EQU   *                                                                
*                                                                               
         CLI   0(R2),0             END OF SPEC FLAG (EL CODE OF X'00')?         
         JNE   WSPC0250            NO - SO CONTINUE                             
*                                                                               
         MVI   0(R4),X'00'         ELSE - MOVE IN THE FLAG                      
         LH    R6,GSPCFLEN         L(RECORD SO FAR)                             
         LA    R6,1(R6)            INC L(RECORD)                                
         STH   R6,GSPCFLEN         STORE NEW LENGTH                             
         SR    R3,R3               FORCE E.O.S.                                 
         J     WSPC0300            AND GO WRITE THE RECORD                      
*                                                                               
WSPC0250 EQU   *                                                                
*                                                                               
         ZIC   R5,1(R2)            GET L(ELEMENT)                               
         LH    R6,GSPCFLEN         L(RECORD SO FAR)                             
         AR    R6,R5               L(REC + THIS ELEMENT)                        
         C     R6,=F'900'          TOTAL LEN .GT. 900?                          
         BH    WSPC0300            YES - SO GO WRITE THE RECORD                 
*                                                                               
         STH   R6,GSPCFLEN         ELSE - STORE NEW LENGTH                      
         MVCL  R4,R2               MOVE IN THE DATA                             
         OR    R3,R3               MORE DATA IN SPEC?                           
         BNZ   WSPC0200            YES - SO LOOP BACK                           
*                                                                               
WSPC0300 EQU   *                                                                
*                                                                               
         LH    R6,GSPCFLEN         L(RECORD SO FAR)                             
         LA    R6,GSPCFRST(R6)     L(KEY+CNTL+DATA)                             
         STH   R6,GSPCFLEN         STORE L(RECORD)                              
         LA    R6,4(R6)            L(ENTIRE TAPE RECORD)                        
         STH   R6,REC              AND STORE IT                                 
         DROP  R1                                                               
*                                                                               
* NOW DUMP IT OUT JUST TO MAKE SURE WE'VE GOT IT WORKING                        
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSPC0400            NO - SO CONTINUE                             
*                                                                               
         MVC   P(15),=C'GOING TO PRNTBL'                                        
         GOTO1 REPORT                                                           
*                                                                               
WSPC0400 EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'Y'       DUMP OUT MODE?                               
         JNE   WSPC0420            NO - SO CONINTUE                             
*                                                                               
         ZICM  R6,REC,2            EXTRACT LENGTH OF RECORD                     
         LA    R5,REC                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
*                                                                               
WSPC0420 EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSPC0440            NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'GOING TO PUT'                                           
         GOTO1 REPORT                                                           
*                                                                               
WSPC0440 EQU   *                                                                
*                                                                               
         PUT   FILOUTA,REC         WRITE IT                                     
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSPC0460            NO - SO CONTINUE                             
*                                                                               
         MVC   P(13),=C'BACK FROM PUT'                                          
         GOTO1 REPORT                                                           
*                                                                               
WSPC0460 EQU   *                                                                
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTRECS          GET TOTAL # RECORDS WRITTEN                  
         LA    R5,1(R5)            INC COUNT OF RECORDS WRITTEN                 
         STH   R5,TOTRECS          AND STORE IT                                 
*                                                                               
         LA    R7,1(R7)            INC SEQUENCE NUMBER                          
         OR    R3,R3               ANY DATA LEFT FOR NEW RECORD?                
         BNZ   WSPC0100            YES - SO BUILD NEXT RECORD                   
*                                                                               
* NOW DELETE THE SPECS                                                          
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSPC0500            NO - SO CONTINUE                             
*                                                                               
         MVC   P(18),=C'DELETING THE SPECS'                                     
         GOTO1 REPORT                                                           
*                                                                               
WSPC0500 EQU   *                                                                
*                                                                               
         GOTO1 LOADER,DMCB,SPECID,-1 DELETE THE SPECS                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* LITERALS, CONSTANTS, AND WORK AREA                                            
*                                                                               
         LTORG                                                                  
       ++INCLUDE REGENRRG                                                       
*                                                                               
*    WORK SPACE, ETC.                                                           
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=2048,BLKSIZE=8120,BUFNO=2                                  
         SPACE 3                                                                
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
AENTRY   DS    XL4                 CURRENT ENTRY IN SPEC LIST                   
ASPECS   DS    XL4                 A(REPORT SPEC)                               
LSPECS   DS    XL4                 L(REPORT SPEC)                               
SPECID   DS    CL8                 REPORT SPEC ID ('RERGXX  ')                  
TODAY    DS    CL6                 YYMDD DATE                                   
TOTRECS  DS    XL2                 TOTAL RECORDS WRITTEN                        
TOTSPECS DS    XL2                 TOTAL REPORT SPECS READ                      
*                                                                               
REC      DS    XL1000              AREA FOR RECORD                              
         SPACE 2                                                                
MYWORKX  EQU   *                                                                
*                                                                               
       ++INCLUDE GEGENSPEC         REPORT SPEC RECORD                           
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPRR02 02/15/00'                                      
         END                                                                    
