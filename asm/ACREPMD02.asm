*          DATA SET ACREPMD02  AT LEVEL 089 AS OF 02/10/21                      
*PHASE ACMD02A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
                                                                                
ACMD02   TITLE 'SCAN FOR ORPHANED MEDIA POSTING RECORDS'                        
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* VGUP 001 11FEB21 <SPEC-47806> ORPHANED MEDIA POSTING DETAIL RECORDS *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                                                                     *         
* THIS CONVERSION TO READ THE MEDIA POSTING DETAIL RECORD AND REPORT  *         
* AND DELETE THEM THAT DO NOT HAVE AN MAIN BILLING RECORD (SR)        *         
* THAT THE MEDIA POSTING RECORD WAS CREATED WITH.                     *         
*                                                                     *         
* QOPT2 = Y RUN FOR ENTIRE FILE                                       *         
* QOPT2 = N RUN FOR SPECIFIC COMPANY (DEFAULT)                        *         
*                                                                     *         
* QOPT3 = Y DELETE ORPHANED MEDIA POSTING DETAIL RECORDS              *         
* QOPT3 = N SCAN AND REPORT, DO NOT DELETE (DEFAULT)                  *         
*                                                                     *         
* THE FLOW OF THE PROGRAM IS, READ THE MEDIA POSTING DETAIL RECORD    *         
* (MPDRECD)  AND LOOP THROUGH THE MEDIA BILLING                       *         
* TRANSFER ELEMENT (MBTELD) AND FIND THE RECEIVABLE ELEMENT - MBTTYP  *         
* IS MBTTRCV.  READ FOR THE SR ACCOUNT FOUND IN THE MBTULA FIELD.     *         
* IF THE SR ACCOUNT STILL EXISTS, READ MATCHING BILLING TRANSACTION.  *         
* MATCHING OF TRANSACTION IS BASED ON ACCOUNT, CONTRA ACCOUNT, OFFICE *         
* CODE, BILL NUMBER AND AMOUNT. IF THE MATCHING BILLING TRANSACTIONS  *         
* STILL EXISTS, SKIP THIS MEDIA POSTING DETAIL RECORD. IF MATCHING    *         
* BILLING TRANSACTIONS NOT FOUND IT IS ELIGIBLE FOR DELETION          *         
*                                                                     *         
***********************************************************************         
                                                                                
ACMD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACMO**,CLEAR=Y                                               
         L     RC,0(,R1)                                                        
         USING ACWORKD,RC                                                       
         LA    RA,SPACEND                                                       
         USING WORKD,RA            RA=A(WORK AREA SAVED AREA)                   
         L     R9,AMONACC                                                       
         USING ACMD,R9                                                          
         MVI   FCRDTRNS,NOQ        TURN OFF ALL READS, GOING TO DO IT           
         MVI   FCRDHIST,NOQ        OURSELVES                                    
         MVI   FCRDACC,NOQ                                                      
         MVI   FCRDTIME,NOQ                                                     
         MVI   FCRDORD,NOQ                                                      
         MVI   FCRDEST,NOQ                                                      
         CLI   MODE,REQLAST                                                     
         JE    RUNL                                                             
         J     EXIT                                                             
                                                                                
EXITL    CLI   *,FF                SET CC=LOW                                   
         J     EXIT                                                             
EXITE    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         SPACE 1                                                                
RUNL     XR    RF,RF                                                            
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,AIOAREA                                                       
         LH    RF,=Y(IOAREA2-WORKD)                                             
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,AIOAREA2                                                      
         MVI   SRFOUND,C'N'                                                     
*                                                                               
         MVI   DMSW,0                                                           
         CLI   QOPT3,YESQ          DELETE RECORD?                               
         BNE   RUNL10                                                           
         CLI   RCWRITE,YESQ        WRITING TO FILE?                             
         BNE   RUNL10                                                           
         OI    DMSW,X'80'                                                       
* READ MEDIA POSTING DETAIL RECORD                                              
         USING MPDRECD,R2                                                       
RUNL10   LA    R2,IOKEY            READ FOR ALL COMPANY RECORDS                 
         XC    IOKEY,IOKEY                                                      
         MVI   MPDKTYP,MPDKTYPQ                                                 
         MVI   MPDKSUB,MPDKSUBQ                                                 
         MVC   MPDKCPY,QCOMPANY                                                 
         CLI   QOPT2,YESQ                                                       
         BNE   *+8                                                              
         MVI   MPDKCPY,X'41'                                                    
         MVC   CCODE,MPDKCPY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,MPDKEY,MPDKEY                         
         BE    RUNL40                                                           
         DC    H'0'                                                             
*                                                                               
RUNL20   LA    R2,IOKEY                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,MPDKEY,MPDKEY,0                       
         BE    RUNL30                                                           
         DC    H'0'                                                             
*                                                                               
RUNL30   LA    R2,IOKEY                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,MPDKEY,MPDKEY,0                       
         BE    RUNL40                                                           
         DC    H'0'                                                             
*                                                                               
RUNL40   CLI   MPDKTYP,MPDKTYPQ    MEDIA POSTING RECORD?                        
         BNE   RUNLX               NO, EXIT                                     
         CLI   MPDKSUB,MPDKSUBQ    MEDIA POSTING DETAIL RECORD?                 
         BNE   RUNLX               NO, EXIT                                     
         CLI   QOPT2,YESQ                                                       
         BE    *+14                                                             
         CLC   CCODE,MPDKCPY                                                    
         BNE   RUNLX                                                            
         MVC   SVMPDDA,MPDKDA                                                   
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,MPDKDA,AIOAREA,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COCODE,MPDKCPY                                                   
         AP    TOTCOUNT,PONE       INCREASE READ COUNT                          
         L     R3,AIOAREA                                                       
*GET MEDIA BILLING TRANSFER ELEMENT                                             
         USING MBTELD,R3                                                        
         MVI   ELCODE,MBTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   RUNL20                                                           
*                                                                               
RUNL60   CLI   MBTTYP,MBTTRCV      RECEIVABLES?                                 
         BE    RUNL65              YES, CHECK FOR SR RECORD EXIST               
         MVI   ELCODE,MBTELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   RUNL20              NOT FOUND, READ NEXT RECORD                  
         B     RUNL60                                                           
*                                                                               
* READ SR ACCOUNT                                                               
         USING ACTRECD,R4                                                       
RUNL65   LA    R4,SAVEKEY                                                       
         MVC   SAVEKEY,SPACES                                                   
         MVC   ACTKCPY,COCODE      COMPANY CODE                                 
         MVC   ACTKULA,MBTULA                                                   
         MVI   SRFOUND,C'Y'                                                     
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY                         
         BNE   RUNL200            REPORT IT FOR DELETE                          
*                                                                               
*SR ACCOUNT FOUND, NOW READ SR TRANSACTION                                      
         PACK  BILLDT,MPDKPER                                                   
         MVO   OBILLDT,BILLDT                                                   
*                                                                               
         USING TRNRECD,R6                                                       
         LA    R6,SAVEKEY                                                       
         MVC   TRNKCPY,COCODE      COMPANY CODE                                 
         MVC   TRNKULA,MBTULA                                                   
         MVC   TRNKOFF,MBTOFFC                                                  
         MVC   TRNKULC,MBTCNTRA                                                 
         MVC   TRNKDATE(2),OBILLDT                                              
         MVI   TRNKDATE+2,X'00'                                                 
         MVC   SAVEKEY1,TRNKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TRNKEY,TRNKEY                         
         BE    RUNL80                                                           
         DC    H'0'                                                             
*                                                                               
RUNL70   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,TRNKEY,TRNKEY,0                       
         BE    RUNL80                                                           
         DC    H'0'                                                             
*                                                                               
RUNL80   CLC   TRNKEY(TRNKREF-TRNKEY-1),SAVEKEY1                                
         BNE   RUNL900             REPORT FOR DELETE                            
*                                                                               
         MVC   MPDREF#(1),MPDKPER+1                                             
         MVI   MPDREF#+1,C'A'                                                   
         CLC   MPDKPER+2(2),=C'10'                                              
         BE    RUNL90                                                           
         MVI   MPDREF#+1,C'B'                                                   
         CLC   MPDKPER+2(2),=C'11'                                              
         BE    RUNL90                                                           
         MVI   MPDREF#+1,C'C'                                                   
         CLC   MPDKPER+2(2),=C'12'                                              
         BE    RUNL90                                                           
         MVC   MPDREF#+1(1),MPDKPER+3                                           
RUNL90   MVC   MPDREF#+2(4),MPDKINV+1                                           
*                                                                               
         CLC   TRNKREF(6),MPDREF#                                               
         BNE   RUNL70              READ NEXT TRANSACTION RECORD                 
*                                                                               
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,TRNKDA,AIOAREA2,DMWORK                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIOAREA2                                                      
T        USING TRNRECD,R5                                                       
         LA    R7,T.TRNRFST        POINT TO FIRST ELEMENT                       
         DROP  T                                                                
         USING TRNELD,R7                                                        
*                                                                               
RUNL100  CLI   TRNEL,0             RECORD END?                                  
         BE    RUNL70              READ NEXT TRANSACTION RECORD                 
*                                                                               
         CLI   TRNEL,TRNELQ        LOCATION ELEMENT ?                           
         BNE   RUNL110             NO: GET NEXT ELEMENT                         
*                                                                               
         CP    TRNAMNT,MBTPOST                                                  
         BE    RUNL20              READ NEXT MEDIA POSTING DETAIL REC           
         B     RUNL70              READ NEXT TRANSACTION RECORD                 
*                                                                               
RUNL110  LLC   RE,TRNLN            LOAD ELEMENT LENGTH                          
         AR    R7,RE               POINT TO NEXT ELEMENT                        
         B     RUNL100             VALIDATE NEXT ELEMENT                        
         DROP  R7                                                               
*                                                                               
RUNL200  MVI   SRFOUND,C'N'                                                     
*                                                                               
RUNL900  GOTO1 VHEXOUT,DMCB,COCODE,HEXOCCD,L'COCODE                             
         MVC   P+1(L'HEXOCCD),HEXOCCD                                           
         MVC   P+4(12),MPDKSYS                                                  
         MVC   P+18(4),MPDKPER                                                  
         MVC   P+24(5),MPDKINV                                                  
         MVC   P+34(14),MBTULA                                                  
         GOTO1 VHEXOUT,DMCB,SVMPDDA,DA,L'ACTKDA                                 
         MVC   P+50(12),MBTCNTRA                                                
         MVC   P+66(2),MBTOFFC                                                  
         MVC   P+75(8),DA                                                       
         MVC   P+85(1),SRFOUND                                                  
*                                                                               
         CLI   QOPT3,YESQ          DELETE RECORD?                               
         BNE   RUNL950                                                          
         CLI   RCWRITE,YESQ        TO UPDATE THE ACCOUNT FILE?                  
         BNE   RUNL950             NO, THEN PRINT TO O/P FOR STATISTIC          
*UPDATE RECORD                                                                  
         GOTO1 DATAMGR,DMCB,(DMSW,DMREAD),ACCDIR,MPDKEY,MPDKEY,0                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DATAMGR,DMCB,(DMSW,GETREC),ACCMST,MPDKDA,AIOAREA,DMWORK          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA                                                       
U        USING MPDRECD,R3                                                       
         OI    U.MPDRSTA,X'80'                                                  
         DROP  U                                                                
         GOTO1 DATAMGR,DMCB,(DMSW,PUTREC),ACCMST,SVMPDDA,AIOAREA,DMWORK         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         OI    MPDKSTA,X'80'                                                    
         GOTO1 DATAMGR,DMCB,(DMSW,DMWRT),ACCDIR,MPDKEY,MPDKEY,0                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ACREPORT                                                         
         AP    COUNT,PONE                                                       
         B     RUNL30              READ NEXT RECORD                             
*                                                                               
RUNL950  GOTO1 ACREPORT                                                         
         AP    COUNT,PONE          INCREASE PRINT COUNT                         
*                                                                               
         B     RUNL20              READ NEXT RECORD                             
*                                                                               
* GETEL                                                                         
         GETEL R3,56,ELCODE                                                     
* EXIT                                                                          
RUNLX    MVC   P+1(L'TEXT1),TEXT1                                               
         CURED (P6,COUNT),(14,P+50),0,ALIGN=LEFT,MINUS=YES                      
         GOTO1 ACREPORT                                                         
         MVC   P+1(L'TEXT2),TEXT2                                               
         CURED (P6,TOTCOUNT),(14,P+50),0,ALIGN=LEFT,MINUS=YES                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
K        EQU   1024                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
SPACEQ   EQU   X'40'                                                            
         SPACE 1                                                                
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPROMOTE DC    V(PROMOTE)                                                       
GETREC   DC    C'GETREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
WRITE    DC    C'DMWRT   '                                                      
*                                                                               
TEXT1    DC    C'NUMBER OF ORPHANED MEDIA POSTING RECORDS'                      
TEXT2    DC    C'TOTAL NUMBER OF MEDIA POSTING RECORDS READ'                    
*                                                                               
* FILE TOTALS                                                                   
*                                                                               
TOTCOUNT DC    PL6'0'                                                           
COUNT    DC    PL6'0'                                                           
PONE     DC    P'1'                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
AIOAREA  DS    A            ADDRESS OF IO AREA                                  
AIOAREA2 DS    A            ADDRESS OF IO AREA                                  
SVMPDDA DS     XL4          SAVE DA                                             
*                                                                               
SRFOUND  DS    XL1          SR RECORD FOUND FLAG                                
ELCODE   DS    XL1          ELEMENT CODE                                        
COCODE   DS    XL1          COMPANY CODE                                        
CCODE    DS    XL1          COMPANY CODE                                        
DMSW     DS    X            READ FOR UPDATE FLAG                                
HEXOCCD  DS    CL2          COMPANY CODE                                        
DA       DS    CL8          DISK ADDRESS                                        
MPDREF#  DS    CL6                                                              
SAVEKEY1 DS    XL64                                                             
BILLDT   DS    XL3                                                              
OBILLDT  DS    XL3                                                              
*                                                                               
SAVEKEY  DS    XL64                                                             
IOKEY    DS    XL64                                                             
IOAREA   DS    XL(2*K)                                                          
IOAREA2  DS    XL(2*K)                                                          
         DS    XL3                                                              
*                                                                               
WORKX    EQU   *-WORKD                                                          
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089ACREPMD02 02/10/21'                                      
         END                                                                    
