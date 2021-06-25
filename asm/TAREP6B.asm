*          DATA SET TAREP6B    AT LEVEL 001 AS OF 01/06/16                      
*PHASE T7036BA,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T7036B - NETSUITE POSTING DOWNLOAD'                             
T7036B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7036B,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     ERXIT                                                            
                                                                                
ERRMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     ERXIT                                                            
                                                                                
ERXIT    GOTO1 ERREX                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         ZAP   DCOUNT,=P'0'                                                     
         ZAP   DAMOUNT,=P'0'                                                    
         ZAP   CCOUNT,=P'0'                                                     
         ZAP   CAMOUNT,=P'0'                                                    
                                                                                
         GOTO1 DATCON,DMCB,(1,TGTODAY1),WORK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,-1                                        
         GOTO1 DATCON,DMCB,WORK+6,(1,YESTERDY)                                  
                                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',DATHEAD),L'DATHEAD                           
         GOTO1 (RF),(R1),(C'T',EMPHEAD),L'EMPHEAD                               
         GOTO1 (RF),(R1),(C'T',REFHEAD),L'REFHEAD                               
         GOTO1 (RF),(R1),(C'T',OFFHEAD),L'OFFHEAD                               
         GOTO1 OUTPDOWN,DMCB,(C'T',ACCHEAD),L'ACCHEAD                           
         GOTO1 (RF),(R1),(C'T',CNTHEAD),L'CNTHEAD                               
         GOTO1 (RF),(R1),(C'T',AMTHEAD),L'AMTHEAD                               
         MVI   DLCXDELC,C' '                                                    
         GOTO1 (RF),(R1),(C'T',DOCHEAD),L'DOCHEAD                               
         MVI   DLCXDELC,C','                                                    
         BAS   RE,EOLDOWN                                                       
                                                                                
         L     RE,=A(WRKBUFF)                                                   
         ST    RE,AWRKBUFF                                                      
         MVC   WFFILE,=CL8'WKFILE'                                              
         XC    WFSPS,WFSPS                                                      
                                                                                
***********************************************************************         
                                                                                
         USING FLTTABD,R4                                                       
         LA    R4,FLTTAB                                                        
PREP10   CLC   RECNUM,FTTYPE                                                    
         JNE   PREP60                                                           
         MVC   WFSPS(3),FTINITS                                                 
         XC    WFINDEX,WFINDEX                                                  
                                                                                
PREP20   BAS   RE,GETWF                                                         
         JNE   PREP60                                                           
PREP30   BAS   RE,WKREAD                                                        
         JNE   PREP20                                                           
                                                                                
         LA    R2,WREC+4                                                        
                                                                                
         USING PSHEADD,R2                                                       
         CLI   0(R2),PSHDELQ                                                    
         JNE   PREP30                                                           
                                                                                
         USING TRANSD,R3                                                        
         LA    R3,70(R2)                                                        
                                                                                
         MVC   WORK(10),SPACES                                                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(20,PARAS)                              
         MVC   WORK(2),PARAS+4                                                  
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),PARAS+6                                                
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),PARAS                                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
                                                                                
         GOTO1 (RF),(R1),(C'T',FTEMP),L'FTEMP                                   
         GOTO1 (RF),(R1),(C'T',TRNSREF),L'TRNSREF                               
         GOTO1 (RF),(R1),(C'T',TRNSANAL),L'TRNSANAL                             
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',PSHDAUL),L'PSHDAUL+L'PSHDAACT                
         GOTO1 (RF),(R1),(C'T',PSHDSUNT),                              +        
               L'PSHDSUNT+L'PSHDSLDG+L'PSHDSACT                                 
         DROP  R2                                                               
                                                                                
         EDIT  (P6,TRNSAMNT),AMOUNT,2,MINUS=YES                                 
         GOTO1 OUTPDOWN,DMCB,(C'N',AMOUNT),L'AMOUNT                             
                                                                                
         MVI   DLCXDELC,C' '                                                    
         TM    TRNSSTAT,X'80'                                                   
         JZ    PREP40                                                           
         GOTO1 (RF),(R1),(C'T',=C'1'),1                                         
         AP    DCOUNT,=P'1'                                                     
         AP    DAMOUNT,TRNSAMNT                                                 
         J     PREP50                                                           
PREP40   GOTO1 (RF),(R1),(C'T',=C'2'),2                                         
         AP    CCOUNT,=P'1'                                                     
         AP    CAMOUNT,TRNSAMNT                                                 
         DROP  R3                                                               
                                                                                
PREP50   MVI   DLCXDELC,C','                                                    
         BAS   RE,EOLDOWN                                                       
         J     PREP30                                                           
                                                                                
***********************************************************************         
                                                                                
PREP60   CLI   FTLNQ(R4),X'FF'                                                  
         JE    PREP70                                                           
         LA    R4,FTLNQ(R4)                                                     
         J     PREP10                                                           
         DROP  R4                                                               
                                                                                
PREP70   J     XIT                                                              
*&&DO                                                                           
PREP70   GOTO1 OUTPDOWN,DMCB,(C'N',SPACES),L'SPACES                             
         GOTO1 (RF),(R1),(C'N',SPACES),L'SPACES                                 
         GOTO1 (RF),(R1),(C'N',SPACES),L'SPACES                                 
         GOTO1 (RF),(R1),(C'N',SUMTOT),L'SUMTOT                                 
         EDIT  (P6,DCOUNT),(10,PARAS),ALIGN=LEFT                                
         GOTO1 OUTPDOWN,DMCB,(C'N',PARAS),10                                    
         EDIT  (P8,DAMOUNT),(12,PARAS),2,ALIGN=LEFT,MINUS=YES                   
         GOTO1 OUTPDOWN,DMCB,(C'N',PARAS),12                                    
         MVI   DLCXDELC,C' '                                                    
         GOTO1 (RF),(R1),(C'T',=C'1'),1                                         
         MVI   DLCXDELC,C','                                                    
         BAS   RE,EOLDOWN                                                       
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'N',SPACES),L'SPACES                             
         GOTO1 (RF),(R1),(C'N',SPACES),L'SPACES                                 
         GOTO1 (RF),(R1),(C'N',SPACES),L'SPACES                                 
         GOTO1 (RF),(R1),(C'N',SUMTOT),L'SUMTOT                                 
         EDIT  (P6,CCOUNT),(10,PARAS),ALIGN=LEFT                                
         GOTO1 OUTPDOWN,DMCB,(C'N',PARAS),10                                    
         EDIT  (P8,CAMOUNT),(12,PARAS),2,ALIGN=LEFT,MINUS=YES                   
         GOTO1 OUTPDOWN,DMCB,(C'N',PARAS),12                                    
         MVI   DLCXDELC,C' '                                                    
         GOTO1 (RF),(R1),(C'T',=C'2'),1                                         
         BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE GETS WORKER FILE                                     *         
***********************************************************************         
                                                                                
GETWF    NTR1                                                                   
         XC    RECORD,RECORD                                                    
                                                                                
         USING UKRECD,R2                                                        
         LA    R2,WFINDEX                                                       
GETWF10  BAS   RE,INDEXRD          GET INDEX OF FIRST/NEXT FILE                 
         JNE   NO                                                               
                                                                                
         CLC   UKDAY,YESTERDY+2    SKIP IF NOT FROM YESTERDAY                   
         JNE   GETWF10                                                          
         CLC   UKSYSPRG,WFSPS      WORKER FILE NAME MUST MATCH                  
         JNE   GETWF10                                                          
                                                                                
         CLI   RECNUM,RNBILL       IF RUNNING FOR BILLING                       
         JNE   YES                                                              
         XC    WORK,WORK           SKIP PRODUCTION POSTINGS                     
         MVC   WORK+8(L'UKUSRID),UKUSRID                                        
         GOTO1 USERVAL,DMCB,(X'B0',WORK)                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TGALPH,=C'DS'                                                    
         JE    YES                                                              
         CLC   TGALPH,=C'D2'                                                    
         JE    YES                                                              
         CLC   TGALPH,=C'D3'                                                    
         JE    YES                                                              
         CLC   TGALPH,=C'DP'                                                    
         JNE   GETWF10                                                          
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TRIES TO FIND A WORKER FILE                          *         
***********************************************************************         
                                                                                
INDEXRD  NTR1                                                                   
         MVC   WFCOMMND,=CL8'INDEX'                                             
         GOTO1 DATAMGR,DMCB,WFCOMMND,WFFILE,WFINDEX,WREC,AWRKBUFF               
         CLI   8(R1),0             IF NO ERROR CONDITION CODE                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE RADS RECORD FROM CURRENT WORKER FILE                 *         
***********************************************************************         
                                                                                
WKREAD   NTR1                                                                   
         LA    RE,WREC             CLEAR WORKER FILE RECORD AREA                
         LHI   RF,L'WREC                                                        
         XCEFL                                                                  
                                                                                
         MVC   WFCOMMND,=CL8'READ'                                              
         GOTO1 DATAMGR,DMCB,WFCOMMND,WFFILE,WFINDEX,WREC,AWRKBUFF               
         CLI   DMCB+8,0                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C','       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,C' '       SPACE FOR END OF LINE                        
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OUTPUT DOWNLOAD FIELD                                        *         
*        ON ENTRY ... P1 BYTE 0=TYPE TO PASS                          *         
*                     P1       =A(DATA)                               *         
*                     P2       =LENGTH                                *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         CLI   DLCBTYP,DLCBNUM                                                  
         JE    OPD30                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         J     OPD50                                                            
                                                                                
OPD30    EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLD(0),0(RF)                                                 
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
RNBILL   EQU   160                                                              
RNCHECK  EQU   161                                                              
                                                                                
DATHEAD  DC    C'DATE'                                                          
EMPHEAD  DC    C'EMPLOYER'                                                      
REFHEAD  DC    C'REFERENCE'                                                     
OFFHEAD  DC    C'OFFICE'                                                        
ACCHEAD  DC    C'ACCOUNT'                                                       
CNTHEAD  DC    C'CONTRA-ACCOUNT'                                                
AMTHEAD  DC    C'AMOUNT'                                                        
DOCHEAD  DC    C'DEBIT/CREDIT'                                                  
                                                                                
SUMTOT   DC    C'SUMMARY TOTAL'                                                 
                                                                                
FLTTAB   DC    AL1(RNBILL),C'TBI',C'TP'                                         
         DC    AL1(RNBILL),C'TPB',C'PP'                                         
         DC    AL1(RNBILL),C'TVB',C'P+'                                         
         DC    AL1(RNCHECK),C'TCK',C'TP'                                        
         DC    AL1(RNCHECK),C'TCU',C'TP'                                        
         DC    AL1(RNCHECK),C'TCN',C'TP'                                        
         DC    AL1(RNCHECK),C'TPC',C'PP'                                        
         DC    AL1(RNCHECK),C'TEC',C'PP'                                        
         DC    AL1(RNCHECK),C'TPK',C'P+'                                        
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
                                                                                
         DS    0D                  WORKER FILE RECORD                           
         DC    C'**WREC**'                                                      
WREC     DS    CL4096                                                           
         DS    0D                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**WRK1**'                                                      
WRKBUFF  DS    14336C                                                           
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
AWRKBUFF DS    A                   A(WORKER FILE BUFFER)                        
WFSPS    DS    CL4                 WORKER FILE SYS/PRO/SUBPRO                   
WFINDEX  DS    CL42                WORKER FILE INDEX                            
WFCOMMND DS    CL8                 WORKER FILE COMMAND                          
WFFILE   DS    CL8                 WORKER FILE FILE                             
RECORD   DS    CL150                                                            
                                                                                
YESTERDY DS    XL3                 YESTERDAY'S DATE                             
                                                                                
AMOUNT   DS    CL12                AMOUNT                                       
                                                                                
CCOUNT   DS    PL6                 CREDIT LINE COUNT                            
CAMOUNT  DS    PL8                 CREDIT AMOUNT                                
DCOUNT   DS    PL6                 DEBIT LINE COUNT                             
DAMOUNT  DS    PL8                 DEBIT AMOUNT                                 
                                                                                
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
                                                                                
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
*DMWRKRK                                                                        
*ACGENPOST                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
***********************************************************************         
*        FILTER TABLE DSECT                                                     
***********************************************************************         
                                                                                
FLTTABD  DSECT                                                                  
FTTYPE   DS    AL1                                                              
FTINITS  DS    CL3                                                              
FTEMP    DS    CL2                                                              
FTLNQ    EQU   *-FLTTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP6B   01/06/16'                                      
         END                                                                    
