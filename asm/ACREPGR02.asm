*          DATA SET ACREPGR02  AT LEVEL 004 AS OF 12/10/12                      
*PHASE ACGR02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* FILTERS                                                                       
*   EFFECTIVE DATE - ALL = SHOW ALL RULES REGARDLESS OF EFFECTIVE DATE          
*                    DATE = SHOW ALL RULES IN EFFECT AS OF DATE                 
*                           SPECIFIED                                           
*                    BLANK = SHOW ALL RULES IN EFFECT AS OF TODAYS DATE         
* OPTIONS                                                                       
*   QOPT1 - I = REPORT ONLY INPUT RULES                                         
*           O = REPORT ONLY OUTPUT RULES                                        
*           B OR BLANK = REPORT ALL RULES (DEFAULT)                             
*                                                                               
* LAP1 - REPORT ON INPUT RULES                                                  
* LAP2 - REPORT ON OUTPUT RULES                                                 
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         TITLE 'GST TAX RULES REPORT'                                           
ACGR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACGR**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACGR02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         GOTO1 ADDICTAT,DMCB,C'LU  ',DDIN,DDOUT                                 
*                                                                               
         MVI   LAPBIT,0                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
REQF     DS    0H                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         TM    LAPBIT,LAP2         IF THIS IS LAP 2 DO NOT CHECK                
         BO    REQF05                                                           
         MVI   RCREQREP,C'Y'       FORCE REQUEST PAGE HEADING                   
         CLI   QOPT1,C'O'          WANT OUTPUT RULES ONLY                       
         BNE   *+12                YES-THEN DEFAULT TO LAP2                     
         OI    LAPBIT,LAP2                                                      
         B     *+8                                                              
         OI    LAPBIT,LAP1         ELSE DEFAULT TO LAP1                         
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
REQF05   MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
         USING ACCOMPD,R4                                                       
         L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         EX    R1,*-6                                                           
*                                                                               
         LA    R2,SREC                                                          
         USING SRECD,R2                                                         
         XC    SRKEY(SRECLNQ),SRKEY     CLEAR EVERYTHING                        
*                                                                               
         MVC   SVEFFDT,SPACES                                                   
         CLC   QSELECT(3),=C'ALL'  WANT TO SEE ALL RECS                         
         BE    REQF20              THEN DO NOTHING                              
         CLC   QSELECT(3),SPACES   BLANK-THEN USE TODAY'S DATE                  
         BNE   REQF10                                                           
         GOTO1 DATCON,DMCB,(5,0),(1,SVEFFDT)                                    
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 DATCON,DMCB,(0,QSELECT),(1,SVEFFDT) SAVE DATE ENTERED            
*                                                                               
         USING TAXRECD,R6                                                       
REQF20   L     R6,AIO1                                                          
         XC    TAXKEY,TAXKEY                                                    
         MVI   TAXKTYP,TAXKTYPQ    X'05'                                        
         MVC   TAXKCPY,RCCOMPFL                                                 
         MVC   SVKEY,0(R6)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO1,AIO1                             
         B     REQF25                                                           
*                                                                               
REQFSQ   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO1,AIO1                             
REQF25   CLC   TAXKEY(TAXKPRV-TAXKEY),SVKEY                                     
         BNE   REQFX                                                            
*                                                                               
REQF30   CLC   SVEFFDT,SPACES      ANY EFFECTIVE DATE FILTER                    
         BE    REQF40              SHOWALL                                      
         CLC   TEMPPRV,TAXKPRV     SAME PROVINCE                                
         BNE   REQF35                                                           
         CLC   TEMPOFF,TAXKOFF     SAME OFFICE                                  
         BE    REQFSQ              GET NEXT REC                                 
REQF35   SR    R1,R1                                                            
         ICM   R1,7,TAXKDATE       LOOK FOR THE NEAREST DATE IN THE             
         LNR   R1,R1               PAST                                         
         BCTR  R1,0                                                             
         STCM  R1,7,WORK                                                        
         CLC   WORK(3),SVEFFDT                                                  
         BH    REQFSQ                                                           
*                                                                               
* NO PROVINCE IN KEY THEN ZERO'S.  NO OFFICE IN KEY THEN FF'S. MUST             
* CHANGE TO ZERO'S SO WILL SORT THE COMPANY LEVEL RECS FIRST                    
*                                                                               
REQF40   MVC   SRPROV,TAXKPRV      PROVINCE                                     
         MVC   SROFFICE,TAXKOFF    OFFICE                                       
         CLC   TAXKOFF,=X'FFFF'    NO OFFICE IN KEY                             
         BNE   *+10                                                             
         XC    SROFFICE,SROFFICE   FORCE IN BINARY ZERO'S FOR SORTING           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TAXKDATE                                                    
         LNR   R1,R1                                                            
         BCTR  R1,0                                                             
         STCM  R1,7,SREFFDTE                                                    
*                                                                               
         MVC   TEMPOFF,TAXKOFF                                                  
         MVC   TEMPPRV,TAXKPRV                                                  
*                                                                               
REQF100  GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         B     REQFSQ                                                           
*                                                                               
REQFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         MVC   SVPROV,SPACES                                                    
         MVC   SVOFF,SPACES                                                     
*                                                                               
REQL10   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R2,15,4(R1)                                                      
         BZ    REQL20                                                           
*                                                                               
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
         USING SRECD,R2                                                         
         USING TAXRECD,R6                                                       
         L     R6,AIO1                                                          
         MVC   SREC,0(R2)                                                       
*                                                                               
         MVC   TEMPPRV,SRPROV      PROVINCE                                     
         MVC   TEMPOFF,SROFFICE    OFFICE                                       
         OC    TEMPOFF,TEMPOFF     ANYTHING IN THE OFFICE                       
         BNZ   *+10                                                             
         MVC   TEMPOFF,=X'FFFF'    THEN SWITCH BACK TO FF'S                     
         SR    R1,R1                                                            
         ICM   R1,7,SREFFDTE                                                    
         LA    R1,1(R1)            ADD 1 TO GET THE RIGHT DATE                  
         LNR   R1,R1                                                            
         STCM  R1,7,TEMPDATE                                                    
*                                                                               
         XC    TAXKEY,TAXKEY                                                    
         MVI   TAXKTYP,TAXKTYPQ    X'05'                                        
         MVC   TAXKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   TAXKPRV,TEMPPRV                                                  
         MVC   TAXKOFF,TEMPOFF                                                  
         MVC   TAXKDATE,TEMPDATE                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO1                             
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    REQL20                                                           
*                                                                               
         USING FFTELD,R4                                                        
         LR    R4,R6                                                            
         MVI   ELCODE,FFTELQ       X'DB'                                        
         BAS   RE,GETEL                                                         
         BNE   REQL15              PUT REC TO SORT                              
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    REQL15                                                           
         EXMVC R1,SVREG,FFTDATA    SAVE THE REGISTRATION # AND THE              
         MVC   SVREGLN,FFTDLEN     LENGTH FOR LATER                             
*                                                                               
         TM    LAPBIT,LAP1         IS THIS INPUT RULES LAP                      
         BZ    REQL30                                                           
         CLI   QOPT1,C'O'          OUTPUT REQUESTED ONLY                        
         BE    REQL30                                                           
*                                                                               
         USING TAXELD,R4                                                        
REQL15   LR    R4,R6                                                            
         MVI   ELCODE,TAXIELQ      X'DE' INPUT TAX ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
REQLNX   BAS   RE,NEXTEL                                                        
         BNE   REQL10              GET NEXT RECORD                              
         OI    BIT,PRINPUT         PRINTING INPUT RULES NOW                     
         BAS   RE,PRDETAIL         FILL IN DETAIL LINE                          
*                                                                               
         BAS   RE,PRNTXP                                                        
         B     REQLNX                                                           
*                                                                               
REQL20   CLI   QOPT1,C'I'          INPUT RULES WANTED ONLY                      
         BE    REQL900             THEN DONE                                    
         TM    LAPBIT,LAP2         HAVE WE JUST DID LAP 2                       
         BO    REQL900             THEN DONE                                    
*                                                                               
* MUST PUT RECORDS TO SORTER AGAIN TO PRINT OUTPUT RULES NOW                    
*                                                                               
         NI    BIT,X'FF'-PRINPUT                                                
         NI    LAPBIT,X'FF'-LAP1   TURN OFF LAP 1 BIT                           
         OI    LAPBIT,LAP2         TURN ON LAP 2 BIT                            
         MVI   FORCEHED,C'Y'       PAGE BREAK ON CHANGE OF RULES                
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     RESET REQFRST                                
         MVI   RCREQREP,C'N'                                                    
         B     XIT                                                              
*                                                                               
         USING TAXELD,R4                                                        
REQL30   LR    R4,R6                                                            
         MVI   ELCODE,TAXOELQ      X'DF' OUTPUT TAX ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
REQL2NX  BAS   RE,NEXTEL                                                        
         BNE   REQL10                                                           
         NI    BIT,X'FF'-PRINPUT   PRINTING OUTPUT RULES NOW                    
         BAS   RE,PRDETAIL         FILL IN DETAIL LINE                          
*                                                                               
         BAS   RE,PRNTXP                                                        
         B     REQL2NX                                                          
*                                                                               
REQL900  GOTO1 ADSORTER,DMCB,=C'END'                                            
         MVI   LAPBIT,0            RESET IN CASE MORE THAN 1 REQUEST            
         MVI   RCREQREP,C'Y'       FORCE REQUEST PAGE HEADING                   
         B     XIT                                                              
         EJECT                                                                  
         DROP  R2,R4,R6                                                         
***********************************************************************         
*              FILL IN REPORT DETAILS LINE                            *         
***********************************************************************         
*                                                                               
         USING TAXELD,R4                                                        
PRDETAIL NTR1                                                                   
*                                                                               
         CLC   TEMPOFF,=X'FFFF'    NO OFFICE                                    
         BNZ   *+14                                                             
         OC    TEMPPRV,TEMPPRV     AND NO PROVINCE                              
         BZ    PRD10                                                            
         CLC   SVOFF,TEMPOFF       SAME OFFICE                                  
         BNE   PRD05                                                            
         CLC   SVPROV,TEMPPRV      SAME PROVINCE THEN DON'T PRINT               
         BE    PRD10                                                            
PRD05    TM    BIT,PRINPUT         PRINTING INPUT OR OUTPUT                     
         BZ    *+14                                                             
         MVC   XHEAD4+20(L'AC@INP),AC@INP    MOVE INTO HEADLINE                 
         B     *+10                                                             
         MVC   XHEAD4+20(L'AC@OUT),AC@OUT    MOVE INTO HEADLINE                 
         GOTO1 ACREPORT            SKIP A LINE                                  
         MVC   PLINOFF,TEMPOFF                                                  
         OC    PLINOFF,SPACES                                                   
         MVI   PLINDIV,C'/'                                                     
         MVC   PLINPROV,TEMPPRV                                                 
         OC    PLINPROV,SPACES                                                  
*                                                                               
PRD10    MVC   SVPROV,TEMPPRV                                                   
         MVC   SVOFF,TEMPOFF                                                    
         XC    WORK,WORK                                                        
         SR    R1,R1                                                            
         ICM   R1,7,TEMPDATE                                                    
         LNR   R1,R1                                                            
         BCTR  R1,0                SUBTRACT 1 TO GET RIGHT DATE                 
         STCM  R1,7,WORK                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(11,PLINDTE) EFFECTIVE DATE                 
         MVC   PLINCODE(1),TAXCODE  TAX CODE                                    
         TM    TAXINDS,TAXIDFLT    DEFAULT TAX RATE?                            
         BZ    *+8                                                              
         MVI   PLINCODE+1,C'*'                                                  
         MVC   PLINTYPE,TAXTYPE    INPUT TYPE                                   
         MVC   PLINACCD,TAXACTA    ACC CODE                                     
         ZIC   R1,TAXLN                                                         
         SH    R1,=Y(TAXLN1Q+1)                                                 
         EXMVC R1,PLINACNM,TAXNAME ACC NAME                                     
         CURED (2,TAXRATE),(6,PLINRATE),2,FLOAT=-,ALIGN=RIGHT                   
         ORG   *-2                                                              
         TM    TAXINDS,TAXIDC3     IS RATE 3 DECIMALS?                          
         BNO   *+8                                                              
         MVI   11(R1),3                                                         
         BASR  RE,RF                                                            
         ZIC   R1,SVREGLN                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    REQL15                                                           
         EXMVC R1,PLINREG,SVREG     MOVE IN REGISTRATION #                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PRINT A LINE                                           *         
***********************************************************************         
*                                                                               
PRNTXP   NTR1                                                                   
         MVC   XHEAD2+12(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+21(L'CMPNAME),CMPNAME                                     
         MVI   XHEAD4+4,C':'                                                    
         TM    BIT,PRINPUT         PRINTING INPUT OR OUTPUT                     
         BZ    *+14                                                             
         MVC   XHEAD4+6(L'AC@INP),AC@INP                                        
         B     PRNTX10                                                          
         MVC   XHEAD4+6(L'AC@OUT),AC@OUT                                        
*                                                                               
PRNTX10  MVC   SVPROV,TEMPPRV                                                   
         MVC   SVOFF,TEMPOFF                                                    
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              EXTERNAL ADDRESS LIST                                  *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    V(HELLO)                                                         
         DC    A(IO1)                                                           
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCBIG   DC    CL8'ACCBIG  '                                                    
*                                                                               
SORTCARD DC    C'SORT FIELDS=(1,2,A,3,2,A,5,3,A),FORMAT=BI,WORK=1 '             
RECCARD  DC    C'RECORD TYPE=F,LENGTH=99 '                                      
*                                                                               
DDIN     DS    0C                  DATA DICTIONARY INPUT                        
         DCDDL AC#INP,5            INPUT                                        
         DCDDL AC#OTPT,6           OUTPUT                                       
         DC    X'00'                                                            
*                                                                               
DDOUT    DS    0C                  DATA DICTIONARY OUTPUT                       
AC@INP   DS    CL5                                                              
AC@OUT   DS    CL6                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              BOX HOOK                                               *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXCOLS+6,C'L'                                                   
*                                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT BOXES                         
         BNE   BX500                                                            
         MVI   BOXCOLS+15,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+48,C'C'                                                  
         MVI   BOXCOLS+60,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+111,C'C'                                                 
         MVI   BOXCOLS+119,C'R'                                                 
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
BX500    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD DSECT                                      *         
***********************************************************************         
SRECD    DSECT                                                                  
SRKEY    DS    0CL4                                                             
SROFFICE DS    CL2                 OFFICE / FOR SORTING                         
SRPROV   DS    CL2                 PROVINCE / FOR SORTING                       
SREFFDTE DS    CL3                 EFFECTIVE DATE / FOR SORTING                 
SREND    DS    0C                                                               
SRECLNQ  EQU   *-SRECD                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0H                                                               
         DS    CL7                                                              
PLINOFF  DS    CL2                 OFFICE                                       
PLINDIV  DS    CL1                 /                                            
PLINPROV DS    CL2                 PROVINCE                                     
         DS    CL4                                                              
PLINDTE  DS    CL8                 EFFECTIVE DATE                               
         DS    CL3                                                              
PLINREG  DS    CL12                REGISTRATION NUMBER                          
         DS    CL5                                                              
PLINCODE DS    CL2                 CODE                                         
         DS    CL3                                                              
PLINTYPE DS    CL10                TYPE                                         
         DS    CL2                                                              
PLINACCD DS    CL12                ACCOUNT CODE                                 
         DS    CL2                                                              
PLINACNM DS    CL36                ACCOUNT NAME                                 
         DS    CL1                                                              
PLINRATE DS    CL6                 RATE                                         
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACGR02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
PRNTBL   DS    A                   PRNTBL                                       
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
CHOPCON  DS    A                   CHOPPER                                      
HELLO    DS    A                   HELLO                                        
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
ELCODE   DS    XL1                                                              
ELEM     DS    CL255               ELEMENT BUFFER                               
SVKEY    DS    CL42                                                             
*                                                                               
TEMPPRV  DS    XL2                 TEMPORARY PROVINCE FIELD                     
TEMPOFF  DS    XL2                  ""  ""   OFFICE FIELD                       
TEMPDATE DS    XL3                  ""  ""   EFFECTIVE DATE                     
SVPROV   DS    XL2                 SAVED PROVINCE                               
SVOFF    DS    XL2                 SAVE OFFICE                                  
SVEFFDT  DS    XL3                 SAVED EFFECTIVE DATE                         
SVREG    DS    CL12                SAVED REGISTRATIION NUMBER                   
SVREGLN  DS    XL1                 SAVED REGISTRATION # LENGTH                  
LAPBIT   DS    XL1                                                              
LAP1     EQU   X'80'                                                            
LAP2     EQU   X'40'                                                            
*                                                                               
BIT      DS    XL1                                                              
PRINPUT  EQU   X'80'               PRINTING INPUT RULES                         
*                                                                               
SREC     DS    CL(SRECLNQ)                                                      
WRK2     DS    CL120                                                            
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPGR02 12/10/12'                                      
         END                                                                    
