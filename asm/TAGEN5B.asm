*          DATA SET TAGEN5B    AT LEVEL 003 AS OF 05/29/15                      
*PHASE T7025BE,*                                                                
         TITLE 'T70218 - EVENT MAINTENANCE AND LIST'                            
T7025B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7025B,R7,R6                                              
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,AUPDINVS         SAVE A(UPDATED INVOICES)                     
         AHI   RE,L'UPDINVS                                                     
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,XRECADD        RECORD ADDEED                                
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JNE   XIT                                                              
         LA    RF,HOOK             SET A(HEADLINE HOOK)                         
         ST    RF,HEADHOOK                                                      
         LA    RF,MYSPECS          SET A(SPECS)                                 
         ST    RF,SPECS                                                         
         XC    COUNTER,COUNTER     CLEAR LINE COUNTER                           
         BRAS  RE,LR               GO LIST THE RECORDS                          
         J     XIT                                                              
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,EVLSELH,H4-1                                
***      GOTO1 (RF),(R1),EVLAGYH,EVLSELH,H7-5                                   
***      MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         J     XIT                                                              
         SPACE 2                                                                
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H8,1,C'--- --------     -------------------'                     
         SSPEC H8,45,C'--------------      ----'                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERALK    LA    R2,EVTAGYH                                                       
         MVI   ERROR,ERAGYLCK      AGENCY IS LOCKED                             
         J     END                                                              
                                                                                
ERCLK    LA    R2,EVTCLIH                                                       
         MVI   ERROR,ERCLILCK      CLIENT IS LOCKED                             
         J     END                                                              
                                                                                
ERPLK    LA    R2,EVTPRDH          PRODUCT IS LOCKED                            
         LHI   RE,ERPRDLCK                                                      
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
EREXI    MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         J     END                                                              
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
APERMIS  LA    R2,EVTAPERH         MISSING ADD PERIOD FIELD                     
         J     ERMIS                                                            
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERIST    MVI   ERROR,ERRECCTY      INVALID SCREEN FOR COMML TYPE                
         J     END                                                              
                                                                                
ERNDE    MVI   ERROR,ERINVDEL      RECORD CANNOT BE DELETED                     
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
MORE     LA    R2,EVTCLIH                                                       
         MVC   SVINPKEY,KEY                                                     
         LHI   RE,261                                                           
         STH   RE,MYMSGNO          MORE PAY PERIODS TO DISPLAY                  
         J     INFEND                                                           
                                                                                
NOMORE   LA    R2,EVTCLIH                                                       
         XC    SVINPKEY,SVINPKEY                                                
         LHI   RE,262              NO MORE PAY PERIODS TO DISPLAY               
         STH   RE,MYMSGNO                                                       
         J     INFEND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         XC    TGFSTREC,TGFSTREC                                                
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         JNE   INIT30                                                           
         LA    R2,EVLSELH                                                       
         LA    R3,EVLLSELH                                                      
INIT10   CLI   5(R2),0                                                          
         JE    INIT20                                                           
         OC    8(L'EVLSEL,R2),SPACES                                            
         CLC   =C'S  ',8(R2)                                                    
         JE    INIT20                                                           
         CLC   =C'C  ',8(R2)                                                    
         JE    INIT20                                                           
         CLC   =C'DE ',8(R2)                                                    
         JNE   ERINV                                                            
INIT20   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         JL    INIT10                                                           
         LA    RF,PFLTAB                                                        
         J     INIT80                                                           
                                                                                
INIT30   XR    R0,R0                                                            
         LA    R2,EVTSELH                                                       
         LA    R3,EVTLCRDH                                                      
INIT40   CLI   5(R2),0                                                          
         JE    INIT50                                                           
         LTR   R0,R0                                                            
         JNZ   ERINV                                                            
         AHI   R0,1                                                             
INIT50   OC    8(L'EVTSEL,R2),SPACES                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         JL    INIT40                                                           
                                                                                
         LTR   R0,R0                                                            
         JNZ   INIT60                                                           
         TM    PROSTAT,PSDELPRO    IF DELETION IS NOT IN PROGRESS               
         JO    INIT70                                                           
         CLI   CALLSP,0            AND STACK IS POPULATED                       
         JE    INIT70                                                           
         CLI   PFAID,0             AND PFKEY IS NOT HIT                         
         JNE   INIT70                                                           
         OC    SVINPKEY,SVINPKEY   AND NO MORE PAY PERIODS TO DISPLAY           
         JNE   INIT70                                                           
         GOTO1 FLDVAL,DMCB,(X'40',EVTAGYH),(X'80',999)                          
         JNE   INIT70                                                           
         OI    TRNSTAT,OKINTPFK    AND NO FIELDS HAVE BEEN CHANGED              
         MVI   PFAID,24            SET TO GO TO NEXT ENTRY IN STACK             
         J     INIT70                                                           
                                                                                
INIT60   LA    R2,EVTSELH                                                       
         STCM  R2,15,TGFSTREC                                                   
                                                                                
INIT70   LA    RF,PFTAB                                                         
                                                                                
INIT80   GOTO1 INITIAL,DMCB,(RF)                                                
                                                                                
         LA    RE,STATAB                                                        
         ST    RE,ASTATAB          SAVE A(EVENT STATUS TABLE)                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        PF KEY TABLE                                                 *         
***********************************************************************         
                                                                                
PFTAB    DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'EVENT   ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG)                                  
         DC    AL1((PF23X-PF23)/KEYLNQ,0)                                       
         DC    CL3'S ',CL8'EVTIME  ',CL8'LIST  '                                
PF23     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'EVTINV-1),AL2(EVTINV-EVTPER)                      
PF23X    EQU   *                                                                
         DC    AL1(PF24X-*,24,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
PF24X    DC    X'FF'                                                            
                                                                                
PFLTAB   DC    AL1(PFL22X-*,22,PFTINT+PFTCPROG)                                 
         DC    AL1((PFL22X-PFL22)/KEYLNQ,0)                                     
         DC    CL3'S ',CL8'EVENT   ',CL8'DISP'                                  
PFL22    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LEVT-1),AL2(LEVT-LISTD)                           
PFL22X   EQU   *                                                                
         DC    AL1(PFL23X-*,23,PFTINT+PFTCPROG)                                 
         DC    AL1((PFL23X-PFL23)/KEYLNQ,0)                                     
         DC    CL3'C ',CL8'EVENT   ',CL8'CHANGE'                                
PFL23    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LEVT-1),AL2(LEVT-LISTD)                           
PFL23X   EQU   *                                                                
         DC    AL1(PFL24X-*,24,PFTINT+PFTCPROG)                                 
         DC    AL1((PFL24X-PFL24)/KEYLNQ,0)                                     
         DC    CL3'DE',CL8'EVENT   ',CL8'DELETE'                                
PFL24    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LEVT-1),AL2(LEVT-LISTD)                           
PFL24X   DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        COMMERCIAL STATUS TABLE                                      *         
***********************************************************************         
                                                                                
STATAB   DS    0CL11                                                            
         DC    AL1(TACOSTLO),CL10'LOCKED'        LOCKED                         
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VKMAINT                                                       
         BAS   RE,VKLIST                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR MAINTENANCE SCREEN               *         
***********************************************************************         
                                                                                
VKMAINT  NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JE    XIT                                                              
                                                                                
         CLC   ACTNUM,TWALACT                                                   
         JE    VKM10                                                            
         NI    EVTAGYH+4,X'DF'                                                  
                                                                                
VKM10    GOTO1 FLDVAL,DMCB,(X'40',EVTAGYH),(X'80',EVTEVTH)                      
         JE    XIT                                                              
                                                                                
         LA    R3,KEY                                                           
         L     R4,AIO                                                           
                                                                                
         GOTO1 VKAGY,DMCB,EVTAGYH                                               
         BAS   RE,VKEVT                                                         
         MVC   SVCOKEY,KEY                                                      
                                                                                
         GOTO1 FLDVAL,DMCB,(2,EVTAGYH),EVTEVTH                                  
         GOTO1 (RF),(R1),(X'20',EVTAGYH),(X'80',EVTEVTH)                        
         NI    PROSTAT,X'FF'-PSDELPRO                                           
         XC    SVINPKEY,SVINPKEY                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR LIST SCREEN                      *         
***********************************************************************         
                                                                                
VKLIST   NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(X'40',EVLAGYH),(X'80',EVLOPTSH)                     
         JE    XIT                                                              
                                                                                
         LHI   RF,TIEND-TASYSIOD                                                
         XCEFL TASYSIOD,(RF)                                                    
         XC    TIFJOB,TIFJOB                                                    
         XC    OPTS,OPTS                                                        
                                                                                
         L     R4,AIO                                                           
         GOTO1 VKAGY,DMCB,EVLAGYH                                               
         MVC   TIFAGY,TGAGY                                                     
                                                                                
         BAS   RE,VKCLI                                                         
         BAS   RE,VKPRD                                                         
         BAS   RE,VKFMT                                                         
         BAS   RE,VKSTR                                                         
         BAS   RE,VKJOB                                                         
         BAS   RE,VKOPT                                                         
                                                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',EVLAGYH),(X'80',EVLOPTSH)                     
         NI    PROSTAT,X'FF'-PSDELPRO                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY                                   *         
*        ON ENTRY ... P1=A(AGENCY FIELD)                                        
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKAGY    NTR1                                                                   
         L     R2,0(R1)                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'22',(R2))                                 
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            ENSURE AGENCY IS PRODUCTIONS                 
         JE    *+6                 PLUS AGENCY                                  
         DC    H'00'                                                            
         MVC   AYSTAT3,TAAYSTA3                                                 
                                                                                
         TM    TAAYSTA7,TAAYSPPL                                                
         JZ    ERINV                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE EVENT ID                                 *         
*        ON ENTRY ... R3=A(KEY)                                       *         
*                     R4=A(I/O AREA 1)                                *         
***********************************************************************         
                                                                                
VKEVT    NTR1                                                                   
         LA    R2,EVTEVTH          R2=A(EVENT ID FIELD)                         
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         JNE   VKE10                                                            
         GOTO1 ANY                                                              
                                                                                
VKE10    CLI   8(R2),C' '                                                       
         JE    ERINV                                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'24',(R2))                                
         JE    VKE20                                                            
                                                                                
         CLI   ACTNUM,ACTADD       IF EVENT DOES NOT EXIST                      
         JNE   ERNFD               ENSURE THAT ACTION IS ADD                    
         MVC   KEY,KEYSAVE                                                      
         J     XIT                                                              
                                                                                
VKE20    CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    EREXI               ENSURE THAT RECORD DOES NOT EXIST            
                                                                                
         USING TLCOPD,R3                                                        
         MVC   TGCOM,TLCOICOM      IF EVENT DOES EXIST, SAVE INT NUMBER         
         DROP  R3                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOMED,TACOMEDE    ENSURE MEDIA IS EVENT                        
         JNE   ERIST                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE CLIENT                                   *         
***********************************************************************         
                                                                                
VKCLI    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'22',EVLCLIH)                              
         MVC   TIFCLI,TGCLI                                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE SORT BY                                  *         
***********************************************************************         
                                                                                
VKPRD    NTR1                                                                   
         XC    TGPRD,TGPRD                                                      
         CLI   EVLPRDH+5,0                                                      
         JE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'22',EVLPRDH)                              
         MVC   TIFPRD,TGPRD                                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE SORT BY/FORMAT                           *         
***********************************************************************         
                                                                                
VKFMT    NTR1                                                                   
         MVI   TIREAD,TLCONCDQ                                                  
         CLI   EVLFMT,C'N'                                                      
         JE    XIT                                                              
         CLI   EVLFMTH+5,0                                                      
         JNE   VKF10                                                            
         MVI   EVLFMT,C'N'                                                      
         MVI   EVLFMTH+5,1                                                      
         MVI   EVLFMTH+6,X'80'                                                  
         J     XIT                                                              
                                                                                
VKF10    MVI   TIREAD,TLCOICDQ                                                  
         CLI   EVLFMT,C'C'                                                      
         JE    XIT                                                              
         LA    R2,EVLFMTH                                                       
         J     ERINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE START                                    *         
***********************************************************************         
                                                                                
VKSTR    NTR1                                                                   
         CLI   EVLSTRH+5,0                                                      
         JE    XIT                                                              
         MVC   TIQSTART(L'EVLSTR),EVLSTR                                        
         OC    TIQSTART,SPACES                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE JOB                                      *         
***********************************************************************         
                                                                                
VKJOB    NTR1                                                                   
         CLI   EVLJOBH+5,0                                                      
         JE    XIT                                                              
         MVC   TIFJOB,EVLJOB                                                    
         OC    TIFJOB,SPACES                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE OPTIONS                                  *         
***********************************************************************         
                                                                                
VKOPT    NTR1                                                                   
         CLI   EVLOPTSH+5,0                                                     
         JE    XIT                                                              
                                                                                
         LA    R2,EVLOPTSH         R2 = A(FIELD)                                
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         JZ    ERINV                                                            
                                                                                
VKO10    MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0                                                         
         JE    ERINV                                                            
                                                                                
         USING OPTD,R4                                                          
         LA    R4,OPTTAB                                                        
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
                                                                                
VKO20    EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   SCDATA1(0),OPTLHS                                                
         JE    VKO30                                                            
         LA    R4,OPTNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         JE    ERINV                                                            
         J     VKO20                                                            
                                                                                
VKO30    LH    RF,OPTDISP                                                       
         AR    RF,RB                                                            
         MVC   ERRDISP,SCDISP2                                                  
         BASR  RE,RF                                                            
                                                                                
VKO40    LA    R3,SCANNEXT                                                      
         BCT   R0,VKO10                                                         
                                                                                
VKOX     OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE LOCKED OPTION                            *         
***********************************************************************         
                                                                                
VKOLCK   DS    0H                                                               
         CLI   SCDATA2,C'Y'                                                     
         JE    VKOLX                                                            
         CLI   SCDATA2,C'N'                                                     
         JNE   ERINV                                                            
         BR    RE                                                               
VKOLX    OI    OPTS,OLOCKED                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
OPTTAB   DS    0H                                                               
         DC    CL10'LOCKED    ',AL2(VKOLCK-VK)                                  
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVC   EVTAGY,TLCOAGY                                                   
         MVI   EVTAGYH+5,L'EVTAGY                                               
         OI    EVTAGYH+6,X'80'                                                  
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   EVTEVT,TACOCID                                                   
         MVI   EVTEVTH+5,L'EVTEVT                                               
         OI    EVTEVTH+6,X'80'                                                  
         DROP  R4                                                               
                                                                                
         NI    EVTAGYH+4,X'DF'                                                  
         BRAS  RE,VK                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY KEY ROUTINES                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY COMMERCIAL RECORD)              *         
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RESTEVT                                                       
                                                                                
         GOTO1 FLDVAL,DMCB,(X'22',EVTCLIH),999                                  
         GOTO1 (RF),(R1),(1,EVTCLIH),(X'80',EVTAINVH)                           
         MVC   EVTCLIN,SPACES                                                   
         MVC   EVTPRDN,SPACES      CLEAR PROTECTED FIELDS                       
         MVC   EVTINTN,SPACES                                                   
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         BAS   RE,DISCLI           DISPLAY CLIENT FIELDS                        
         BAS   RE,DISPRD           DISPLAY PRODUCT FIELDS                       
         BAS   RE,DISCOM           DISPLAY INTERNAL COMMERCIAL NUMBER           
         GOTO1 CHAROUT,DMCB,TANAELQ,EVTTITNH                                    
         GOTO1 (RF),(R1),TANUELQ,EVTPOH,TANUTAUT                                
         GOTO1 (RF),(R1),TANUELQ,EVTJOBH,TANUTEST                               
         BAS   RE,DISSTA                                                        
         GOTO1 CHAROUT,DMCB,TACMELQ,EVTCOMMH,TACMTYPG                           
         GOTO1 ACTVOUT,DMCB,EVTLCHGH                                            
                                                                                
         LA    R2,EVTSELH                                                       
         LA    R5,EVTLCRDH                                                      
         GOTO1 FLDVAL,DMCB,(5,(R2)),(R5)                                        
                                                                                
         USING TLINPD,R3                                                        
         LA    R3,KEY                                                           
                                                                                
         OC    SVINPKEY,SVINPKEY                                                
         JZ    DR10                                                             
         MVC   KEY,SVINPKEY                                                     
         J     DR20                                                             
                                                                                
DR10     XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINECDQ                                                 
         MVC   TLINECOM,TLCOCOM                                                 
                                                                                
DR20     GOTO1 HIGH                                                             
         J     DR40                                                             
DR30     GOTO1 SEQ                                                              
DR40     CLC   KEY(TLINETPS-TLINPD),KEYSAVE                                     
         JNE   DR60                                                             
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         GOTO1 DATCON,DMCB,(X'11',TAINPTPD),(8,8(R2))                           
         OI    1(R2),X'20'                                                      
         OC    TAINPINF,TAINPINF                                                
         JNZ   *+8                                                              
         NI    1(R2),X'DF'                                                      
         MVI   5(R2),17                                                         
         DROP  R4                                                               
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   TGINV,TLINEINV                                                   
         XC    TGINV,DRHEXFFS                                                   
         GOTO1 TINVCON,DMCB,TGINV,8(R2),DATCON                                  
         MVI   5(R2),6                                                          
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
                                                                                
         TM    TLINESTA,TLINESCR                                                
         JZ    DR50                                                             
         MVC   8(2,R2),=C'CR'                                                   
         MVI   5(R2),2                                                          
         DROP  R3                                                               
                                                                                
DR50     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
                                                                                
         CR    R2,R5                                                            
         JL    DR30                                                             
         GOTO1 SEQ                                                              
                                                                                
DR60     GOTO1 FLDVAL,DMCB,(8,(R2)),(R5)                                        
                                                                                
         CLI   ACTNUM,ACTDEL                                                    
         JNE   DR70                                                             
         OI    PROSTAT,PSDELPRO                                                 
         J     XIT                                                              
                                                                                
DR70     CLC   KEY(TLINETPS-TLINPD),KEYSAVE                                     
         JE    MORE                                                             
         OC    SVINPKEY,SVINPKEY                                                
         JNZ   NOMORE                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY CLIENT VALUES                             *         
*        ON ENTRY ... R4 = A(EVENT RECORD)                            *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DISCLI   NTR1                                                                   
         MVC   EVTCLI,TLCOCLI      DISPLAY CLIENT CODE AND NAME                 
         MVI   EVTCLIH+5,L'EVTCLI                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'0C',EVTCLIH),EVTCLINH                     
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY PRODUCT VALUES                            *         
*        ON ENTRY ... R4 = A(EVENT RECORD)                            *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DISPRD   NTR1                                                                   
         OC    TLCOPRD,TLCOPRD     IF PRODUCT CODE PRESENT                      
         JZ    XIT                 DISPLAY PRODUCT CODE AND NAME                
         MVC   EVTPRD,TLCOPRD                                                   
         MVI   EVTPRDH+5,L'EVTPRD                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'0C',EVTPRDH),EVTPRDNH                     
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY INTERNAL COMMERCIAL NUMBER                *         
*        ON ENTRY ... R4 = A(EVENT RECORD)                            *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DISCOM   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER, DISPLAY                       
         JNE   XIT                 INTERNAL COMMERCIAL NUMBER                   
         MVI   EVTINTN,C'('                                                     
         GOTO1 HEXOUT,DMCB,TLCOCOM,EVTINTN+1,L'TGCOM,0                          
         MVI   EVTINTN+9,C')'                                                   
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY STATUS                                    *         
*        ON ENTRY ... R4 = A(EVENT RECORD)                            *         
***********************************************************************         
                                                                                
DISSTA   NTR1                                                                   
         XR    R0,R0               R0=L'STATUS FIELD                            
         LA    R2,EVTSTAT          R2=A(STATUS FIELD)                           
         L     R3,ASTATAB          R3=A(STATUS TABLE)                           
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   TACOSTAT,0                                                       
         JE    DSTA30                                                           
                                                                                
DSTA10   CLI   0(R3),X'FF'                                                      
         JE    DSTA30                                                           
         MVC   BYTE,0(R3)                                                       
         NC    BYTE,TACOSTAT                                                    
         JZ    DSTA20                                                           
         GOTO1 DISKYW,DMCB,1(R3)                                                
                                                                                
DSTA20   LA    R3,L'STATAB(R3)                                                  
         J     DSTA10                                                           
         DROP  R4                                                               
                                                                                
DSTA30   LTR   R0,R0                                                            
         JZ    XIT                                                              
         STC   R0,EVTSTATH+5                                                    
         LA    R2,EVTSTAT                                                       
         AR    R2,R0                                                            
         MVI   0(R2),C' '                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY STATUS KEYWORD                            *         
*        ON ENTRY ... P1 = A(STATUS KEYWORD LITERAL)                  *         
*                     R2 = A(STATUS FIELD)                            *         
***********************************************************************         
                                                                                
DISKYW   NTR1                                                                   
         L     R1,0(R1)                                                         
         AR    R2,R0                                                            
                                                                                
         LTR   R0,R0                                                            
         JZ    DKYW10                                                           
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
                                                                                
DKYW10   MVC   0(10,R2),0(R1)                                                   
                                                                                
DKYW20   CLI   0(R2),C' '                                                       
         JE    DKYWX                                                            
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
         J     DKYW20                                                           
                                                                                
DKYWX    XIT1  REGS=(R0)                                                        
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
DRHEXFFS DC    10X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         TM    AYSTAT3,TAAYSLCK    ENSURE AGENCY IS NOT LOCKED                  
         JO    ERALK                                                            
                                                                                
         LA    R3,KEY                                                           
         L     R4,AIO1                                                          
                                                                                
         XC    EVTCLIN,EVTCLIN                                                  
         XC    EVTPRDN,EVTPRDN                                                  
         GOTO1 FLDVAL,DMCB,(2,EVTCLINH),999                                     
                                                                                
         BAS   RE,VRINADD          INITIALIZE FOR ADD                           
         BAS   RE,VRINCHG          OR CHANGE                                    
                                                                                
         BAS   RE,VALCLI                                                        
         BAS   RE,VALPRD                                                        
         GOTO1 NAMIN,DMCB,TANAELQ,(X'20',EVTTITNH)                              
         GOTO1 (RF),(R1),TANUELQ,(X'80',EVTPOH),TANUTAUT                        
         GOTO1 (RF),(R1),TANUELQ,(X'80',EVTJOBH),TANUTEST                       
         BAS   RE,VALSTA                                                        
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',EVTCOMMH),TACMTYPG                     
         GOTO1 ACTVIN,DMCB,0                                                    
                                                                                
         MVC   AIO,AIO3                                                         
         BAS   RE,GETCOM           GET INTERNAL COMMERCIAL NUMBER               
         BAS   RE,VALEPI           VALIDATE EXISTING PERIOD/INVOICES            
         BAS   RE,VUAPI            VALIDATE/UPDATE ADD PERIOD/INVOICE           
         BAS   RE,UPDEPI           UPDATE EXISTING PERIOD/INVOICES              
         MVC   AIO,AIO1                                                         
                                                                                
         BAS   RE,VRFNADD          FINISH ADD                                   
         BAS   RE,VRFNCHG          OR CHANGE                                    
         MVC   SVCOKEY(L'TLCOKEY),0(R4)                                         
         MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE FOR ADD                                *         
***********************************************************************         
                                                                                
VRINADD  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
                                                                                
         MVI   EVSTAT,0                                                         
                                                                                
         USING TACOD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACOEL,TACOELQ                                                   
         MVI   TACOLEN,TACOLNQ3                                                 
         MVC   TACOCID,EVTEVT      ADD COMMERCIAL DETAILS ELEMENT               
         OC    TACOCID,SPACES      WITH EVENT ID                                
         MVI   TACOMED,TACOMEDE    AND MEDIA EVENT                              
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD NEXT CAST SEQUENCE NUMBER                
         MVI   TANUEL,TANUELQ      ELEMENT TO COMMERCIAL RECORD                 
         MVI   TANULEN,TANULNQ1                                                 
         MVI   TANUTYPE,TANUTSEQ                                                
         MVC   TANUNXTC,=X'0001'                                                
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO INITIALIZE FOR CHANGE                             *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
***********************************************************************         
                                                                                
VRINCHG  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS CHANGE                          
         JE    XIT                                                              
                                                                                
         USING TLINPD,R3                                                        
         MVI   EVSTAT,0                                                         
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLINHINV-TLINPD),KEYSAVE                                     
         JNE   XIT                                                              
         OI    EVSTAT,EVPAID       SET PAID STATUS                              
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE CLIENT FIELD                             *         
*        ON ENTRY ... AIO1 = A(IN PROGRESS EVENT RECORD)              *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
VALCLI   NTR1                                                                   
         LA    R2,EVTCLIH                                                       
         GOTO1 ANY                                                              
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'2A',(R2)),EVTCLINH                        
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS CHANGE                          
         JE    VCLI10                                                           
         CLC   TLCOCLI,TGCLI       CLIENT CANNOT BE CHANGED                     
         JE    VCLI10                                                           
         TM    EVSTAT,EVPAID       IF EVENT HAS BEEN PAID                       
         JO    ERINV                                                            
                                                                                
VCLI10   MVC   TLCOCLI,TGCLI                                                    
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
         CLI   ACTNUM,ACTADD       IF ATTEMPTING TO ADD EVENT                   
         JNE   XIT                                                              
         L     R4,AIO3                                                          
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         TM    TACISTAT,TACISLCK   ENSURE CLIENT IS NOT LOCKED                  
         JO    ERCLK                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE PRODUCT FIELD                            *         
*        ON ENTRY ... AIO1 = A(IN PROGRESS EVENT RECORD)              *         
***********************************************************************         
                                                                                
VALPRD   NTR1                                                                   
         XC    TGPRD,TGPRD                                                      
                                                                                
         CLI   EVTPRDH+5,0                                                      
         JE    VPRD10                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'2A',EVTPRDH),EVTPRDNH                     
         MVC   AIO,AIO1                                                         
                                                                                
         USING TAPID,R4                                                         
VPRD10   CLI   ACTNUM,ACTADD       IF ATTEMPTING TO ADD EVENT                   
         JNE   VPRD20                                                           
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VPRD30                                                           
         TM    TAPISTAT,TAPISLCK   ENSURE PRODUCT IS NOT LOCKED                 
         JO    ERPLK                                                            
         J     VPRD30                                                           
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
VPRD20   CLC   TLCOPRD,TGPRD       PRODUCT CANNOT BE CHANGED                    
         JE    VPRD30                                                           
         TM    EVSTAT,EVPAID       IF EVENT HAS BEEN PAID                       
         JO    ERINV                                                            
                                                                                
VPRD30   L     R4,AIO1                                                          
         MVC   TLCOPRD,TGPRD                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE STATUS FIELD                             *         
*        ON ENTRY ... AIO1 = A(IN PROGRESS EVENT RECORD)              *         
***********************************************************************         
                                                                                
VALSTA   NTR1                                                                   
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   TACOSTAT,0          INITIALIZE STATUSES                          
                                                                                
         CLI   EVTSTATH+5,0        EXIT IF STATUS IS NOT PROVIDED               
         JE    XIT                                                              
                                                                                
         LA    R2,EVTSTATH         R2=A(STATUS FIELD)                           
         XC    HALF,HALF                                                        
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         JE    ERINV                                                            
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
                                                                                
         USING SCAND,R1                                                         
         LA    R1,BLOCK            R1=A(SCAN BLOCK)                             
                                                                                
VSTA10   L     RF,ASTATAB                                                       
VSTA20   CLI   0(RF),X'FF'                                                      
         JE    ERINV                                                            
                                                                                
         CLC   SCDATA1,1(RF)                                                    
         JE    VSTA30                                                           
         LA    RF,L'STATAB(RF)                                                  
         J     VSTA20                                                           
                                                                                
VSTA30   OC    TACOSTAT,0(RF)                                                   
         DROP  R4                                                               
                                                                                
VSTA40   ZIC   RF,SCLEN1                                                        
         ZIC   RE,SCLEN2                                                        
         LTR   RE,RE                                                            
         JZ    *+8                                                              
         AHI   RE,1                                                             
         LA    RF,1(RF,RE)                                                      
         AH    RF,HALF                                                          
         STH   RF,HALF                                                          
                                                                                
         LA    R1,SCANNEXT                                                      
         BCT   R0,VSTA10                                                        
         J     XIT                                                              
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO GET NEXT INTERNAL COMMERCIAL NUMBER               *         
***********************************************************************         
                                                                                
GETCOM   NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
                                                                                
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ SYSTEM RECORD                           
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZICM  R1,TASYLCOM,4       GET NEXT INTERNAL COMMERCIAL                 
         AHI   R1,1                NUMBER                                       
         STCM  R1,15,TGCOM                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE EXISTING PERIOD/INVOICE FIELDS           *         
***********************************************************************         
                                                                                
VALEPI   NTR1                                                                   
         USING UPDINVSD,R5                                                      
         L     R5,AUPDINVS         R5=A(UPDATED INVOICES)                       
         MVI   0(R5),X'FF'                                                      
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS NOT ADD                         
         JE    XIT                                                              
                                                                                
         LA    R2,EVTSELH          R2=A(FIRST ETIME FIELD)                      
                                                                                
VEPI10   ZIC   RF,0(R2)            BUMP TO PAY PERIOD FIELD                     
         AR    R2,RF                                                            
         TM    1(R2),X'20'         IF IT'S PROTECTED ...                        
         JZ    VEPI30                                                           
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO INVOICE FIELD                        
VEPI20   ZIC   RF,0(R2)                                                         
         AR    R2,RF               THEN NEXT CREDIT FIELD                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               THEN NEXT ETIME FIELD                        
         LA    RF,EVTLCRDH                                                      
         CR    R2,RF               IF WE'RE NOT AT END OF SCREEN                
         JL    VEPI10              GO PROCESS NEXT LINE                         
         J     XIT                                                              
                                                                                
VEPI30   CLI   5(R2),0             IF PAY PERIOD HAS BEEN                       
         JNE   VEPI40              CLEARED                                      
                                                                                
         ZIC   RF,0(R2)            BUMP TO INVOICE FIELD                        
         AR    R2,RF                                                            
         BAS   RE,GETINV           GET INVOICE RECORD                           
         BAS   RE,ENSNPAID         ENSURE THAT IT HAS NOT BEEN PAID             
                                                                                
         XC    UIPER,UIPER                                                      
         MVC   UIINV,TGINV         AND ADD TO UPDATED INVOICE TABLE             
         LA    R5,UILNQ(R5)                                                     
         MVI   0(R5),X'FF'                                                      
         J     VEPI20                                                           
                                                                                
VEPI40   GOTO1 PDVAL,DMCB,BLOCK    ELSE, VALIDATE PAY PERIOD                    
                                                                                
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         ZICM  RF,PVALNDYS,2                                                    
         CHI   RF,365                                                           
         JH    ERINV                                                            
                                                                                
         ZIC   RF,0(R2)            BUMP TO INVOICE FIELD                        
         AR    R2,RF                                                            
         BAS   RE,GETINV           GET INVOICE RECORD                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAINPTPD,PVALPSTA   IF PAY PERIOD IS CHANGING                    
         JE    VEPI20                                                           
         BAS   RE,ENSNPAID         ENSURE THAT IT HAS NOT BEEN PAID             
         MVC   UIPER,PVALPSTA      AND ADD TO UPDATED INVOICE TABLE             
         MVC   UIINV,TGINV                                                      
         LA    R5,UILNQ(R5)                                                     
         MVI   0(R5),X'FF'                                                      
         J     VEPI20                                                           
         DROP  R3,R4,R5                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO READ INVOICE RECORD IN FIELD POINTED AT BY R2     *         
***********************************************************************         
                                                                                
GETINV   NTR1                                                                   
         GOTO1 TINVCON,DMCB,8(R2),TGINV,DATCON                                  
         CLI   0(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE ENSURE INVOICE IN AIO3 IS NOT PAID                   *         
***********************************************************************         
                                                                                
ENSNPAID NTR1                                                                   
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      ENSURE THAT IT HAS NOT BEEN                  
         BRAS  RE,GETEL            PAID                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAINPINF,TAINPINF                                                
         JNZ   ERINV                                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE PERIOD/INVOICE FIELDS AND UPDATE         *         
*        INVOICE                                                      *         
***********************************************************************         
                                                                                
VUAPI    NTR1                                                                   
         CLI   EVTAPERH+5,0                                                     
         JNE   VUAPI10                                                          
         CLI   EVTAINVH+5,0                                                     
         JNE   APERMIS                                                          
         J     XIT                                                              
                                                                                
VUAPI10  LA    R2,EVTAPERH                                                      
         GOTO1 PDVAL,DMCB,BLOCK                                                 
                                                                                
         USING PERVALD,RE                                                       
         LA    RE,BLOCK                                                         
         ZICM  RF,PVALNDYS,2                                                    
         CHI   RF,365                                                           
         JH    ERINV                                                            
         DROP  RE                                                               
                                                                                
         LA    R2,EVTAINVH                                                      
         GOTO1 ANY                                                              
         GOTO1 TINVCON,DMCB,EVTAINV,TGINV,DATCON                                
         CLI   0(R1),X'FF'                                                      
         JE    ERINV                                                            
         XC    TGINV,=6X'FF'                                                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B4',0)                                    
         JNE   ERINV                                                            
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAINTMCO,TAINTMCO                                                
         JNZ   ERINV                                                            
         MVC   TAINTMCO,TGCOM                                                   
                                                                                
         USING PERVALD,RF                                                       
         LA    RF,BLOCK                                                         
         MVC   TAINPTPD,PVALPSTA                                                
         GOTO1 PUTREC                                                           
         DROP  R4,RF                                                            
                                                                                
         GOTO1 ADDPTRS,DMCB,ASVPTRS                                             
         XC    SVINPKEY,SVINPKEY                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPDATE EXISTING PERIOD/INVOICE RECORDS            *         
***********************************************************************         
                                                                                
UPDEPI   NTR1                                                                   
         USING UPDINVSD,R5                                                      
         L     R5,AUPDINVS         R5=A(UPDATED INVOICES)                       
UEPI10   CLI   0(R5),X'FF'                                                      
         JE    XIT                                                              
                                                                                
         XC    SVINPKEY,SVINPKEY                                                
                                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B4',UIINV)                                
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         OC    UIPER,UIPER                                                      
         JNZ   UEPI20                                                           
         XC    TAINTMCO,TAINTMCO                                                
         XC    TAINPTPD,TAINPTPD                                                
         J     UEPI30                                                           
                                                                                
UEPI20   MVC   TAINPTPD,UIPER                                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
UEPI30   GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,ASVPTRS                                             
                                                                                
         LA    R5,UILNQ(R5)                                                     
         J     UEPI10                                                           
         DROP  R5                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO ADD EVENT RECORD                                  *         
*        ON ENTRY ... AIO1 = A(IN PROGRESS EVENT RECORD)              *         
***********************************************************************         
                                                                                
VRFNADD  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ SYSTEM RECORD                           
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TASYLCOM,TGCOM      UPDATE SYSTEM RECORD                         
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO1             FINSH BUILDING RECORD KEY                    
         MVI   TLCOCD,TLCOCDQ      WITH RECORD CODE                             
         MVC   TLCOAGY,TGAGY       AGENCY CODE                                  
         MVC   TLCOCID,EVTEVT      EVENT ID                                     
         OC    TLCOCID,SPACES                                                   
         MVC   TLCOCOM,TGCOM       AND INTERNAL COMMERCIAL NUMBER               
         DROP  R4                                                               
                                                                                
         BAS   RE,MYADDREC                                                      
         GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
                                                                                
         NI    EVTEVTH+4,X'FF'-X'20'                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO CHANGE EVENT RECORD                               *         
*        ON ENTRY ... AIO1 = A(IN PROGRESS EVENT RECORD)              *         
***********************************************************************         
                                                                                
VRFNCHG  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF ACTION IS CHANGE                          
         JE    XIT                                                              
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'20',0)                                   
         JE    *+6                 REREAD EVENT TO AVOID PUTREC                 
         DC    H'00'               DRAMA                                        
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         USING TLCOD,R2                                                         
         L     R2,AIO3             R2=A(ORIGINAL EVENT RECORD)                  
                                                                                
***********************************************************************         
                                                                                
         L     R4,AIO1                                                          
         CLC   TLCOKEY,0(R4)       IF KEY IS NOT CHANGING                       
         JNE   VRFC10                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              PUT EVENT RECORD                             
         J     VRFC20                                                           
                                                                                
***********************************************************************         
                                                                                
VRFC10   OI    TLCOSTAT,X'80'      IF KEY IS CHANGING                           
         GOTO1 PUTREC              DELETE ORIGINAL EVENT RECORD                 
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
         DROP  R2                                                               
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         L     RE,ASVPTRS                                                       
         XC    0(255,RE),0(RE)     ADD UPDATED EVENT RECORD                     
         BAS   RE,MYADDREC                                                      
                                                                                
***********************************************************************         
                                                                                
VRFC20   GOTO1 ADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO ADD RECORD                                        *         
*        ON ENTRY ... AIO1 = A(RECORD TO ADD)                         *         
***********************************************************************         
                                                                                
MYADDREC NTR1                                                                   
         USING TLRCD,R4                                                         
         L     R4,AIO1                                                          
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,TLRCKEY     SET KEY WITH RECORD KEY                      
         MVC   TLDRSTAT,TLRCSTAT                                                
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                                                             
                                                                                
         CLC   TLDRKEY,KEYSAVE     IF RECORD IS FOUND                           
         JNE   MYAR10                                                           
         TM    TLDRSTAT,X'80'      IT MUST BE DELETED                           
         JO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     WRITE BACK KEY IN UNDELETED STATUS           
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD                                  
         GOTO1 WRITE                                                            
         DROP  R3                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         NI    TLRCSTAT,X'7F'      PUT BACK IN UNDELETED STATUS                 
         GOTO1 PUTREC                                                           
         J     MYARX                                                            
         DROP  R4                                                               
                                                                                
MYAR10   MVC   KEY,KEYSAVE         IF RECORD WAS NOT FOUND                      
         GOTO1 ADDREC              ADD IT                                       
                                                                                
MYARX    NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DELETES COMMERCIAL/VERSION                           *         
***********************************************************************         
                                                                                
         USING TLCOD,R4                                                         
DE       NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY              R3=A(KEY)                                    
         L     R4,AIO              R4=A(I/O AREA)                               
         NI    DMINBTS,X'F7'                                                    
                                                                                
         USING TLCAD,R3                                                         
         XC    KEY,KEY             ENSURE NO EMPLOYEES ATTACHED TO              
         MVI   TLCACD,TLCACDQ      EVENT                                        
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACOM+L'TLCACOM-TLCAKEY),KEYSAVE                           
         JE    ERNDE                                                            
         DROP  R3                                                               
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY             ENSURE NO INVOICES ATTACHED TO               
         MVI   TLINPCD,TLINECDQ    EVENT                                        
         MVC   TLINECOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLINETPS-TLINPKEY),KEYSAVE                                   
         JE    ERNDE                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         OI    TLCOSTAT,X'80'      DELETE EVENT                                 
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPTRS),AUPPTRS                             
         DROP  R4                                                               
                                                                                
         NI    PROSTAT,X'FF'-PSDELPRO                                           
         MVI   IOOPT,C'Y'                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DE ROUTINE                                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RESTORES EVENT RECORD IN AIO                         *         
***********************************************************************         
                                                                                
RESTEVT  NTR1  BASE=*,LABEL=*                                                   
         CLC   KEY(L'TLCOKEY),SVCOKEY                                           
         JE    XIT                                                              
         MVC   KEY,SVCOKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         CLI   MODE,VALREC                                                      
         JE    RE10                                                             
         CLI   MODE,RECDEL                                                      
         JNE   RE20                                                             
RE10     MVI   RDUPDATE,C'Y'                                                    
RE20     GOTO1 GETREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR RESTORE EVENT                                   *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         LA    R2,P                R2=A(DISPLAY LINE)                           
         AHI   R2,L'LSELECT                                                     
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TIQSKEY,TIQSKEY                                                  
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         OC    COUNTER,COUNTER     IF ANYTHING REPORTED                         
         BZ    LRX                                                              
         BRAS  RE,PRNTIT           SKIP A LINE                                  
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         GOTO1 FLDVAL,DMCB,(X'01',EVLLSTH),(X'80',EVLLAST)                      
         B     LRX                                                              
LRX      J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         USING LISTD,R2                                                         
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
                                                                                
         USING TACOD,R4                                                         
         TM    OPTS,OLOCKED                                                     
         JO    LRH10                                                            
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         TM    TACOSTAT,TACOSTLO                                                
         JO    NO                                                               
         DROP  R4                                                               
                                                                                
LRH10    MVC   LEVT,TICID                                                       
         MVC   LNAM,TINAME                                                      
                                                                                
         MVC   AIO,TIAREC                                                       
         MVI   FLDWHDR,L'FLDWHDR                                                
         MVC   FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         GOTO1 CHAROUT,DMCB,TANUELQ,FLDWHDR,TANUTEST                            
                                                                                
         OC    TIFJOB,TIFJOB                                                    
         JZ    LRH20                                                            
         OC    FLDWHDR+8(L'FLDWHDR-8),SPACES                                    
         CLC   TIFJOB,FLDWHDR+8                                                 
         JNE   NO                                                               
                                                                                
LRH20    MVC   LJOB,FLDWHDR+8                                                   
                                                                                
         OC    TIPRD,TIPRD                                                      
         JZ    LRH40                                                            
         MVC   LPRD,TIPRD                                                       
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A6',LPRD)                                 
         JNE   LRH30                                                            
         GOTO1 CHAROUT,DMCB,TANAELQ,FLDWHDR                                     
         MVC   LPRDN,FLDWHDR+8                                                  
LRH30    MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
                                                                                
LRH40    MVC   DMDSKADD,TIDSKADD                                                
         CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    DISP50                                                           
         GOTO1 LISTMON                                                          
         J     YES                                                              
DISP50   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LH    RE,COUNTER          DISPLAY LINE TO REPORT                       
         AHI   RE,1                                                             
         STH   RE,COUNTER                                                       
LRHX     J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR LIST RECORD ROUTINES                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR64D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR65D                                                       
                                                                                
         DS    D                                                                
                                                                                
COUNTER  DS    H                   RECORD COUNTER                               
PROSTAT  DS    XL1                 PROGRAM STATUS                               
PSDELPRO EQU   X'80'               DELETION IN PROGRESS                         
                                                                                
AUPDINVS DS    A                   A(UPDATED INVOICES)                          
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPTRS  DS    A                   A(UPDATED POINTER BLOCK)                     
                                                                                
ASTATAB  DS    A                   A(STATUS TABLE)                              
                                                                                
SVCOKEY  DS    XL(L'KEY)           SAVED EVENT KEY                              
SVINPKEY DS    XL(L'KEY)           SAVED INVOICE KEY                            
                                                                                
EVSTAT   DS    XL1                 EVENT STATUS                                 
EVPAID   EQU   X'80'               EVENT HAS BEEN PAID                          
                                                                                
AYSTAT3  DS    XL(L'TAAYSTA3)      AGENCY STATUS 3                              
                                                                                
TIFJOB   DS    XL(L'EVLJOB)        JOB FILTER                                   
OPTS     DS    X                   LIST OPTIONS                                 
OLOCKED  EQU   X'80'               INCLUDE LOCKED RECORDS                       
                                                                                
FLDWHDR  DS    XL(8+L'LJOB)                                                     
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT TO COVER LIST LINE                                    *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
LSELECT  DS    0CL4                                                             
LEVT     DS    CL12                EVENT ID                                     
         DS    CL1                                                              
LNAM     DS    CL26                EVENT NAME                                   
         DS    CL1                                                              
LPRD     DS    CL6                 PRODUCT/REGION                               
         DS    CL1                                                              
LPRDN    DS    CL12                PRODUCT/REGION NAME                          
         DS    CL1                                                              
LJOB     DS    CL16                JOB                                          
                                                                                
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
UPDINVS  DS    XL((100*UILNQ)+1)     UPDATED INVOICES                           
SVPTRBLK DS    CL((50*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                       
UPPTRBLK DS    CL((50*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS           
TMPLNQ   EQU   *-TMPD                                                           
                                                                                
**********************************************************************          
*        DSECT FOR UPDATED INVOICE TABLE                             *          
**********************************************************************          
                                                                                
UPDINVSD DSECT                                                                  
UIPER    DS    XL(L'TAINPTPD)                                                   
UIINV    DS    XL(L'TGINV)                                                      
UILNQ    EQU   *-UPDINVSD                                                       
                                                                                
**********************************************************************          
*        DSECT FOR OPTIONS TABLE                                     *          
**********************************************************************          
                                                                                
OPTD     DSECT                                                                  
OPTLHS   DS    CL10                                                             
OPTDISP  DS    AL2                                                              
OPTNEXT  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEN5B   05/29/15'                                      
         END                                                                    
