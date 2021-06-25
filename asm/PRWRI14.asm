*          DATA SET PRWRI14    AT LEVEL 074 AS OF 01/08/13                      
*PHASE T40514A,*                                                                
*INCLUDE PPBILEXP                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE GETCOST                                                                
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INITIALIZATION-INIT'                
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
T40514   CSECT                                                                  
         NMOD1 0,T40514                                                         
*                                                                               
         L     R6,WBIWORKA         POINT TO PROGRAM WORKING STORAGE             
         USING WBIWORKD,R6         ESTABLISH WORKING STORAGE                    
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      ESTABLISH SCREEN                             
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           ESTABLISH DDSPOOL STORAGE                    
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         L     RA,AGLOBAL                                                       
         USING GLOBALD,RA          ESTABLISH DRIVER STORAGE                     
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP OF NOT OFF-LINE                         
         BNE   OFFLINX                                                          
*                                                                               
*        RESERVED FOR OFF-LINE SPECIFIC CODE                                    
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)  FIND & SAVE ADDRESS OF SPFUSER         
         ST    R1,WSPFUSRA                                                      
*                                                                               
OFFLINX  DS    0H                                                               
*                                  REPORT CALLING MODE                          
         TITLE 'PRWRI14-BILL INTERFACE TAPE-CALLING MODE'                       
***********************************************************************         
*                                                                     *         
*        CHECK REPORT CALLING MODE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      PRNTIO HOOK                                  
         BE    PRIOHOOK                                                         
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BE    LST                                                              
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-REPORT INIT-INIT'                   
***********************************************************************         
*                                                                     *         
*        REPORT INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIT     DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        IF  OFF-LINE                                 
         BNE   INITX                                                            
*                                                                               
         CLI   TWAFIRST,0          IF FIRST REQUEST                             
         BNE   INIT2                                                            
*                                                                               
*        GET CORE FOR RECORD AREAS                                              
*                                                                               
         OC    WBIWORKA,WBIWORKA   CHECK FOR MULTIPLE ENTRY                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,WBIWORKX-WBIWORKD   AMOUNT OF CORE FOR GETMAIN                
         LA    R4,WBIWORKA            ADDRESS STORAGE AREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)   GET CORE                                   
*                                                                               
         LTR   RF,RF               CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,WBIWORKL         SAVE LENGTH GOTTEN                           
*                                                                               
         L     R6,WBIWORKA         ESTABLISH WORKING STORAGE                    
*                                                                               
*        GET CORE FOR INVOICE RECORD TABLE                                      
*                                                                               
         OC    WBIINVTA,WBIINVTA   CHECK FOR MULTIPLE ENTRY                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,=A(INVTENTL*100)   ALLOW FOR 100 ENTRY TABLE                  
         LA    R4,WBIINVTA            ADDRESS STORAGE AREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)   GET CORE                                   
*                                                                               
         LTR   RF,RF               CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,WBIINVTL         SAVE LENGTH GOTTEN                           
         L     R4,WBIINVTA         POINT TO RETURNED AREA                       
*                                                                               
         USING INVTBLD,R4          ESTABLISH INVOICE RECORD TABLE               
         XC    INVTBLD(INVTENTL),INVTBLD   INIT FIRST ENTRY                     
         MVC   WBIINVTX,WBIINVTA   INIT A(END OF TABLE)                         
*                                                                               
*        IF OFF-LINE BIFILE DCB WILL BE MOVED TO SPFUSER AREA                   
*              SET ITS ADDRESS IN WORKING STORAGE                               
*                                                                               
         L     R1,WSPFUSRA         POINT TO USER SAVEAREA                       
         LA    RF,BIFILE-SAVVALS(R1)  DCB DISPLACEMENT INTO SAVEAREA            
         ST    RF,WBIFILEA         SET NEW DCB ADDRESS                          
*                                                                               
*        MOVE DCB AND INTER-REQUEST VALUES TO SPFUSER AREA                      
*                                                                               
         ZAP   TAPECNT,=P'0'           INITIALIZE TAPE COUNT                    
         MVI   FRSTLAST,C'Y'           REQUEST RUNFRST/RUNLAST                  
*                                                                               
         L     R0,WSPFUSRA             SAVE INTER-REQUEST VALUES                
         LA    R1,SAVVALSL               AND DCB                                
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,WBIFILEA                                                      
*                                                                               
         OPEN  ((R2),OUTPUT)       OPEN INTERFACE TAPE                          
         LTR   RF,RF               NO ERRORS TOLERATED                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     INIT3                                                            
*                                                                               
INIT2    DS    0H                  ELSE                                         
*                                                                               
         L     RE,WSPFUSRA             RESTORE SAVED VALUES                     
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INIT3    DS    0H                  ELSE                                         
*                                                                               
         OI    PBQREAD,PBQRDBUY+PBQRDBLS BUYS & BILLS TO BE READ                
         OI    PBBYOPTS,PBBYBLLQ     READING BILL ELEMENTS                      
         OI    PBBYOPTS,PBBYBHDQ     READING BILL HEADERS                       
*                                                                               
         MVI   PBQPRTYP,PBQPRMOS     INDICATE PERIOD IS FOR                     
*                                      MONTH OF SERVICE                         
         MVI   MYFIRSTH,12         SET START OF HEADLINES                       
*                                                                               
INITX    B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-REQUEST VALIDATION-VALID'           
***********************************************************************         
*                                                                     *         
*        REQUEST VALIDATION                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALID    DS    0H                                                               
*                                                                               
         OI    PBQPOPT,PBQPODEL    INCLUDE DELETED BUYS                         
*                                                                               
         LA    R2,WRIDIVH          VALIDATE DIVISION ENTRY                      
         GOTO1 VALDIV                                                           
*                                                                               
         LA    R2,WRIREGH          VALIDATE REGION   ENTRY                      
         GOTO1 VALREG                                                           
*                                                                               
         LA    R2,WRIDISH          VALIDATE DISTRICT ENTRY                      
         GOTO1 VALDST                                                           
*                                                                               
         LA    R2,WRIADCH          VALIDATE AD CODE  ENTRY                      
         GOTO1 VALADC                                                           
*                                                                               
         LA    R2,WRITITH          VALIDATE TITLES   ENTRY                      
         GOTO1 VALTITS                                                          
*                                    MONTH OF SERVICE                           
         MVC   SUBTITLE(13),=C'BILLING FROM '                                   
         GOTO1 DATCON,DMCB,(3,PBQBST),(6,SUBTITLE+13)                           
         MVC   SUBTITLE+19(4),=C' TO '                                          
         GOTO1 DATCON,DMCB,(3,PBQBEND),(6,SUBTITLE+23)                          
*                                                                               
         OC    SUBTITLE,SPACES                                                  
*                                                                               
         GOTO1 CENTER,DMCB,SUBTITLE,29                                          
*                                                                               
         LA    R2,WRITITH                                                       
*                                                                               
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
VALIDX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-ERROR EXITS'                        
***********************************************************************         
*                                                                     *         
*        ERROR EXITS AND MESSAGES                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
*                                                                               
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-HOOK FROM PRINTIO-PRIOHOOK'         
***********************************************************************         
*                                                                     *         
*        PRINTIO HOOK                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRIOHOOK DS    0H                                                               
*                                                                               
*        DETERMINE TYPE OF HOOK                                                 
*                                                                               
         CLI   PBMODE,PBPROCCL     CLIENT FIRST                                 
         BE    PRIOCL                                                           
*                                                                               
PRIOHKX  DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-NEW CLIENT HOOK-PRIOCL'             
***********************************************************************         
*                                                                     *         
*        PRINTIO HOOK - NEW CLIENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRIOCL   DS    0H                                                               
*                                                                               
PRIOCLX  DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER HOOK-DRHOOK'                 
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ENTRY POINT                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHOOK   L     RA,AGLOBAL                                                       
         USING GLOBALD,RA                                                       
*                                                                               
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    DRHKRSLV                                                         
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRHKINIT                                                         
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    DRHKEXEC                                                         
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT                                  
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         TITLE 'PRWRI14-BILL INTERFACE TAPE-RESOLVE ADDR-DRHKRSLV'              
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                     *         
*                                                                     *         
*        SEARCH ROUTINE TABLE WHICH IS ORGANIZED AS                   *         
*           CL8  - ROUTINE LABLE                                      *         
*           AL4  - ROUTINE ADDRESS                                    *         
*         MATCH ON LABEL IN GLLABEL AND RETURN ADDRESS IN GLROUT      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHKRSLV DS    0H                                                               
*                                                                               
         L     R1,=A(DRROUTSA)     POINT TO ROUTINE LIST                        
*                                                                               
DHRSLOOP DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE IF END OF TABLE REACHED                 
         BE    DHRSLPDN                                                         
*                                                                               
         CLC   0(8,R1),GLLABEL     DONE IF ROUTINE LABEL FOUND                  
         BE    DHRSLPFD                                                         
*                                                                               
         LA    R1,12(R1)           BUMP TO NEXT ENTRY IN LIST                   
         B     DHRSLOOP                                                         
*                                                                               
DHRSLPFD DS    0H                                                               
*                                                                               
         MVC   GLAROUT,8(R1)       RETURN ROUTINE ADDRESS                       
*                                                                               
DHRSLPDN DS    0H                                                               
*                                                                               
DRHKRSLX DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER INIT-DRHKINIT'               
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE TO INITIALIZE DRIVER                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHKINIT DS    0H                                                               
         B     XIT                                                              
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER EXEC-DRHKEXEC'               
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE TO EXECUTE ROUTINES                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHKEXEC DS    0H                                                               
*                                                                               
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R4,GLADTENT         R4=A(DRIVE TABLE ENTRY)                      
*                                                                               
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BASR  RE,RF                                                            
*                                                                               
DRHKEXEX DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER PUTSRT-PUTSRT'               
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE WHEN RECORD ABOUT TO BE PUT TO SORT      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTSRT   DS    0H                                                               
         CLI   WSORTSW,C'N'        IF RECORD TO BE DELETED                      
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT          TELL DRIVER                               
*                                                                               
         B     XIT                                                              
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER HEADHOOK-HEADHK'             
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE WHEN HEADLINES TO BE PRINTED             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
HEADHK   DS    0H                                                               
*                                                                               
HEADHKX  B     XIT                                                              
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DRIVER LINE PRINT-PRINT'            
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE WHEN LINE TO BE PRINTED                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINT    DS    0H                                                               
         B     XIT                                                              
         TITLE 'PRWRI14-BILL INTERFACE TAPE-END REPORT-FINAL'                   
***********************************************************************         
*                                                                     *         
*        END OF REPORT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FINAL    L     R0,WSPFUSRA         SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALL1                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-END OF RUN-LST'                     
***********************************************************************         
*                                                                     *         
*        END OF REPORT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
LST      L     RE,WSPFUSRA         RESTORE SAVED VALUES                         
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,WBIFILEA         GET DCB ADDRESS                              
*                                                                               
         CLOSE ((R5))              CLOSE TAPE DATASET                           
*                                                                               
*        FREE CORE FOR INVOICE RECORD TABLE                                     
*                                                                               
         L     R3,WBIINVTL            AMOUNT OF CORE FOR GETMAIN                
         LA    R4,WBIINVTA            ADDRESS STORAGE AREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)   GET CORE                                  
*                                                                               
         LTR   RF,RF               CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FREE CORE FOR RECORD AREAS                                             
*                                                                               
         L     R3,WBIWORKL            AMOUNT OF CORE FOR GETMAIN                
         LA    R4,WBIWORKA            ADDRESS STORAGE AREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)   GET CORE                                  
*                                                                               
         LTR   RF,RF               CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LSTX     B     XIT                                                              
*                                                                               
WSPFUSRA DS    A                   A(SPFUSER)                                   
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-SAVED VARIABLES AND DCB'            
***********************************************************************         
*                                                                     *         
*        VALUES SAVED BETWEEN REQUESTS                                *         
*        TAPE DCB                                                     *         
*        WORKING STORAGE ADDRESS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SAVVALS  DS    0X                                                               
*                                                                               
WBIWORKA DC    A(0)                A(WORKING STORAGE)                           
WBIWORKL DC    F'0'                WORKING STORAGE LENGTH                       
*                                                                               
SAVVALL1 EQU   *-SAVVALS                                                        
*                                                                               
BIFILE   DCB   DDNAME=BIFILE,DSORG=PS,LRECL=508,BLKSIZE=2036,          X        
               MACRF=PM,RECFM=VB                                                
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-ROUTINES TABLE-DRROUTSA'            
***********************************************************************         
*                                                                     *         
*        TABLE FOR RESOLVING ROUTINE ADDRESSES FOR DRIVER             *         
*          CL8  - ROUTINE LABEL                                       *         
*          AL4  - ROUTINE ADDRESS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRROUTSA DS    0F                                                               
*                                                                               
         DC    CL8'FBIAGY  '       AGENCY  FIRST TIME                           
         DC    A(FBIAGY)                                                        
*                                                                               
         DC    CL8'FBIMED  '       MEDIA   FIRST TIME                           
         DC    A(FBIMED)                                                        
*                                                                               
         DC    CL8'FBICLT  '       CLIENT  FIRST TIME                           
         DC    A(FBICLT)                                                        
*                                                                               
         DC    CL8'IBIINV  '       INVOICE  INPUT                               
         DC    A(IBIINV)                                                        
         DC    CL8'FBIINV  '       INVOICE  FIRST TIME                          
         DC    A(FBIINV)                                                        
         DC    CL8'OBIINV  '       INVOICE  INPUT                               
         DC    A(OBIINV)                                                        
         DC    CL8'TBIINV  '       INVOICE  TOTAL                               
         DC    A(TBIINV)                                                        
*                                                                               
         DC    CL8'FBIDIV  '       DIVISION FIRST TIME                          
         DC    A(FBIDIV)                                                        
*                                                                               
         DC    CL8'FBIPRD  '       PRODUCT  FIRST TIME                          
         DC    A(FBIPRD)                                                        
*                                                                               
         DC    CL8'FBIEST  '       ESTIMATE FIRST TIME                          
         DC    A(FBIEST)                                                        
*                                                                               
         DC    CL8'FBIREG  '       REGION   FIRST TIME                          
         DC    A(FBIREG)                                                        
*                                                                               
         DC    CL8'FBIDST  '       DISTRICT FIRST TIME                          
         DC    A(FBIDST)                                                        
*                                                                               
         DC    CL8'FBIPUB  '       PUB      FIRST TIME                          
         DC    A(FBIPUB)                                                        
*                                                                               
         DC    CL8'FBIADC  '       AD CODE  FIRST TIME                          
         DC    A(FBIADC)                                                        
*                                                                               
         DC    CL8'FBIIDT  '       INSERT DATE FIRST TIME                       
         DC    A(FBIIDT)                                                        
*                                                                               
         DC    CL8'IBIDUM  '       DUMMY FIELD TO FORCE FIRST ON                
         DC    A(IBIDUM)             INSERTION DATE                             
*                                                                               
*        BILL HEADER FIELDS                                                     
*                                                                               
         DC    CL8'OBIIB#  '                                                    
         DC    A(OBIIB#)                                                        
*                                                                               
         DC    CL8'OBIDATE '       DATE OUTPUT ROUTINE                          
         DC    A(OBIDATE)                                                       
*                                                                               
         DC    CL8'OBIBTYP '       BILL TYPE OUTPUT ROUTINE                     
         DC    A(OBIBTYP)                                                       
*                                                                               
         DC    CL8'IBIBCHR '       BILL CHARACTERISTICS INPUT                   
         DC    A(IBIBCHR)                                                       
         DC    CL8'OBIBCHR '       BILL CHARACTERISTICS OUTPUT                  
         DC    A(OBIBCHR)                                                       
*                                                                               
*        BUY FIELDS                                                             
*                                                                               
         DC    CL8'IBISHR  '       SHARE INPUT                                  
         DC    A(IBISHR)                                                        
         DC    CL8'OBISHR  '       SHARE OUTPUT                                 
         DC    A(OBISHR)                                                        
*                                                                               
         DC    CL8'OBISPC  '       SPACE OUTPUT                                 
         DC    A(OBISPC)                                                        
*                                                                               
         DC    CL8'IBIURT  '       UNIT RATE INPUT                              
         DC    A(IBIURT)                                                        
         DC    CL8'OBIURT  '       UNIT RATE OUTPUT                             
         DC    A(OBIURT)                                                        
*                                                                               
         DC    CL8'IBIMOS  '       MONTH OF SERVICE INPUT                       
         DC    A(IBIMOS)                                                        
         DC    CL8'OBIMOS  '       MONTH OF SERVICE OUTPUT                      
         DC    A(OBIMOS)                                                        
*                                                                               
         DC    CL8'IBICOM  '       COMMENT          INPUT                       
         DC    A(IBICOM)                                                        
         DC    CL8'OBICOM  '       COMMENT          OUTPUT                      
         DC    A(OBICOM)                                                        
*                                                                               
         DC    CL8'IBIBFM  '       BILL FORMULA     INPUT                       
         DC    A(IBIBFM)                                                        
         DC    CL8'OBIBFM  '       BILL FORMULA     OUTPUT                      
         DC    A(OBIBFM)                                                        
*                                                                               
         DC    CL8'IBIDLR  '       DOLLAR AMOUNT    INPUT                       
         DC    A(IBIDLR)                                                        
         DC    CL8'OBIDLR  '       DOLLAR AMOUNT    OUTPUT                      
         DC    A(OBIDLR)                                                        
*                                                                               
         DC    CL8'IBIBLL  '       ACTUAL BILLED    INPUT                       
         DC    A(IBIBLL)                                                        
*                                                                               
         DC    CL8'IBIGST  '       BILLED GST       INPUT                       
         DC    A(IBIGST)                                                        
*                                                                               
         DC    CL8'IBIPRV  '       PREVIOUS BILLING INPUT                       
         DC    A(IBIPRV)                                                        
         DC    CL8'OBIPRV  '       PREVIOUS BILLING OUTPUT                      
         DC    A(OBIPRV)                                                        
*                                                                               
         DC    CL8'IBIEND  '       END OF RECORD ON INPUT                       
         DC    A(IBIEND)                                                        
         DC    CL8'OBIEND  '       END OF RECORD ON OUTPUT                      
         DC    A(OBIEND)                                                        
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-AGENCY FIRST-FBIAGY'                
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR AGENCY AFTER SORT                             *         
*          READ AGENCY RECORD                                         *         
*          PUT OUT AGENCY RECORD                                      *         
*                                                                     *         
*NTRY    R2==> XL10 TRUNCATED AGENCY NAME                             *         
*              CL1  MEDIA                                             *         
*              CL2  AGENCY CODE                                       *         
*              XL1  AGENCY CODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIAGY   NMOD1 0,**#FAG                                                         
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         XC    PRTSWS(PRTSWSL),PRTSWS   INIT RECORD WRITE SWITCHES              
*                                                                               
*        BUILD AGENCY OUTPUT RECORD                                             
*                                                                               
         XC    BIAGRECA(BIAGRECL),BIAGRECA   INIT AGENCY RECORD                 
         MVC   BIAGLREC,=Y(BIAGRECL)   SET AGENCY RECORD LENGTH                 
*                                                                               
         MVC   BIAGRID,=AL2(BIAGIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIAGRID+L'BIAGRID,FLDSEPQ                                        
*                                                                               
         MVC   BIAGAGY,11(R2)      SAVE AGENCY ID                               
*                                                                               
*        AGENCY NAME & ADDRESS FROM ID REC.                                     
*                                                                               
         XC    KEY,KEY             ESTABLISH CONTROL FILE KEY                   
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
*                                                                               
*        BUILD CONTROL FILE KEY                                                 
*                                                                               
         MVI   CTIKTYP,C'I'        AGENCY ID RECORD ID                          
         L     R1,ATWA                                                          
         MVC   CTIKID+8(2),10(R1)  AGENCY ID                                    
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'  SET FILE NAME                             
         MVI   USEIO,C'Y'          READ INTO IO AREA NOT KEY ISAM FILE          
         MVI   RDUPDATE,C'N'       DON'T READ FOR UPDATE                        
*                                                                               
         GOTO1 READ                READ AGENCY ID KEY                           
*                                                                               
         XC    FILENAME,FILENAME   RESET TO DEFAULT FILE                        
         MVI   USEIO,0             RESET TO READ INTO KEYAREA                   
*                                                                               
*                                  FIND DESTINATION ELEMENT                     
*                                                                               
         L     R4,AIO              POINT TO READ RECORD                         
         LA    R5,CTIDATA          POINT TO FIRST ELEMENT IN RECORD             
         SR    RF,RF                                                            
*                                                                               
FBIAGYLP DS    0H                                                               
*                                                                               
         USING CTDSTD,R5           ESTABLISH AS DESTINATION ELEMENT             
*                                                                               
         CLI   CTDSTEL,0           SKIP IF END OF RECORD REACHED                
         BE    FBIAGYDN                                                         
*                                                                               
         CLI   CTDSTEL,CTDSTELQ    LOOK FOR DESTINATION ELEMENT                 
         BE    FBIAGYFD            FOUND                                        
*                                                                               
FBIAGYCN DS    0H                                                               
*                                                                               
         IC    RF,CTDSTLEN         GET ELEMENT LENGTH                           
         LA    R5,CTDSTEL(RF)      BUMP TO NEXT ELEMENT                         
         B     FBIAGYLP                                                         
*                                                                               
FBIAGYFD DS    0H                                                               
*                                                                               
         MVC   BIAGNAME,CTDSTNAM   COPY AGENCY NAME                             
         MVI   BIAGNAME+L'BIAGNAME,FLDSEPQ                                      
*                                                                               
         MVC   BIAGADR1,CTDSTADD   COPY AGENCY ADDRESS                          
         MVI   BIAGADR1+L'BIAGADR1,FLDSEPQ                                      
*                                                                               
         CLI   CTDSTLEN,100        CONTINUE ONLY IF MORE ADDR AVAILABLE         
         BNH   FBIAGYDN                                                         
*                                                                               
         MVC   BIAGADR2,CTDSTAD2   COPY AGENCY ADDRESS                          
         MVI   BIAGADR2+L'BIAGADR2,FLDSEPQ                                      
*                                                                               
         MVC   BIAGADR3,CTDSTAD3   COPY AGENCY ADDRESS                          
         MVI   BIAGADR3+L'BIAGADR3,FLDSEPQ                                      
*                                                                               
FBIAGYDN DS    0H                                                               
*                                                                               
         XC    PPBILEXD(PPBXDL),PPBILEXD   INIT PPBILEXP CONTROL BLOCK          
         MVC   PPBXAGY,11(R2)      SET AGENCY IN PPBILEXP CONTROL BLK           
         MVC   PPBXACOM,PBCOMFAC   PASS A(COMFACS)                              
*                                                                               
*                                                                               
         CLC   BIAGAGY,WBIAGYSV    SKIP IF AGENCY UNCHANGED                     
         BE    FBIAGYPX                                                         
*                                                                               
         MVC   WBIAGYSV,BIAGAGY    SAVE AGENCY   CODE                           
         MVI   PRTAGY,C'Y'         INDICATE AGENCY RECORD TO WRITE              
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIMEDSV,WBIMEDSV   MEDIA                                        
         XC    WBICLTSV,WBICLTSV   CLIENT                                       
         XC    WBIINVSV,WBIINVSV   INVOICE                                      
         XC    WBIDIVSV,WBIDIVSV   DIVISION                                     
         XC    WBIPRDSV,WBIPRDSV   PRODUCT                                      
         XC    WBIESTSV,WBIESTSV   ESTIMATE                                     
         XC    WBIREGSV,WBIREGSV   REGION                                       
         XC    WBIDSTSV,WBIDSTSV   DISTRICT                                     
         XC    WBIPUBSV,WBIPUBSV   PUB                                          
         XC    WBIZNESV,WBIZNESV   ZONE                                         
         XC    WBIEDNSV,WBIEDNSV   EDITION                                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBIAGYPX DS    0H                                                               
*                                                                               
FBIAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-MEDIA FIRST-FBIMED'                 
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR MEDIA AFTER SORT                              *         
*          PUT OUT MEDIA RECORD                                       *         
*                                                                     *         
*NTRY    R2==> CL1  MEDIA                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIMED   NMOD1 0,**#FAG                                                         
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        FIND MEDIA IN BUFFER                                                   
*                                                                               
         L     R4,PBAMEDBF         POINT TO MEDIA BUFFER                        
         USING MEDBUFFD,R4         ESTABLISH AS MEDIA BUFFER ENTRY              
*                                                                               
FMELOOP  DS    0H                                                               
*                                                                               
         CLI   MBFMED,0            CAN'T HAVE END OF BUFFER                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MBFMED,0(R2)        MATCH MEDIA CODE                             
         BE    FMEFND                                                           
*                                                                               
FMECONT  DS    0H                                                               
*                                                                               
         LA    R4,MBFLEN(R4)       BUMP TO NEXT BUFFER ENTRY                    
         B     FMELOOP                                                          
*                                                                               
FMEFND   DS    0H                                                               
*                                                                               
*        BUILD MEDIA OUTPUT RECORD                                              
*                                                                               
         XC    BIMERECA(BIMERECL),BIMERECA   INIT MEDIA RECORD                  
         MVC   BIMELREC,=Y(BIMERECL)   SET MEDIA RECORD LENGTH                  
*                                                                               
         MVC   BIMERID,=AL2(BIMEIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIMERID+L'BIMERID,FLDSEPQ                                        
*                                                                               
         MVI   BIMESYST,C'P'       INDICATE PRINT SYSTEM                        
         MVI   BIMESYST+L'BIMESYST,FLDSEPQ                                      
*                                                                               
         MVC   BIMEMED,0(R2)       SET MEDIA CODE                               
         MVI   BIMEMED+L'BIMEMED,FLDSEPQ                                        
*                                                                               
         MVC   BIMENAME,MBFNAME    COPY MEDIA NAME                              
         MVI   BIMENAME+L'BIMENAME,FLDSEPQ                                      
*                                                                               
         MVC   PPBXMED,0(R2)       SET MEDIA IN PPBILEXP CONTROL BLOCK          
*                                                                               
         CLC   BIMEMED,WBIMEDSV    SKIP IF MEDIA  UNCHANGED                     
         BE    FBIMEDPX                                                         
*                                                                               
         MVC   WBIMEDSV,BIMEMED    SAVE MEDIA    CODE                           
         MVI   PRTMED,C'Y'         INDICATE MEDIA  RECORD TO WRITE              
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBICLTSV,WBICLTSV   CLIENT                                       
         XC    WBIINVSV,WBIINVSV   INVOICE                                      
         XC    WBIDIVSV,WBIDIVSV   DIVISION                                     
         XC    WBIPRDSV,WBIPRDSV   PRODUCT                                      
         XC    WBIESTSV,WBIESTSV   ESTIMATE                                     
         XC    WBIREGSV,WBIREGSV   REGION                                       
         XC    WBIDSTSV,WBIDSTSV   DISTRICT                                     
         XC    WBIPUBSV,WBIPUBSV   PUB                                          
         XC    WBIZNESV,WBIZNESV   ZONE                                         
         XC    WBIEDNSV,WBIEDNSV   EDITION                                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBIMEDPX DS    0H                                                               
*                                                                               
FBIMEDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-CLIENT FIRST-FBICLT'                
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR CLIENT AFTER SORT                             *         
*          READ CLIENT RECORD                                         *         
*          PUT OUT CLIENT RECORD                                      *         
*                                                                     *         
*NTRY    R2==> XL1  DATA LENGTH                                       *         
*              CL?  CLIENT NAME FOR SORTING                           *         
*              CL3  CLIENT CODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBICLT   NMOD1 0,**#FCL                                                         
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        READ CLIENT RECORD                                                     
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH AS CLIENT KEY                      
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKAGY,PBAGY      SET AGENCY ID                                
         MVC   PCLTKMED,PBMED      SET MEDIA                                    
         MVI   PCLTKRCD,PCLTKIDQ   SET CLIENT RECORD ID                         
*                                                                               
         SR    R1,R1               FIND CLIENT CODE                             
         IC    R1,0(R2)            TOTAL LENGTH OF CODE AND NAME                
         SH    R1,=H'3'            CODE LENGTH                                  
         LA    R1,0(R1,R2)         POINT TO CLIENT CODE                         
         MVC   PCLTKCLT,0(R1)      SET CLIENT ID                                
*                                                                               
         GOTO1 HIGH                READ CLIENT RECORD POINTER                   
*                                                                               
         CLC   PCLTKEY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ CLIENT RECORD                           
*                                                                               
         L     R4,AIO              SWITCH POINTERS                              
*                                                                               
*        BUILD CLIENT OUTPUT RECORD                                             
*                                                                               
         XC    BICLRECA(BICLRECL),BICLRECA   INIT CLIENT RECORD                 
         MVC   BICLLREC,=Y(BICLRECL)   SET CLIENT RECORD LENGTH                 
*                                                                               
         MVC   BICLRID,=AL2(BICLIDQ)   SET RECORD IDENTIFIER                    
         MVI   BICLRID+L'BICLRID,FLDSEPQ                                        
*                                                                               
         MVC   BICLCLT,PCLTKCLT    SET CLIENT CODE                              
         MVI   BICLCLT+L'BICLCLT,FLDSEPQ                                        
*                                                                               
         MVC   BICLNAME,PCLTNAME   COPY CLIENT NAME                             
         MVI   BICLNAME+L'BICLNAME,FLDSEPQ                                      
*                                                                               
*        FIGURE OUT CLIENT NUMBER - SEE PCLTRECD FOR DESCRIPTION                
*                                                                               
         CLI   PCLTNUM,X'FF'       CHECK FOR PACKED 4 DIGIT NMBR                
         BNE   FCLNMBR3                                                         
*                                                                               
         MVC   WORK(L'CNMPTRN),CNMPTRN SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'CNMPTRN),PCLTNUM+1  EDIT AND MARK NUMBER                  
*                                                                               
         LA    RF,WORK+L'CNMPTRN   CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BICLNMBR(0),0(R1)   MOVE TO RECORD                               
*                                                                               
         B     FCLNMBRX                                                         
*                                                                               
CNMPTRN  DC    X'4020202020'        CLIENT NUMBER EDIT PATTERN                  
*                                                                               
FCLNMBR3 DS    0H                                                               
*                                                                               
         MVC   DUB(1),PCLTNUM      COPY FIRST BYTE                              
*                                                                               
         NI    DUB,X'F0'           KILL SECOND NIBBLE                           
*                                                                               
         CLI   DUB,X'80'           EQUALITY MEANS 5 DIGIT BINARY                
         BNE   FCLNMBR7                                                         
*                                                                               
         MVC   DUB(3),PCLTNUM      COPY CLIENT NUMBER                           
*                                                                               
         NI    DUB,X'FF'-X'80'     KILL HIGH ORDER BIT                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,DUB            GET REAL CLIENT NUMBER                       
*                                                                               
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   WORK(L'CNM5PTRN),CNM5PTRN SET EDIT PATTERN                       
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'CNM5PTRN),DUB+5  EDIT AND MARK NUMBER                     
*                                                                               
         LA    RF,WORK+L'CNM5PTRN  CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BICLNMBR(0),0(R1)   MOVE TO RECORD                               
*                                                                               
         B     FCLNMBRX                                                         
*                                                                               
CNM5PTRN DC    X'402020202020'     EDIT PATTERN                                 
*                                                                               
FCLNMBR7 DS    0H                                                               
*                                                                               
         MVC   BICLNMBR(L'PCLTNUM),PCLTNUM    COPY NUMBER AS IS                 
*                                                                               
FCLNMBRX DS    0H                                                               
*                                                                               
         MVI   BICLNMBR+L'BICLNMBR,FLDSEPQ                                      
*                                                                               
         MVC   BICLOFC,PCLTOFF     COPY OFFICE CODE                             
         MVI   BICLOFC+L'BICLOFC,FLDSEPQ                                        
*                                                                               
         MVC   BICLAOFC,PCLTAOFC   COPY ACCOUNTING OFFICE CODE                  
         MVI   BICLAOFC+L'BICLAOFC,FLDSEPQ                                      
*                                                                               
         MVC   PPBXCLT,PCLTKCLT    SET CLIENT CODE   FOR PPBILEXP               
         MVC   PPBXOFF,PCLTOFF     SET CLIENT OFFICE FOR PPBILEXP               
*                                                                               
         CLC   BICLCLT,WBICLTSV    SKIP IF CLIENT UNCHANGED                     
         BE    FBICLTPX                                                         
*                                                                               
         MVC   WBICLTSV,BICLCLT    SAVE CLIENT   CODE                           
         MVI   PRTCLT,C'Y'         INDICATE CLIENT RECORD TO PRINT              
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIINVSV,WBIINVSV   INVOICE                                      
         XC    WBIDIVSV,WBIDIVSV   DIVISION                                     
         XC    WBIPRDSV,WBIPRDSV   PRODUCT                                      
         XC    WBIESTSV,WBIESTSV   ESTIMATE                                     
         XC    WBIREGSV,WBIREGSV   REGION                                       
         XC    WBIDSTSV,WBIDSTSV   DISTRICT                                     
         XC    WBIPUBSV,WBIPUBSV   PUB                                          
         XC    WBIZNESV,WBIZNESV   ZONE                                         
         XC    WBIEDNSV,WBIEDNSV   EDITION                                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBICLTPX DS    0H                                                               
*                                                                               
FBICLTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE INPUT-IBIINV'               
***********************************************************************         
*                                                                     *         
*        INVOICE NUMBER INPUT                                         *         
*                                                                     *         
*NTRY   AIO1 ==> BUY RECORD                                           *         
*                                                                     *         
*XIT    R2   ==> XL3   BILL DATE                                      *         
*                XL2   BILL NUMBER                                    *         
*                                                                     *         
*NTRY   AIO1 ==> BILL RECORD                                          *         
*                                                                     *         
*XIT    R2   ==> XL3   BILL DATE                                      *         
*                XL2   BILL NUMBER                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIINV   NMOD1 0,**#IIV                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         MVI   WSORTSW,0           RESET SORT SWITCH                            
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIIBLL                                                          
*                                                                               
*        GET INVOICE NUMBER FROM BUY BILLING ELEMENT                            
*                                                                               
         ICM   R5,15,PBBLLELA      POINT TO BILL ELEMENT                        
         BNZ   *+12                                                             
         MVI   WSORTSW,C'N'        DROP FROM SORT                               
         B     IBIINVX                                                          
*                                                                               
         USING PBILELD,R5          ESTABLISH BILL ELEMENT                       
*                                                                               
         MVC   0(3,R2),PBLDATE     RETURN BILL DATE                             
         MVC   3(2,R2),PBINVNO     RETURN BILL NUMBER                           
*                                                                               
         MVC   IBIBILST,PBLDATE    SET GETINS START AND END DATES               
         MVC   IBIBILEN,PBLDATE    SET GETINS START AND END DATES               
*                                                                               
*        GO TO GETINS TO RESET BUCKETS FOR BILL ELEMENT                         
*                                                                               
         MVC   PBGRS,=C'CRATE'     GET BUY FINANCIAL DATA                       
*                                                                               
         GOTO1 PAGETINS,DMCB,(C'B',AIO1),PBGRS,                        X        
               PBPRD,IBIBILST,=C'BOTH'                                          
*                                                                               
         B     IBIINVX                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        GET INVOICE NUMBER FROM BILL RECORD                                    
*                                                                               
IBIIBLL  DS    0H                                                               
*                                                                               
         XC    IBIBILA,IBIBILA     CLEAR BILLING ELEMENT ADDR                   
*                                                                               
         USING PBILLRCD,R4         ESTABLISH AS BILL HEADER                     
*                                                                               
         CLI   PBILKRCD,PBILKIDQ   SKIP IF NOT BILL HEADER                      
         BNE   IBIINVX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,PBILLDAT,(3,(R2))  BILL DATE                         
*                                                                               
         MVC   3(2,R2),PBILKBNO    RETURN BILL NUMBER                           
*                                                                               
*        SAVE BILL HEADER RECORD IN TABLE                                       
*                                                                               
         CLC   PBILKPRD,IBIIBLPR   IF A NEW PRODUCT                             
         BE    IBIIBL10                                                         
*                                                                               
         MVC   IBIIBLPR,PBILKPRD   SAVE PRODUCT CODE                            
*                                                                               
         L     RE,WBIINVTA         POINT TO START OF INVOICE TABLE              
*                                                                               
         XC    0(INVTENTL,RE),0(RE) CLEAR 1ST TABLE ENTRY                       
*                                                                               
         MVC   WBIINVTX,WBIINVTA   INIT NEXT AVAILABLE SLOT                     
*                                                                               
IBIIBL10 DS    0H                                                               
*                                                                               
         LR    RF,R4               SAVE RECORD POINTER                          
*                                                                               
         L     R4,WBIINVTX         POINT TO NEXT SLOT IN TABLE                  
*                                                                               
         MVC   0(INVTENTL,R4),0(RF) SAVE INVOICE RECORD                         
*                                                                               
         MVC   WORK+32(32),0(R4)                                                
         LA    R4,INVTENTL(R4)     POINT TO NEXT ENTRY AREA                     
         XC    0(INVTENTL,R4),0(R4) CLEAR NEXT AREA                             
*                                                                               
         ST    R4,WBIINVTX         SAVE ADDRESS                                 
*                                                                               
         MVC   WORK(32),0(R6)                                                   
IBIINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
IBIBILA  DC    A(0)                ADDRESS OF NEXT BUY BILELEM                  
*                                                                               
IBIBILST DS    XL3                 START DATE FOR GETINS                        
IBIBILEN DS    XL3                 END   DATE FOR GETINS                        
IBIIBLPR DC    CL3' '              CURRENT PRODUCT SAVEAREA                     
*                                                                               
IBIBILWK DS    CL32                WORKAREA                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE FIRST-FBIINV'               
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR INVOICE AFTER SORT                            *         
*                                                                     *         
*          INITIALIZE INVOICE HEADER RECORD                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIINV   NMOD1 0,**#FIV                                                         
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        BUILD INVOICE HEADER RECORD                                            
*                                                                               
         XC    BIIHRECA(BIIHRECL),BIIHRECA   INIT INV HDR RECORD                
         MVC   BIIHLREC,=Y(BIIHRECL)   SET INV HDR RECORD LENGTH                
*                                                                               
         MVC   BIIHRID,=AL2(BIIHIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIIHRID+L'BIIHRID,FLDSEPQ                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R2)),DUB   RE-FORMAT DATE                        
*                                                                               
         MVC   BIIHINV(2),DUB+2    BILL MONTH                                   
         SR    RF,RF                                                            
         ICM   RF,3,3(R2)          BILL NUMBER                                  
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  BIIHINV+2(4),DUB    BILL NUMBER                                  
*                                                                               
         MVI   BIIHINV+L'BIIHINV,FLDSEPQ                                        
*                                                                               
         XC    BIITRECA(BIITRECL),BIITRECA   INIT INV TOTAL RECORD              
         MVC   BIITLREC,=Y(BIITRECL)   SET INV TOTAL RECORD LENGTH              
*                                                                               
         MVC   BIITRID,=AL2(BIITIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIITRID+L'BIITRID,FLDSEPQ                                        
*                                                                               
         MVC   WORK,PPBILEXD                                                    
         GOTO1 DATCON,DMCB,(3,(R2)),PPBXBDT PASS BILL DATE TO PPBILEXP          
*                                                                               
         CLC   BIIHINV,WBIINVSV    SKIP IF INVOICE UNCHANGED                    
         BE    FBIINVPX                                                         
*                                                                               
         MVC   WBIINVSV,BIIHINV    SAVE INVOICE NUMBER                          
         MVI   PRTINV,C'Y'         INDICATE INVOICE RECORD TO PRINT             
*                                                                               
*                                  FORCE CHANGE IN NOTHING                      
*                                                                               
FBIINVPX DS    0H                                                               
*                                                                               
FBIINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE OUTPUT-OBIINV'              
***********************************************************************         
*                                                                     *         
*        OUTPUT FOR INVOICE HEADER RECORD                             *         
*NTRY   R2   ==> XL3   BILL DATE                                      *         
*                XL2   BILL NUMBER                                    *         
*                                                                     *         
*XIT    R3   ==> CL10  INVOICE NUMBER                                 *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIINV   NMOD1 0,**#OIV                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVC   0(07,R3),=C'INVOICE' INVOICE NUMBER                              
*                                                                               
         MVC   PPBXINV,BIIHINV     PASS INVOICE NUMBER TO PPBILEXP              
         GOTO1 DATCON,DMCB,(3,(R2)),PPBXBDT   PASS BILL DATE                    
*                                                                               
         MVC   WORK,PPBILEXD                                                    
         GOTO1 =V(PPBILEXP),DMCB,PPBILEXD  GET EXPANDED INV #                   
*                                                                               
         MVC   NAMEAREA-OUTAREA(10,R3),PPBXEXP EXPANDED BILL #                  
*                                                                               
OBIINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE TOTAL-TBIINV'               
***********************************************************************         
*                                                                     *         
*        TOTALS FOR INVOICE                                           *         
*                                                                     *         
*          SET TO WRITE TOTAL RECORD                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
TBIINV   NMOD1 0,**#TIV                                                         
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,GLADTENT         ESTABLISH DRIVETABLE ENTRY                   
         USING DRFLD,R4                                                         
*                                                                               
         ICM   R5,15,DRFLAPOS      GET PRINT POSITION                           
*                                                                               
         MVC   4(20,R5),=CL20'TOTALS FOR INVOICE'                               
*                                                                               
TBIINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DIVISION FIRST-FBIDIV'              
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR DIVISION                                      *         
*          PUT OUT DIVISION RECORD                                    *         
*                                                                     *         
*NTRY    R2==> CL3   DIVISION CODE                                    *         
*              CL20  DIVISION NAME                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIDIV   NMOD1 0,**#FDV                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        BUILD DIVISION OUTPUT RECORD                                           
*                                                                               
         CLI   0(R2),X'FF'         SKIP IF NO DIVISION FOUND                    
         BE    FBIDIVX                                                          
*                                                                               
         CLI   0(R2),0             SKIP IF NO DIVISION FOUND                    
         BE    FBIDIVX                                                          
*                                                                               
         XC    BIDVRECA(BIDVRECL),BIDVRECA   INIT RECORD                        
         MVC   BIDVLREC,=Y(BIDVRECL)   SET RECORD LENGTH                        
*                                                                               
         MVC   BIDVRID,=AL2(BIDVIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIDVRID+L'BIDVRID,FLDSEPQ                                        
*                                                                               
         MVC   BIDVDIV,0(R2)       SET DIVISION CODE                            
         MVI   BIDVDIV+L'BIDVDIV,FLDSEPQ                                        
*                                                                               
         MVC   BIDVNAME,3(R2)      COPY DIVISION NAME                           
         MVI   BIDVNAME+L'BIDVNAME,FLDSEPQ                                      
*                                                                               
         CLC   BIDVDIV,WBIDIVSV    SKIP IF DIVISION UNCHANGED                   
         BE    FBIDIVPX                                                         
*                                                                               
         MVC   WBIDIVSV,BIDVDIV    SAVE DIVISION NUMBER                         
         MVI   PRTDIV,C'Y'         INDICATE DIVISION RECORD TO PRINT            
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIPRDSV,WBIPRDSV   PRODUCT                                      
         XC    WBIESTSV,WBIESTSV   ESTIMATE                                     
*                                                                               
FBIDIVPX DS    0H                                                               
*                                                                               
FBIDIVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PRODUCT FIRST-FBIPRD'               
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR PRODUCT                                       *         
*          READ PRODUCT                                               *         
*          PUT OUT PRODUCT RECORD                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIPRD   NMOD1 0,**#FPR                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        READ PRODUCT RECORD                                                    
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH AS PRODUCT KEY                     
         USING PPRDRECD,R4                                                      
*                                                                               
         MVC   PPRDKAGY,PBAGY      SET AGENCY ID                                
         MVC   PPRDKMED,PBMED      SET MEDIA                                    
         MVI   PPRDKRCD,PPRDKIDQ   SET PRODUCT RECORD ID                        
         MVC   PPRDKCLT,BICLCLT    SET CLIENT  CODE                             
         MVC   PPRDKPRD,0(R2)      SET PRODUCT ID                               
*                                                                               
         GOTO1 HIGH                READ PRODUCT RECORD POINTER                  
*                                                                               
         CLC   PPRDKEY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ PRODUCT RECORD                          
*                                                                               
         L     R4,AIO              SWITCH POINTERS                              
*                                                                               
*        BUILD PRODUCT OUTPUT RECORD                                            
*                                                                               
         XC    BIPRRECA(BIPRRECL),BIPRRECA   INIT PRODUCT RECORD                
         MVC   BIPRLREC,=Y(BIPRRECL)   SET PRODUCT RECORD LENGTH                
*                                                                               
         MVC   BIPRRID,=AL2(BIPRIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIPRRID+L'BIPRRID,FLDSEPQ                                        
*                                                                               
         MVC   BIPRPRD,PPRDKPRD    SET PRODUCT CODE                             
         MVI   BIPRPRD+L'BIPRPRD,FLDSEPQ                                        
*                                                                               
*        FIGURE OUT PRODUCT NUMBER - SEE PPRDRECD FOR DESCRIPTION               
*                                                                               
         CLI   PPRDACCT,X'FF'      CHECK FOR BINARY NUMBER IN LAST 3            
         BNE   FPRNMBR5                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PPRDACCT+1     GET PRODUCT NUMBER                           
*                                                                               
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   WORK(L'PNMPTRN),PNMPTRN SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'PNMPTRN),DUB+4  EDIT AND MARK NUMBER                      
*                                                                               
         LA    RF,WORK+L'PNMPTRN  CALCULATE AMOUNT TO MOVE                      
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIPRNMBR(0),0(R1)   MOVE TO RECORD                               
*                                                                               
         B     FPRNMBRX                                                         
*                                                                               
PNMPTRN  DC    X'4020202020202020'  EDIT PATTERN                                
*                                                                               
FPRNMBR5 DS    0H                                                               
*                                                                               
         MVC   BIPRNMBR(L'PPRDACCT),PPRDACCT   COPY NUMBER AS IS                
*                                                                               
FPRNMBRX DS    0H                                                               
*                                                                               
         MVI   BIPRNMBR+L'BIPRNMBR,FLDSEPQ                                      
*                                                                               
         MVC   BIPRNAME,PPRDNAME   COPY PRODUCT NAME                            
         MVI   BIPRNAME+L'BIPRNAME,FLDSEPQ                                      
*                                                                               
         CLC   BIPRPRD,WBIPRDSV    SKIP IF PRODUCT UNCHANGED                    
         BE    FBIPRDPX                                                         
*                                                                               
         MVC   WBIPRDSV,BIPRPRD    SAVE PRODUCT CODE                            
         MVI   PRTPRD,C'Y'         INDICATE PRODUCT RECORD TO PRINT             
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIESTSV,WBIESTSV   ESTIMATE                                     
*                                                                               
FBIPRDPX DS    0H                                                               
*                                                                               
FBIPRDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-ESTIMATE FIRST-FBIEST'              
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR ESTIMATE                                      *         
*          READ ESTIMATE                                              *         
*          PUT OUT ESTIMATE RECORD                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIEST   NMOD1 0,**#FES                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        READ ESTIMATE RECORD                                                   
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH AS ESTIMATE KEY                    
         USING PESTRECD,R4                                                      
*                                                                               
         MVC   PESTKAGY,PBAGY      SET AGENCY ID                                
         MVC   PESTKMED,PBMED      SET MEDIA                                    
         MVI   PESTKRCD,PESTKIDQ   SET ESTIMATE RECORD ID                       
         MVC   PESTKCLT,BICLCLT    SET CLIENT   ID                              
         MVC   PESTKPRD,BIPRPRD    SET PRODUCT  ID                              
         MVC   PESTKEST,0(R2)      SET ESTIMATE ID                              
*                                                                               
         GOTO1 HIGH                READ ESTIMATE RECORD POINTER                 
*                                                                               
         CLC   PESTKEY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ ESTIMATE RECORD                         
*                                                                               
         L     R4,AIO              SWITCH POINTERS                              
*                                                                               
*        BUILD ESTIMATE OUTPUT RECORD                                           
*                                                                               
         XC    BIESRECA(BIESRECL),BIESRECA   INIT ESTIMATE RECORD               
         MVC   BIESLREC,=Y(BIESRECL)   SET ESTIMATE RECORD LENGTH               
*                                                                               
         MVC   BIESRID,=AL2(BIESIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIESRID+L'BIESRID,FLDSEPQ                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PESTKEST       BINARY ESTIMATE NUMBER                       
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   WORK(L'ESTPTRN),ESTPTRN SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'ESTPTRN),DUB+6   EDIT AND MARK NUMBER                     
*                                                                               
         LA    RF,WORK+L'ESTPTRN   CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIESEST(0),0(R1)    MOVE TO RECORD                               
*                                                                               
         MVI   BIESEST+L'BIESEST,FLDSEPQ                                        
*                                                                               
         MVC   BIESNAME,PESTNAME   COPY ESTIMATE NAME                           
         MVI   BIESNAME+L'BIESNAME,FLDSEPQ                                      
*                                                                               
         MVC   BIESNAM2,PESTNAM2   COPY ESTIMATE NAME PART 2                    
         MVI   BIESNAM2+L'BIESNAM2,FLDSEPQ                                      
*                                                                               
         CLC   BIESEST,WBIESTSV    SKIP IF ESTIMATE UNCHANGED                   
         BE    FBIESTPX                                                         
*                                                                               
         MVC   WBIESTSV,BIESEST    SAVE ESTIMATE CODE                           
         MVI   PRTEST,C'Y'         INDICATE ESTIMATE RECORD TO PRINT            
*                                                                               
*                                  FORCE CHANGE IN NOTHING                      
*                                                                               
FBIESTPX DS    0H                                                               
*                                                                               
FBIESTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
ESTPTRN  DC    X'40202020'         ESTIMATE NUMBER EDIT PATTERN                 
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-REGION FIRST-FBIREG'                
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR REGION                                        *         
*                                                                     *         
*          PUT OUT REGION RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIREG   NMOD1 0,**#FRG                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         CLI   PRTINV,C'Y'         SKIP IF INVOICE HEADER TO PRINT              
         BE    FBIREGX               MEANS WE HAVE BILL HEADER DATA             
*                                                                               
*        BUILD REGION OUTPUT RECORD                                             
*                                                                               
         XC    BIRGRECA(BIRGRECL),BIRGRECA   INIT RECORD                        
         MVC   BIRGLREC,=Y(BIRGRECL)   SET RECORD LENGTH                        
*                                                                               
         MVC   BIRGRID,=AL2(BIRGIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIRGRID+L'BIRGRID,FLDSEPQ                                        
*                                                                               
         MVI   BIRGREG+L'BIRGREG,FLDSEPQ                                        
*                                                                               
         CLI   0(R2),X'FF'         IF NO REGION FOUND                           
         BE    *+8                                                              
         CLI   0(R2),0                                                          
         BNE   *+12                                                             
         MVI   BIRGREG,C'*'        SET INDICATOR                                
         B     FBIREG10                                                         
*                                                                               
         MVC   BIRGREG,0(R2)       SET REGION CODE                              
*                                                                               
         MVC   BIRGNAME,3(R2)      COPY REGION NAME                             
         MVI   BIRGNAME+L'BIRGNAME,FLDSEPQ                                      
*                                                                               
FBIREG10 DS    0H                                                               
*                                                                               
         CLC   BIRGREG,WBIREGSV    SKIP IF REGION UNCHANGED                     
         BE    FBIREGPX                                                         
*                                                                               
         MVC   WBIREGSV,BIRGREG    SAVE REGION   CODE                           
         MVI   PRTREG,C'Y'         INDICATE REGION RECORD TO PRINT              
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIDSTSV,WBIDSTSV   DISTRICT                                     
         XC    WBIPUBSV,WBIPUBSV   PUB                                          
         XC    WBIZNESV,WBIZNESV   ZONE                                         
         XC    WBIEDNSV,WBIEDNSV   EDITION                                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBIREGPX DS    0H                                                               
*                                                                               
FBIREGX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DISTRICT FIRST-FBIDST'              
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR DISTRICT                                      *         
*                                                                     *         
*          PUT OUT DISTRICT RECORD                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIDST   NMOD1 0,**#FDS                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         CLI   PRTINV,C'Y'         SKIP IF INVOICE HEADER TO PRINT              
         BE    FBIDSTX               MEANS WE HAVE BILL HEADER DATA             
*                                                                               
*        BUILD DISTRICT OUTPUT RECORD                                           
*                                                                               
         XC    BIDSRECA(BIDSRECL),BIDSRECA   INIT RECORD                        
         MVC   BIDSLREC,=Y(BIDSRECL)   SET RECORD LENGTH                        
*                                                                               
         MVC   BIDSRID,=AL2(BIDSIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIDSRID+L'BIDSRID,FLDSEPQ                                        
*                                                                               
         MVI   BIDSDST+L'BIDSDST,FLDSEPQ                                        
*                                                                               
         CLI   0(R2),X'FF'         IF NO DISTRICT FOUND                         
         BE    *+8                                                              
         CLI   0(R2),0                                                          
         BNE   *+12                                                             
         MVI   BIDSDST,C'*'           SET INDICATOR                             
         B     FBIDST10                                                         
*                                                                               
         MVC   BIDSDST,0(R2)       SET DISTRICT CODE                            
*                                                                               
         MVC   BIDSNAME,3(R2)      COPY DISTRICT NAME                           
         MVI   BIDSNAME+L'BIDSNAME,FLDSEPQ                                      
*                                                                               
FBIDST10 DS    0H                                                               
*                                                                               
         CLC   WBIDSTSV,BIDSDST    SKIP IF DISTRICT UNCHANGED                   
         BE    FBIDSTPX                                                         
*                                                                               
         MVC   WBIDSTSV,BIDSDST    SAVE DISTRICT CODE                           
         MVI   PRTDST,C'Y'         INDICATE DISTRICT RECORD TO PRINT            
*                                                                               
*                                  FORCE CHANGE IN                              
*                                                                               
         XC    WBIPUBSV,WBIPUBSV   PUB                                          
         XC    WBIZNESV,WBIZNESV   ZONE                                         
         XC    WBIEDNSV,WBIEDNSV   EDITION                                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBIDSTPX DS    0H                                                               
*                                                                               
FBIDSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PUB FIRST-FBIPUB'                   
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR PUB                                           *         
*          READ PUB                                                   *         
*          PUT OUT PUB RECORD                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIPUB   NMOD1 0,**#FPB                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         CLI   PRTINV,C'Y'         SKIP IF INVOICE HEADER TO PRINT              
         BE    FBIPUBX               MEANS WE HAVE BILL HEADER DATA             
*                                                                               
*        READ PUB RECORD                                                        
*                                                                               
         OC    28(L'PUBKPUB,R2),28(R2)    SKIP IF NO PUB GIVEN                  
         BZ    FBIPUBX                                                          
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH AS PUB KEY                         
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,PBMED       SET MEDIA                                    
         MVC   PUBKPUB,28(R2)      SET PUB  ID                                  
         MVC   PUBKZON,32(R2)      SET ZONE ID                                  
         MVC   PUBKED,33(R2)       SET ED   ID                                  
         MVC   PUBKAGY,PBAGY       SET AGENCY ID                                
         MVI   PUBKCOD,PUBKIDQ     SET PUB RECORD ID                            
*                                                                               
         MVC   FILENAME,=CL8'PUBDIR'                                            
*                                                                               
         GOTO1 HIGH                READ PUB RECORD POINTER                      
*                                                                               
         CLC   PUBKEY,KEYSAVE      MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILENAME,=CL8'PUBFIL'                                            
*                                                                               
         GOTO1 GETREC              READ PUB RECORD                              
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
*                                                                               
         L     R4,AIO              SWITCH POINTERS                              
*                                                                               
*        BUILD PUB OUTPUT RECORD                                                
*                                                                               
         XC    BIPBRECA(BIPBRECL),BIPBRECA   INIT PUB RECORD                    
         MVC   BIPBLREC,=Y(BIPBRECL)   SET PUB RECORD LENGTH                    
*                                                                               
         MVC   BIPBRID,=AL2(BIPBIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIPBRID+L'BIPBRID,FLDSEPQ                                        
*                                                                               
*        EDIT PUB NUMBER,ZONE,EDITION                                           
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 =V(PUBEDIT),DMCB,28(R2),(C'S',WORK)                              
*                                                                               
         LA    RF,WORK             INIT REGISTERS                               
         LR    RE,RF                                                            
         LA    R0,8                MAX DIGITS IN PUB NUMBER                     
*                                                                               
         CLI   0(RF),C','          LOOK FOR END OF PUB ID                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         SR    RF,RE               GET LENGTH OF PUB ID                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIPBPUB(0),0(RE)    SET PUB CODE                                 
*                                                                               
         MVI   BIPBPUB+L'BIPBPUB,FLDSEPQ                                        
*                                                                               
         LA    RF,2(RF,RE)         POINT TO ZONE                                
         LR    RE,RF               SAVE STARTING POINT                          
         LA    R0,2                MAX 2 POSITIONS FOR ZONE                     
*                                                                               
         CLI   0(RF),C','          LOOK FOR END OF ZONE ID                      
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         SR    RF,RE               GET LENGTH OF ZONE ID                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIPBZNE(0),0(RE)    SET ZONE CODE                                
*                                                                               
         MVI   BIPBZNE+L'BIPBZNE,FLDSEPQ                                        
*                                                                               
         LA    RF,2(RF,RE)         POINT TO EDITION                             
         MVC   BIPBEDN,0(RF)       SET EDITION                                  
*                                                                               
         MVI   BIPBEDN+L'BIPBEDN,FLDSEPQ                                        
*                                                                               
*        FIND NAME AND ADDRESS ELEMENT                                          
*                                                                               
         SR    RF,RF                                                            
         LA    R3,PUBKEY+33        POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
         CLI   0(R3),0             SKIP IF END OF RECORD FOUND                  
         BE    FPBNAMX                                                          
         CLI   0(R3),PUBNAMQ       LOOKING FOR NAME ELEMENT                     
         BE    *+16                                                             
         IC    RF,1(RF)            BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
         B     *-24                                                             
*                                                                               
         USING PUBNAMD,R3          ESTABLISH NAME ELEMENT                       
*                                                                               
         MVC   BIPBNAM1,PUBNAME    COPY PUB NAME                                
         MVI   BIPBNAM1+L'BIPBNAM1,FLDSEPQ                                      
*                                                                               
         MVC   BIPBNAM2,PUBZNAME   COPY ZONE NAME                               
         MVI   BIPBNAM2+L'BIPBNAM2,FLDSEPQ                                      
*                                                                               
FPBNAMX  DS    0H                                                               
*                                                                               
         CLC   WBIPUBSV,BIPBPUB    SKIP IF PUB, ZONE & EDN THE SAME             
         BNE   *+10                                                             
         CLC   WBIZNESV,BIPBZNE                                                 
         BNE   *+10                                                             
         CLC   WBIEDNSV,BIPBEDN                                                 
         BE    FBIPUBPX                                                         
*                                                                               
         MVC   WBIPUBSV,BIPBPUB    SAVE PUB ID                                  
         MVC   WBIZNESV,BIPBZNE    SAVE EDITON ID                               
         MVC   WBIEDNSV,BIPBEDN    SAVE ZONE ID                                 
*                                                                               
         MVI   PRTPUB,C'Y'         INDICATE PUB RECORD TO PRINT                 
*                                                                               
*                                  FORCE CHANGE IN NOTHING                      
         XC    WBIADCSV,WBIADCSV   AD CODE                                      
*                                                                               
FBIPUBPX DS    0H                                                               
*                                                                               
FPBPUTX  DS    0H                                                               
*                                                                               
FBIPUBX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-AD CODE FIRST-FBIADC'               
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR AD CODE                                       *         
*          READ JOB                                                   *         
*          PUT OUT AD CODE RECORD                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIADC   NMOD1 0,**#FAD                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         CLI   PRTINV,C'Y'         SKIP IF INVOICE HEADER TO PRINT              
         BE    FBIADCX               MEANS WE HAVE BILL HEADER DATA             
*                                                                               
         XC    BIACRECA(BIACRECL),BIACRECA   INIT AD CODE RECORD                
         MVC   BIACLREC,=Y(BIACRECL)   SET AD CODE RECORD LENGTH                
*                                                                               
         MVC   BIACRID,=AL2(BIACIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIACRID+L'BIACRID,FLDSEPQ                                        
*                                                                               
         MVI   BIACADC+L'BIACADC,FLDSEPQ                                        
*                                                                               
         OC    0(L'PJOBKJOB,R2),0(R2)  IF NO AD CODE FOUND                      
         BNZ   FBIADC05                                                         
*                                                                               
         MVI   BIACADC,C'*'           SET INDICATOR                             
         B     FBIADC10                                                         
*                                                                               
FBIADC05 DS    0H                                                               
*                                                                               
*        READ JOB RECORD                                                        
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH AS JOB KEY                         
         USING PJOBRECD,R4                                                      
*                                                                               
         MVC   PJOBKAGY,PBAGY      SET AGENCY ID                                
         MVC   PJOBKMED,PBMED      SET MEDIA                                    
         MVI   PJOBKRCD,PJOBKIDQ   SET JOB RECORD ID                            
         MVC   PJOBKCLT,BICLCLT    SET CLIENT  ID                               
         MVC   PJOBKPRD,BIPRPRD    SET PRODUCT ID                               
         MVC   PJOBKJOB,0(R2)      SET JOB     ID                               
*                                                                               
         GOTO1 HIGH                READ JOB RECORD POINTER                      
*                                                                               
         CLC   PJOBKEY,KEYSAVE     MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ JOB RECORD                              
*                                                                               
         L     R4,AIO              SWITCH POINTERS                              
*                                                                               
*        BUILD AD CODE OUTPUT RECORD                                            
*                                                                               
         MVC   BIACADC,PJOBKJOB        SET AD CODE                              
*                                                                               
         MVC   BIACCAP1,PJOBCAP1        SET CAPTION PART 1                      
         MVI   BIACCAP1+L'BIACCAP1,FLDSEPQ                                      
*                                                                               
         MVC   BIACCAP2,PJOBCAP2        SET CAPTION PART 2                      
         MVI   BIACCAP2+L'BIACCAP2,FLDSEPQ                                      
*                                                                               
FBIADC10 DS    0H                                                               
*                                                                               
         CLC   WBIADCSV,BIACADC    SKIP IF AD CODE UNCHANGED                    
         BE    FBIADCPX                                                         
*                                                                               
         MVC   WBIADCSV,BIACADC    SAVE AD CODE                                 
         MVI   PRTADC,C'Y'         INDICATE AD CODE RECORD TO PRINT             
*                                                                               
*                                  FORCE CHANGE IN NOTHING                      
*                                                                               
FBIADCPX DS    0H                                                               
*                                                                               
FBIADCX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INSERT FIRST-FBIIDT'                
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR INSERTION DATE                                *         
*          BUILD INVOICE DETAIL RECORD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FBIIDT   NMOD1 0,**#FID                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         OC    0(3,R2),0(R2)        SKIP IF NO INVOICE DATE                     
         BZ    FBIIDTX                                                          
*                                                                               
*        INIT INVOICE DETAIL RECORD                                             
*                                                                               
         XC    BIIDRECA(256),BIIDRECA   INIT INV DETAIL RECORD                  
         XC    BIIDRECA+256(BIIDRECL-256),BIIDRECA+256                          
         MVC   BIIDLREC,=Y(BIIDRECL)   SET INV DETAIL RECORD LENGTH             
*                                                                               
         MVC   BIIDRID,=AL2(BIIDIDQ)   SET RECORD IDENTIFIER                    
         MVI   BIIDRID+L'BIIDRID,FLDSEPQ                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(0,BIIDDTE)   DATE                         
         MVI   BIIDDTE+L'BIIDDTE,FLDSEPQ                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,3(R2)          LINE NUMBER IN BINARY                        
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         MVC   WORK(L'LNEPTRN),LNEPTRN SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'LNEPTRN),DUB+6   EDIT AND MARK NUMBER                     
*                                                                               
         LA    RF,WORK+L'LNEPTRN   CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIIDLNE(0),0(R1)    MOVE TO RECORD                               
*                                                                               
         MVI   BIIDLNE+L'BIIDLNE,FLDSEPQ                                        
*                                                                               
         MVI   PRTINS,C'Y'         INDICATE INSERTION RECORD TO PRINT           
*                                                                               
FBIIDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
LNEPTRN  DC    X'40202020'         LINE NUMBER EDIT PATTERN                     
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DUMMY COLUMN-IBIDUM'                
***********************************************************************         
*                                                                     *         
*        DUMMY COLUMN TO FORCE A FIRST TIME BREAK IN INSERTION DATE   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIDUM   NMOD1 0,**#IDM                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
IBIDUMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE #-OBIIB#'                   
***********************************************************************         
*                                                                     *         
*        INVOICE NUMBER AS IT APPEARS ON BILL                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIIB#   NMOD1 0,**#OIB#                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVC   PPBXINV,BIIHINV     PASS INVOICE NUMBER TO PPBILEXP              
*                                                                               
         GOTO1 =V(PPBILEXP),DMCB,PPBILEXD  GET EXPANDED INV #                   
*                                                                               
         MVC   WORK,PPBILEXD                                                    
*                                                                               
         LTR   R3,R3               SKIP IF NO OUTPUT AREA GIVEN                 
         BZ    *+10                                                             
         MVC   0(L'PPBXEXP,R3),PPBXEXP  PRINT EXPANDED BILL #                   
*                                                                               
         MVC   BIIHINV#,PPBXEXP    ADD CLIENT BILL NUMBER TO RECORD             
*                                                                               
         MVI   BIIHINV#+L'BIIHINV#,FLDSEPQ                                      
*                                                                               
OBIIB#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-GENERAL DATE-OBIDATE'               
***********************************************************************         
*                                                                     *         
*        GENERAL DATE ROUTINE                                         *         
*                                                                     *         
*        ALL DATES 3 BYTE BINARY ON INPUT                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIDATE  NMOD1 0,**#ODATE                                                       
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         OC    0(3,R2),0(R2)       SKIP IF NO DATE GIVEN                        
         BZ    OBIDATEX                                                         
*                                                                               
         SR    R4,R4               DETERMINE DATE TYPE                          
*                                                                               
         CLI   GLARGS,C'I'         BILL HEADER INVOICE DATE                     
         BNE   *+8                                                              
         LA    R4,BIIHIDTE                                                      
*                                                                               
         CLI   GLARGS,C'D'         BILL HEADER DUE     DATE                     
         BNE   *+8                                                              
         LA    R4,BIIHDDTE                                                      
*                                                                               
         CLI   GLARGS,C'R'         BILL HEADER RUN     DATE                     
         BNE   *+18                                                             
         MVC   BIIHRDTE,0(R2)      ALREADY IN CHARACTER                         
         MVI   BIIHRDTE+6,FLDSEPQ                                               
         B     OBIDATEX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(R4)  ADD DATE TO RECORD                   
*                                                                               
         MVI   6(R4),FLDSEPQ                                                    
*                                                                               
OBIDATEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILL TYPE-OBIBTYP'                  
***********************************************************************         
*                                                                     *         
*        BILL TYPE OUTPUT                                             *         
*                                                                     *         
*NTRY    R2==> B/M - B PROFILE OR MANUAL                              *         
*              #   - B PROFILE NUMBER                                 *         
*              -   - '-'                                              *         
*              XXX - 'REG' 'AOR' 'COM' 'ADJ' 'CD'                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIBTYP  NMOD1 0,**#OBTYP                                                       
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVC   BIIHBTYP,1(R2)      BILL TYPE                                    
         MVI   BIIHBTYP+L'BIIHBTYP,FLDSEPQ                                      
*                                                                               
         MVI   BIIHMAN,C'N'        INIT TO NO                                   
*                                                                               
         CLI   0(R2),C'M'          CHECK FOR MANUAL BILL                        
         BNE   *+8                                                              
         MVI   BIIHMAN,C'Y'        INIDICATE MANUAL BILL                        
*                                                                               
         MVI   BIIHMAN+L'BIIHMAN,FLDSEPQ                                        
*                                                                               
OBITYPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILL CHARACT-IBIBCHR'               
***********************************************************************         
*                                                                     *         
*        BILL CHARACTERISTICS FROM BILL HEADER                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIBCHR  NMOD1 0,**#IBCHR                                                       
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BILL RECORD                        
         USING PBILLRCD,R4                                                      
*                                                                               
         CLI   PBILKRCD,PBILKIDQ   MUST HAVE A BILL RECORD                      
         BNE   IBIBCHRX                                                         
*                                                                               
         MVC   0(1,R2),PBILSEP     RETURN INDICATORS FOR INVOICE                
         MVC   1(1,R2),PBILCMSW                                                 
*                                                                               
IBIBCHRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-CHARACTER-OBIBCHR'                  
***********************************************************************         
*                                                                     *         
*        BILL CHARACTERISTICS FROM BILL HEADER                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIBCHR  NMOD1 0,**#OBCHR                                                       
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVI   BIIHREV,C'N'        INIT TO NO                                   
*                                                                               
         MVI   BIIHREV+L'BIIHREV,FLDSEPQ                                        
*                                                                               
         MVI   BIIHCLRD,C'N'       INIT TO NO                                   
*                                                                               
         MVI   BIIHCLRD+L'BIIHCLRD,FLDSEPQ                                      
*                                                                               
         MVI   BIIHCOMO,C'N'       INIT TO NO                                   
*                                                                               
         TM    1(R2),X'02'         COMMISSION ONLY BILL                         
         BNO   *+8                                                              
         MVI   BIIHCOMO,C'Y'                                                    
*                                                                               
         MVI   BIIHCOMO+L'BIIHCOMO,FLDSEPQ                                      
*                                                                               
         MVI   BIIHCOMU,C'N'       INIT TO NO                                   
*                                                                               
         TM    1(R2),X'01'         UPFRONT COMMISSION BILL                      
         BNO   *+8                                                              
         MVI   BIIHCOMU,C'Y'                                                    
*                                                                               
         MVI   BIIHCOMU+L'BIIHCOMU,FLDSEPQ                                      
*                                                                               
         MVI   BIIHADJS,C'N'       INIT TO NO                                   
*                                                                               
         TM    1(R2),X'80'         SEPARATE ADJUSTMENT BILL                     
         BNO   *+8                                                              
         MVI   BIIHADJS,C'Y'                                                    
*                                                                               
         MVI   BIIHADJS+L'BIIHADJS,FLDSEPQ                                      
*                                                                               
         MVI   BIIHCDS,C'N'        INIT TO NO                                   
*                                                                               
         CLI   0(R2),C'C'          SEPARATE CD BILL                             
         BNE   *+8                                                              
         MVI   BIIHCDS,C'Y'                                                     
*                                                                               
         MVI   BIIHCDS+L'BIIHCDS,FLDSEPQ                                        
*                                                                               
         MVI   BIIHAOR,C'N'        INIT TO NO                                   
*                                                                               
         TM    1(R2),X'20'         AOR BILL                                     
         BNO   *+8                                                              
         MVI   BIIHAOR,C'Y'                                                     
*                                                                               
         MVI   BIIHAOR+L'BIIHAOR,FLDSEPQ                                        
*                                                                               
OBIBCHRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-SHARES INPUT-IBISHR'                
***********************************************************************         
*                                                                     *         
*        PRODUCT'S SHARES INPUT                                       *         
*                                                                     *         
*EXIT    R2==> PL3 - PERCENTAGE AS 999.99                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBISHR   NMOD1 0,**#ISH                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         ZAP   0(3,R2),=P'0'       INIT DATA                                    
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   MUST HAVE A BUY RECORD                       
         BNE   IBISHRX                                                          
*                                                                               
         LA    R5,PBUYKEY+33       POINT TO FIRST ELEMENT IN RECORD             
         SR    RF,RF                                                            
*                                                                               
         USING PPRELD,R5           ESTABLISH AS PRODUCT ELEMENT                 
*                                                                               
IBISHRLP DS    0H                                                               
*                                                                               
         CLI   PPRELEM,0           SKIP IF END OF RECORD FOUND                  
         BE    IBISHRX                                                          
*                                                                               
         CLI   PPRELEM,PPRELQ      FIND PRODUCT ELEMENT                         
         BNE   IBISHRCN                                                         
*                                                                               
         CLC   PBPROD,PPRCODE      FIND CURRENT PRODUCT'S ELEMENT               
         BE    IBISHRFD                                                         
*                                                                               
IBISHRCN DS    0H                                                               
*                                                                               
         IC    RF,PPRELEM+1        BUMP TO NEXT ELEMENT                         
         LA    R5,PPRELEM(RF)                                                   
         B     IBISHRLP                                                         
*                                                                               
IBISHRFD DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         SR    R1,R1                                                            
*                                                                               
         CLI   GLARGS+1,C'S'       IF SPACE SHARE WANTED                        
         BNE   *+8                                                              
         IC    RF,PPRSPCE             GET SPACE UNITS                           
*                                                                               
         CLI   GLARGS+1,C'C'       IF COST SHARE WANTED                         
         BNE   *+8                                                              
         IC    RF,PPRCOST             GET COST UNITS                            
*                                                                               
         M     RE,=F'1000'         SCALE UP FOR DECIMALS                        
*                                                                               
         MVC   DUB(1),PBDWTSUM     COPY TOTAL UNITS                             
         NI    DUB,X'FF'-X'80'     KILL HIGH ORDER BIT                          
         IC    R1,DUB              TOTAL UNITS                                  
*                                                                               
         DR    RE,R1               GET PERCENTAGE                               
         CVD   R1,DUB              CVD                                          
         SRP   DUB,64-1,5          ROUND RESULT                                 
*                                                                               
         ZAP   0(3,R2),DUB+5       RETURN SHARE AS 999.99                       
*                                                                               
IBISHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-SHARES OUTPUT-OBISHR'               
***********************************************************************         
*                                                                     *         
*        PRODUCT'S SHARES OUTPUT                                      *         
*                                                                     *         
*NTRY    R2==> PL3 - PERCENTAGE TO 999.99                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBISHR   NMOD1 0,**#OSH                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         LA    R1,WORK+1           PRESET MARKER                                
*                                                                               
         MVC   WORK(7),=X'402020206B2020' SET EDIT PATTERN                      
         EDMK  WORK(7),0(R2)       EDIT SHARE                                   
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF RESULT                   
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         LA    RF,7                EDIT PATTERN LENGTH                          
         SR    RF,RE               LENGTH OF RESULT                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    *+8                                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       PRINT RESULT                                 
*                                                                               
         CLI   GLARGS,C'S'         IF SPACE SHARE                               
         BNE   OBISHRSN                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIIDSHRS(0),0(R1)      ADD TO RECORD                             
*                                                                               
         MVI   BIIDSHRS+L'BIIDSHRS,FLDSEPQ                                      
*                                                                               
         B     OBISHRCX                                                         
*                                                                               
OBISHRSN DS    0H                                                               
*                                                                               
         CLI   GLARGS,C'C'         IF COST SHARE                                
         BNE   OBISHRCX                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIIDSHRC(0),0(R1)      ADD TO RECORD                             
*                                                                               
         MVI   BIIDSHRC+L'BIIDSHRC,FLDSEPQ                                      
*                                                                               
OBISHRCX DS    0H                                                               
*                                                                               
OBISHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-SPACE OUTPUT-IBISPC'                
***********************************************************************         
*                                                                     *         
*        SPACE DESCRIPTION OUTPUT                                     *         
*          BUILD INVOICE DETAIL RECORD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBISPC   NMOD1 0,**#OSP                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R1,GLADTENT         ESTABLISH DRIVER OUTPUT ENTRY                
         USING DROD,R1                                                          
*                                                                               
         CLI   0(R2),X'FF'         CLEAR OUT NO SPACE INDICATOR                 
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DROLEN         COLUMN WIDTH                                 
         BNZ   *+8                                                              
         LA    RF,17               DEFAULT TO 34                                
*                                                                               
         SLL   RF,1                DOUBLE FOR 2 LINES OF DESCRIPTION            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIIDSPC(0),0(R2)    ENTER SPACE DESCRIPTION                      
*                                                                               
         MVI   BIIDSPC+L'BIIDSPC,FLDSEPQ                                        
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    OBISPCPX                                                         
*                                                                               
         LA    RF,1(RF)                                                         
         SRL   RF,1                LENGTH FOR 1 LINE                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       PRINT RESULT                                 
*                                                                               
         LA    R2,1(RF,R2)         SECOND HALF                                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   198(0,R3),0(R2)       PRINT RESULT                               
*                                                                               
OBISPCPX DS    0H                                                               
*                                                                               
OBISPCX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-UNIT RATE INPUT-IBIURT'             
***********************************************************************         
*                                                                     *         
*        UNIT RATE - INPUT                                            *         
*                                                                     *         
*EXIT    R2 ==>  PL5 RATE TO 5 DECIMALS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIURT   NMOD1 0,**#IURT                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         LH    RF,=Y(PPBYOUTC-SYSD)  DISPLACEMENT INTO WORKAREA                 
         AR    RF,R9               GET ADDRESS                                  
         MVC   0(L'PBYOUR,R2),PBYOUR-PPBYOUTD(RF) UNIT RATE                     
*                                                                               
IBIURTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-UNIT RATE OUTPUT-OBIURT'            
***********************************************************************         
*                                                                     *         
*        UNIT RATE - OUTPUT                                           *         
*                                                                     *         
*EXIT    R2 ==>  PL5 RATE TO 5 DECIMALS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIURT   NMOD1 0,**#OURT                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVC   BIIDURTE,0(R2)      MOVE RATE TO OUTPUT                          
*                                                                               
         MVI   BIIDURTE+L'BIIDURTE,FLDSEPQ                                      
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE DRIVER PRINT IT                         
*                                                                               
OBIURTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-MOS INPUT-IBIMOS'                   
***********************************************************************         
*                                                                     *         
*        MONTH OF SERVICE INPUT                                       *         
*                                                                     *         
*EXIT    R2 ==>  YMD BINARY BILLABLE DATE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIMOS   NMOD1 0,**#IMS                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIMOSBX                                                         
*                                                                               
         LA    R5,PBUYKEY+33       POINT TO FIRST ELEMENT IN RECORD             
         SR    RF,RF                                                            
*                                                                               
         USING PBDELEM,R5          ESTABLISH AS BUY DESCRITPION ELEMENT         
*                                                                               
         CLI   PBDELEM,PBDELQ      FIND DESCRIPTION ELEMENT                     
         BE    *+24                                                             
         CLI   PBDELEM,0           SKIP IF END OF RECORD FOUND                  
         BE    IBIMOSX                                                          
         IC    RF,PBDELEM+1        BUMP TO NEXT ELEMENT                         
         LA    R5,PBDELEM(RF)                                                   
         B     *-24                                                             
*                                                                               
         MVC   0(2,R2),PBDBDATE    RETURN YM OF BILLABLE DATE                   
*                                                                               
         B     IBIMOSX                                                          
*                                                                               
IBIMOSBX DS    0H                                                               
*                                                                               
IBIMOSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-MOS OUTPUT-OBIMOS'                  
***********************************************************************         
*                                                                     *         
*        MONTH OF SERVICE OUTPUT                                      *         
*                                                                     *         
*NTRY    R2 ==> YMD BINARY                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIMOS   NMOD1 0,**#OMS                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R2)),WORK  DATE AS YYMMDD CH                     
*                                                                               
         MVC   BIIDMOS,WORK        PUT IN DETAIL RECORD                         
*                                                                               
         MVI   BIIDMOS+L'BIIDMOS,FLDSEPQ                                        
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    OBIMOSX                                                          
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    *+8                                                              
         GOTO1 DATCON,DMCB,(3,(R2)),(6,0(R3)) MMM/YY                            
*                                                                               
OBIMOSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-COMMENT INPUT-IBICOM'               
***********************************************************************         
*                                                                     *         
*        COMMENT INPUT                                                *         
*                                                                     *         
*        MAXIMUM 5 COMMENTS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBICOM   NMOD1 0,**#ICM                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVI   0(R2),0             INIT OUTPUT                                  
         MVI   1(R2),C' '          FORCE SOME INPUT                             
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   MUST HAVE A BUY RECORD                       
         BNE   IBICOMX                                                          
*                                                                               
         LA    R5,PBUYKEY+33       POINT TO FIRST ELEMENT IN RECORD             
         SR    RF,RF                                                            
         LA    R0,5                MAX NUMBER OF COMMENTS                       
*                                                                               
IBICOMLP DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF END OF RECORD FOUND                  
         BE    IBICOMX                                                          
*                                                                               
         CLI   0(R5),PCOMELQ       FIND COMMENT ELEMENT                         
         BNE   IBICOMCN                                                         
*                                                                               
         IC    RF,1(R5)            GET ELEMENT LENGTH                           
         SH    RF,=H'2'            COMMENT LENGTH                               
         STC   RF,0(R2)            PASS COMMENT LENGTH                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),2(R5)       ADD COMMENT TO INPUT                         
*                                                                               
         LA    R2,2(RF,R2)         BUMP TO NEXT AVAILABLE AREA                  
         MVI   0(R2),0             INIT LENGTH OF NEXT COMMENT                  
*                                                                               
         BCT   R0,*+8              STOP AFTER 5 COMMENTS                        
         B     IBICOMX                                                          
*                                                                               
IBICOMCN DS    0H                                                               
*                                                                               
         IC    RF,1(R5)            BUMP TO NEXT ELEMENT                         
         LA    R5,0(RF,R5)                                                      
         B     IBICOMLP                                                         
*                                                                               
IBICOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-COMMENT OUTPUT-OBICOM'              
***********************************************************************         
*                                                                     *         
*        COMMENT OUTPUT                                               *         
*                                                                     *         
*NTRY    R2==> STRING OF COMMENTS  AL1(LENGTH),C'COMMENT'             *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBICOM   NMOD1 0,**#OCM                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         LA    R1,BIIDCOM1         POINT TO START OF COMMENTS                   
         SR    RF,RF                                                            
         LA    R0,5                MAX 5 COMMENTS                               
*                                                                               
OBICOMLP DS    0H                                                               
*                                                                               
         ICM   RF,1,0(R2)          GET COMMENT LENGTH                           
         BZ    OBICOMDN            END OF COMMENTS                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),1(R2)       MOVE COMMENT TO RECORD                       
*                                                                               
         MVI   L'BIIDCOM1(R1),FLDSEPQ                                           
*                                                                               
OBICOMCN DS    0H                                                               
*                                                                               
         LA    R1,L'BIIDCOM1+1(R1)    BUMP TO NEXT COMMENT FIELD                
         LA    R2,2(RF,R2)         BUMP TO NEXT COMMENT                         
         BCT   R0,OBICOMLP                                                      
*                                                                               
         B     OBICOMD1            MAX 5 COMMENTS REACHED                       
*                                                                               
OBICOMDN DS    0H                  FILL IN REMAINING COMMENT DELIMITERS         
*                                                                               
         MVI   L'BIIDCOM1(R1),FLDSEPQ                                           
         LA    R1,L'BIIDCOM1+1(R1)    BUMP TO NEXT COMMENT FIELD                
         BCT   R0,*-8                                                           
*                                                                               
OBICOMD1 DS    0H                                                               
*                                                                               
OBICOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILL FORM INPUT-IBIBFM'             
***********************************************************************         
*                                                                     *         
*        BILL FORMULA INPUT                                           *         
*          FIND BILL FORMULA                                          *         
*                                                                     *         
*EXIT    R2==> XL1 - BILL BASIS                                       *         
*              XL1 - COMMISSION BASIS                                 *         
*              XL3 - COMISSION ADJUSMENT PERCENT                      *         
*              XL1 - X'FF'                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIBFM   NMOD1 0,**#IBF                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BILL RECORD                        
         USING PBUYRECD,R4                                                      
*                                                                               
         MVI   5(R2),X'FF'         FORCE RETURN TO OBIBFM                       
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIBFMX                                                          
*                                                                               
*        FIND BILL HEADER RECORD IN TABLE                                       
*                                                                               
         ICM   R5,15,PBBLLELA      ESTABLISH CURRENT BILL ELEMENT               
         BZ    IBIBFMX             NONE AVAILABLE                               
         USING PBILELD,R5                                                       
*                                                                               
         L     R3,WBIINVTA         POINT TO START OF INVOICE TABLE              
         USING PBILLRCD,R3         ESTABLISH AS BILL RECORD                     
*                                                                               
IBIBFMLP DS    0H                                                               
*                                                                               
         OC    0(INVTENTL,R3),0(R3) DONE IF END OF TABLE REACHED                
         BZ    IBIBFMX                                                          
*                                                                               
         CLC   PBILKPRD,PBPRD      MUST MATCH PRODUCT                           
         BNE   IBIBFMCN                                                         
         CLC   PBILKEST,PBUYKEST   MUST MATCH ESTIMATE                          
         BNE   IBIBFMCN                                                         
         CLC   PBILKMOS,PBDBDATE   MUST MATCH MONTH OF SERVICE                  
         BNE   IBIBFMCN                                                         
         CLC   PBILKBMN,PBLDATE    MUST MATCH BILL MONTH                        
         BNE   IBIBFMCN                                                         
         CLC   PBILKBNO,PBINVNO    MUST MATCH INVOICE NUMBER                    
         BE    IBIBFMFD                                                         
*                                                                               
IBIBFMCN DS    0H                                                               
*                                                                               
*        MVC   WORK,0(R3)                                                       
*                                                                               
         LA    R3,INVTENTL(R3)     BUMP TO NEXT INVOICE IN TABLE                
         B     IBIBFMLP                                                         
*                                                                               
IBIBFMFD DS    0H                                                               
*                                                                               
         MVC   0(1,R2),PBILBASA    BILL BASIS                                   
*                                                                               
         OC    PBILADJ,PBILADJ     SKIP IF NO COMMISSION ADJUSTMENT             
         BZ    *+16                                                             
         MVC   1(1,R2),PBILBASB    COMMISSION BASIS                             
         MVC   2(3,R2),PBILADJ     COMMISSION ADJUSTMENT                        
*                                                                               
IBIBFMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILL FORMULA-OBIBFM'                
***********************************************************************         
*                                                                     *         
*        BILL FORMULA                                                 *         
*                                                                     *         
*NTRY    R2==> XL1 - BILL BASIS                                       *         
*              XL1 - COMMISSION BASIS                                 *         
*              XL3 - COMISSION ADJUSMENT PERCENT                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIBFM   NMOD1 0,**#OBFM                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         USING BILPROFD,R2         ESTABLISH BILLING PROFILE                    
*                                                                               
         LA    R0,2                SET LOOP COUNTER                             
         LA    R1,BIIDBBSE         POINT TO FIRST BASIS FIELD                   
         LA    RF,BILBASA          POINT TO FIRST BASIS INPUT                   
*                                                                               
OBIBFMLP DS    0H                                                               
*                                                                               
         TM    0(RF),X'01'         TEST FOR GROSS BASIS                         
         BNO   *+8                                                              
         MVI   0(R1),C'G'                                                       
*                                                                               
         TM    0(RF),X'02'         TEST FOR NET   BASIS                         
         BNO   *+8                                                              
         MVI   0(R1),C'N'                                                       
*                                                                               
         TM    0(RF),X'04'         TEST FOR LESS CD                             
         BNO   *+10                                                             
         MVC   1(3,R1),=C'-CD'                                                  
*                                                                               
         TM    0(RF),X'08'         TEST FOR AGENCY COMMISSION ONLY              
         BNO   *+10                                                             
         MVC   0(4,R1),=CL4'AC'                                                 
*                                                                               
         MVI   L'BIIDBBSE(R1),FLDSEPQ                                           
*                                                                               
OBIBFMCN DS    0H                                                               
*                                                                               
         LA    R1,BIIDCBSE         BUMP TO COMMISSION BASIS IN RECORD           
         LA    RF,BILBASB          BUMP TO COMMISSION BASIS IN PROFILE          
         BCT   R0,OBIBFMLP                                                      
*                                                                               
OBIBFMDN DS    0H                                                               
*                                                                               
*        PUT OUT COMMISSION PERCENT                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,BILADJ         GET COMMISSION ADJUSTMENT                    
*                                                                               
         TM    BILADJ,X'80'        IF NEGATIVE AMOUNT                           
         BNO   *+12                                                             
         MVI   WORK,X'FF'                                                       
         ICM   RF,8,WORK              FILL IN HIGH ORDER BITS                   
*                                                                               
         CVD   RF,DUB              CVD COMMISSION PERCENT                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'OBFEPTRN),OBFEPTRN   SET EDIT PATTERN                     
*                                                                               
         LA    R1,WORK+L'OBFEPTRN  SET DEFAULT MARK POSITION                    
*                                                                               
         EDMK  WORK(L'OBFEPTRN),DUB+3   EDIT PER CENT                           
*                                                                               
         CP    DUB,=P'0'           IF NEGATIVE                                  
         BNM   *+10                                                             
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'          FLOAT IN MINUS SIGN                          
*                                                                               
         LA    RF,WORK+L'OBFEPTRN  CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               SIGNIFICANT # OF CHARACTERS                  
         BZ    OBFADJX             NOTHING TO MOVE                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIIDCAPC(0),0(R1)   ADD TO RECORD                                
*                                                                               
OBFADJX  DS    0H                                                               
*                                                                               
         MVI   BIIDCAPC+L'BIIDCAPC,FLDSEPQ                                      
*                                                                               
OBIBFMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
OBFEPTRN DC    X'4020202020204B20202020'                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DOLLARS INPUT-IBIDLR'               
***********************************************************************         
*                                                                     *         
*        DOLLARS INPUT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIDLR   NMOD1 0,**#I$$                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIDLRX                                                          
*                                                                               
         ICM   R5,15,PBBLLELA      ESTABLISH CURRENT BILL ELEMENT               
         BZ    IBIDLRX             NONE AVAILABLE                               
         USING PBILELD,R5                                                       
*                                                                               
         ICM   R3,15,PBGROSS       GET GROSS AMOUNT                             
         ICM   RE,15,PBAGYCOM      GET AGENCY COMMISSION                        
         ICM   RF,15,PBCSHDSC      GET CASH DISCOUNT                            
*                                                                               
*        FIND DATA TYPE                                                         
*                                                                               
         SR    R1,R1                                                            
*                                                                               
         CLI   GLARGS+2,C'1'       GROSS                                        
         BNE   IBIDLGRX                                                         
*                                                                               
         LR    R1,R3               COLLECT GROSS                                
         B     IBIDLRTX                                                         
*                                                                               
IBIDLGRX DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'2'       GROSS LESS CD                                
         BNE   IBIDLGCX                                                         
*                                                                               
         LR    R1,R3               COLLECT GROSS                                
         SR    R1,RF               LESS CD                                      
         B     IBIDLRTX                                                         
*                                                                               
IBIDLGCX DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'3'       NET LESS CD                                  
         BNE   IBIDLNCX                                                         
*                                                                               
         LR    R1,R3               COLLECT GROSS                                
         SR    R1,RE               LESS AGENCY COMMISSION                       
         SR    R1,RF               LESS CD                                      
         B     IBIDLRTX                                                         
*                                                                               
IBIDLNCX DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'4'       NET                                          
         BNE   IBIDLNTX                                                         
*                                                                               
         LR    R1,R3               COLLECT GROSS                                
         SR    R1,RE               LESS AGENCY COMMISSION                       
         B     IBIDLRTX                                                         
*                                                                               
IBIDLNTX DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'5'       CD                                           
         BNE   IBIDLCDX                                                         
*                                                                               
         LR    R1,RF               COLLECT CD                                   
         B     IBIDLRTX                                                         
*                                                                               
IBIDLCDX DS    0H                                                               
*                                                                               
         CLI   GLARGS+2,C'6'       AC                                           
         BNE   IBIDLACX                                                         
*                                                                               
         LR    R1,RE               COLLECT AGENCY COMMISSION                    
         B     IBIDLRTX                                                         
*                                                                               
IBIDLACX DS    0H                                                               
*                                                                               
IBIDLRTX DS    0H                                                               
*                                                                               
         CVD   R1,DUB              DESIRED AMOUNT                               
*                                                                               
         ZAP   0(8,R2),DUB         RETURN AMOUNT                                
*                                                                               
IBIDLRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILL ACTUAL-IBIBLL'                 
***********************************************************************         
*                                                                     *         
*        ACTUAL BILLED INPUT                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIBLL   NMOD1 0,**#IBL                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIBLLX                                                          
*                                                                               
         ZAP   WBIGST,=P'0'        INIT GST FOR BILL ELEMENT                    
*                                                                               
*        FIND BILL HEADER RECORD IN TABLE                                       
*                                                                               
         ICM   R5,15,PBBLLELA      ESTABLISH CURRENT BILL ELEMENT               
         BZ    IBIBLLX             NONE AVAILABLE                               
         USING PBILELD,R5                                                       
*                                                                               
         L     R3,WBIINVTA         POINT TO START OF INVOICE TABLE              
         USING PBILLRCD,R3         ESTABLISH AS BILL RECORD                     
*                                                                               
IBIBLLLP DS    0H                                                               
*                                                                               
         OC    0(INVTENTL,R3),0(R3) DONE IF END OF TABLE REACHED                
         BZ    IBIBLLX                                                          
*                                                                               
         CLC   PBILKPRD,PBPRD      MUST MATCH PRODUCT                           
         BNE   IBIBLLCN                                                         
         CLC   PBILKEST,PBUYKEST   MUST MATCH ESTIMATE                          
         BNE   IBIBLLCN                                                         
         CLC   PBILKMOS,PBDBDATE   MUST MATCH MONTH OF SERVICE                  
         BNE   IBIBLLCN                                                         
         CLC   PBILKBMN,PBLDATE    MUST MATCH BILL MONTH                        
         BNE   IBIBLLCN                                                         
         CLC   PBILKBNO,PBINVNO    MUST MATCH INVOICE NUMBER                    
         BE    IBIBLLFD                                                         
*                                                                               
IBIBLLCN DS    0H                                                               
*                                                                               
         MVC   WORK,0(R3)                                                       
*                                                                               
         LA    R3,INVTENTL(R3)     BUMP TO NEXT INVOICE IN TABLE                
         B     IBIBLLLP                                                         
*                                                                               
IBIBLLFD DS    0H                                                               
*                                                                               
*        CALCULATE BILLED COST VIA GETCOST                                      
*                                                                               
         MVC   IBIBLGRS,PBGROSS    RE-ARRANGE FIELD ORDER                       
         MVC   IBIBLCD,PBCSHDSC                                                 
         MVC   IBIBLAC,PBAGYCOM                                                 
*                                                                               
         GOTO1 =V(GETCOST),DMCB,PBILBASA,IBIBLGRS,AIO1                          
*                                                                               
         ICM   RF,15,DMCB+4        GET RETURNED COST                            
         CVD   RF,DUB              RETURN AMOUNT                                
         ZAP   0(8,R2),DUB                                                      
*                                                                               
*        FIND VAT ELEMENT                                                       
*                                                                               
         LA    RE,PBILLEL          FIRST ELEMENT IN RECORD                      
         SR    RF,RF                                                            
*                                                                               
         CLI   0(RE),X'00'         END OF RECORD                                
         BE    IBIBLL50                                                         
         CLI   0(RE),X'0A'         LOOK FOR VAT ELEMENT                         
         BE    *+16                                                             
         IC    RF,1(RE)            ELEMENT LENGTH                               
         LA    RE,0(RF,RE)         POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         OC    PBILLVAT-PBILVEL(L'PBILLVAT,RE),PBILLVAT-PBILVEL(RE)             
         BZ    IBIBLL50                                                         
*                                                                               
         MP    DUB,=P'7'           GST = 7% OF BILLED AMOUNT                    
         SRP   DUB,64-2,5          ROUND TO 2 DECIMALS                          
         ZAP   WBIGST,DUB          SAVE GST AMOUNT                              
         AP    0(8,R2),DUB         ADD TO ACTUAL BILL AMOUNT                    
*                                                                               
IBIBLL50 DS    0H                                                               
*                                                                               
*                                                                               
IBIBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
IBIBLGRS DS    F                   WORK GROSS                                   
IBIBLCD  DS    F                   WORK CASH DISCOUNT                           
IBIBLAC  DS    F                   WORK AGENCY COMMISSION                       
*                                                                               
         LTORG                                                                  
         DROP  R3,R4,R5                                                         
         TITLE 'PRWRI14-BILL INTERFACE TAPE-BILLED GSR-IBIGST'                  
***********************************************************************         
*                                                                     *         
*        BILLED GST                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIGST   NMOD1 0,**#IGS                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         L     R3,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R3                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIGSTX                                                          
*                                                                               
         ICM   R5,15,PBBLLELA      ESTABLISH CURRENT BILL ELEMENT               
         BZ    IBIGSTX             NONE AVAILABLE                               
         USING PBILELD,R5                                                       
*                                                                               
         ZAP   0(8,R2),WBIGST      RETURN BILLED GST                            
*                                                                               
IBIGSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R5                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DOLLARS OUTPUT-OBIDLR'              
***********************************************************************         
*                                                                     *         
*        DOLLARS OUTPUT                                               *         
*          BUILD INVOICE DETAIL RECORD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIDLR   NMOD1 0,**#O$$                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        FIND ADDRESS OF DATA IN RECORD                                         
*                                                                               
         CLI   GLARGS+1,C'L'       IF BILL HEADER SOURCE                        
         BNE   OBIDLR1                                                          
         OC    0(8,R2),0(R2)       AND IF THERE IS DATA                         
         BNZ   *+14                                                             
         ZAP   DUB,=P'0'                                                        
         B     OBIDLR1                                                          
*                                                                               
         ZAP   DUB,0(8,R2)         MAKE 8 BYTES PACKED                          
         LA    R2,DUB              SWITCH INPUT POINTER                         
*                                                                               
OBIDLR1  DS    0H                                                               
*                                                                               
         OC    0(8,R2),0(R2)       SKIP IF NO DATA                              
         BZ    OBIDLRX                                                          
*                                                                               
         LA    RF,OBID$TB          POINT TO TABLE                               
*                                                                               
         TM    GLINDS,GLTOTLIN     IF TOTAL LINE USE TOTAL TABLE                
         BNO   *+8                                                              
         LA    RF,OBID$TT          POINT TO TOTAL TABLE                         
*                                                                               
         SR    RE,RE                                                            
*                                                                               
         CLC   0(2,RF),GLARGS      MATCH AMOUNT TYPE                            
         BE    *+12                                                             
         LA    RF,OBID$TBL(RF)     BUMP TO NEXT ENTRY                           
         B     *-14                                                             
*                                                                               
         ICM   RE,7,2(RF)          A(FIELD IN RECORD)                           
         LA    RE,WBIWORKD(RE)                                                  
*                                                                               
         MVC   WORK(L'EDTPTRN),EDTPTRN SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'EDTPTRN),2(R2) EDIT AND MARK DOLLARS                      
*                                                                               
         CP    0(8,R2),=P'0'       IF NEGATIVE RESULT                           
         BNM   *+10                                                             
         BCTR  R1,0                   SET FLOATING MINUS SIGN                   
         MVI   0(R1),C'-'                                                       
*                                                                               
         LA    RF,WORK+L'EDTPTRN   CALCULATE AMOUNT TO MOVE                     
         SR    RF,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE TO RECORD                               
*                                                                               
         MVI   L'BIIDGRS(RE),FLDSEPQ                                            
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE DRIVER PRINT IT                         
*                                                                               
OBIDLRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
EDTPTRN  DC    X'402020202020202020204B2020'                                    
*                                                                               
OBID$TB  DC    C'1',X'00',AL3(BIIDGRS-WBIWORKD) BILLED GROSS                    
OBID$TBL EQU   *-OBID$TB                  TABLE ENTRY LENGTH                    
         DC    C'4',X'00',AL3(BIIDNET-WBIWORKD) BILLED NET                      
         DC    C'5',X'00',AL3(BIIDCD-WBIWORKD)  BILLED CD                       
         DC    C'2',X'00',AL3(BIIDACT-WBIWORKD) BILLED ACTUAL AMOUNT            
         DC    C'7',X'00',AL3(BIIDTAX-WBIWORKD) BILLED TAX                      
         DC    C'1L',AL3(BIITGRS-WBIWORKD) HEADER GROSS                         
         DC    C'3L',AL3(BIITNET-WBIWORKD) HEADER NET                           
         DC    C'5L',AL3(BIITCD-WBIWORKD)  HEADER CD                            
         DC    C'2L',AL3(BIITACT-WBIWORKD) HEADER ACTUAL AMOUNT                 
         DC    C'7L',AL3(BIITTAX-WBIWORKD) HEADER TAX                           
         DC    C'8L',AL3(BIITGST-WBIWORKD) HEADER GST                           
*                                                                               
*                                                                               
*        TABLE FOR TOTALS                                                       
*                                                                               
OBID$TT  DC    C'1',X'00',AL3(BIIDGRS-WBIWORKD) BILLED GROSS                    
OBID$TTL EQU   *-OBID$TT                  TABLE ENTRY LENGTH                    
         DC    C'4',X'00',AL3(BIIDNET-WBIWORKD) BILLED NET                      
         DC    C'5',X'00',AL3(BIIDCD-WBIWORKD)  BILLED CD                       
         DC    C'2',X'00',AL3(BIIDACT-WBIWORKD) BILLED ACTUAL AMOUNT            
         DC    C'7',X'00',AL3(BIIDTAX-WBIWORKD) BILLED TAX                      
         DC    C'8',X'00',AL3(BIIDGST-WBIWORKD) BILLED GST                      
         DC    C'1L',AL3(BIITGRS-WBIWORKD) HEADER GROSS                         
         DC    C'4L',AL3(BIITNET-WBIWORKD) HEADER NET                           
         DC    C'5L',AL3(BIITCD-WBIWORKD)  HEADER CD                            
         DC    C'2L',AL3(BIITACT-WBIWORKD) HEADER ACTUAL AMOUNT                 
         DC    C'7L',AL3(BIITTAX-WBIWORKD) HEADER TAX                           
         DC    C'8L',AL3(BIITGST-WBIWORKD) HEADER GST                           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PREVIOUS BILLS-IBIPRV'              
***********************************************************************         
*                                                                     *         
*        ADD PREVIOUS BILL DATA TO SORT RECORD                        *         
*                                                                     *         
*        R2==>  XL3  - PREVIOUS BILL DATE                             *         
*               XL2  - PREVIOUS BILL NUMBER                           *         
*               XL4  - PREVIOUS BILL GROSS                            *         
*               XL4  - PREVIOUS BILL AGENCY COMMISSION                *         
*               XL4  - PREVIOUS BILL CASH DISCOUNT                    *         
*               XL1  - C'E'                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIPRV   NMOD1 0,**#IPV                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         MVI   17(R2),C'E'         FORCE NON-NULL DATA                          
*                                                                               
         XC    WBIPBLSV,WBIPBLSV   INIT BILL ELEMENT SAVEAREA                   
*                                                                               
         ICM   R3,15,PBBLLELA      POINT TO CURRENT BILL ELEMENT                
         BZ    IBIPRVX                                                          
*                                                                               
         L     R4,AIO1             ESTABLISH BUY RECORD                         
         USING PBUYRECD,R4                                                      
*                                                                               
         CLI   PBUYKRCD,PBUYKIDQ   SKIP IF NOT A BUY RECORD                     
         BNE   IBIPRVX                                                          
*                                                                               
         SR    RF,RF                                                            
         LA    R5,PBUYKEY+33       POINT TO 1ST ELEMENT IN BUY                  
         USING PBILELD,R5          ESTABLISH AS BILL ELEMENT                    
*                                                                               
IBIPRVLP DS    0H                                                               
*                                                                               
         CLI   PBILELEM,0          CHECK FOR END OF RECORD                      
         BE    IBIPRVDN                                                         
*                                                                               
         CLI   PBILELEM,PBILELQ    LOOK FOR BILL ELEMENT                        
         BNE   IBIPRVCN                                                         
*                                                                               
         OC    PBLDATE,PBLDATE     SKIP IF THERE IS NO BILL DATE                
         BZ    IBIPRVCN                                                         
*                                                                               
         CLC   PBPRD,PBPROD        MUST BE FOR CURRENT PRODUCT                  
         BNE   IBIPRVCN                                                         
*                                                                               
         CLC   PBLDATE,PBLDATE-PBILELD(R3) BEFORE CURRENT BILL ELEMENT          
         BNL   IBIPRVCN                                                         
*                                                                               
*        ADD SAVED PREVIOUS BILLED DATA AND UPDATE WITH THIS DATA               
*                                                                               
         MVC   WBIPBLBD,PBLDATE    REPLACE BILL DATE                            
         MVC   WBIPBLB#,PBINVNO    AND     BILL NUMBER                          
*                                                                               
*        ADD BILL ELEMENT AMOUNTS TO SAVE AREA                                  
*                                                                               
         ICM   RF,15,PBGROSS       GET CURRENT ELEMENT GROSS                    
         BZ    IBIPRVGX                                                         
*                                                                               
         ICM   RE,15,WBIPBLGR      GET PREVIOUS AMOUNT                          
         AR    RE,RF               INCREMENT AMOUNT                             
         STCM  RE,15,WBIPBLGR                                                   
*                                                                               
IBIPRVGX DS    0H                                                               
*                                                                               
         ICM   R1,15,PBAGYCOM      GET CURRENT ELEMENT AC                       
*                                                                               
         SR    RF,R1               GROSS-AC=NET                                 
*                                                                               
         ICM   RE,15,WBIPBLNT      GET PREVIOUS AMOUNT                          
         AR    RE,RF               INCREMENT AMOUNT                             
         STCM  RE,15,WBIPBLNT                                                   
*                                                                               
         ICM   RF,15,PBCSHDSC      GET CURRENT ELEMENT CD                       
         BZ    IBIPRVCX                                                         
*                                                                               
         ICM   RE,15,WBIPBLCD      GET PREVIOUS AMOUNT                          
         AR    RE,RF               INCREMENT AMOUNT                             
         STCM  RE,15,WBIPBLCD                                                   
*                                                                               
IBIPRVCX DS    0H                                                               
*                                                                               
IBIPRVCN DS    0H                                                               
*                                                                               
         IC    RF,PBILELEM+1       ELEMENT LENGTH                               
         LA    R5,PBILELEM(RF)     POINT TO NEXT ELEMENT IN BUY                 
         B     IBIPRVLP                                                         
*                                                                               
IBIPRVDN DS    0H                  HAVE BILL ELEMENT                            
*                                                                               
         MVC   0(L'WBIPBLSV,R2),WBIPBLSV  ADD OLD DATA TO SORT RECORD           
*                                                                               
IBIPRVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PREVIOUS BILLS-OBIPRV'              
***********************************************************************         
*                                                                     *         
*        PREVIOUS BILLING OUTPUT                                      *         
*                                                                     *         
*          ADD PREVIOUS BILL DATA TO INVOICE DETAIL RECORD            *         
*          PRINT ANY RECORDS THAT NEED PRINTING                       *         
*                                                                     *         
*        R2==>  XL3  - PREVIOUS BILL DATE                             *         
*               XL2  - PREVIOUS BILL NUMBER                           *         
*               XL4  - PREVIOUS BILL GROSS                            *         
*               XL4  - PREVIOUS BILL AGENCY COMMISSION                *         
*               XL4  - PREVIOUS BILL CASH DISCOUNT                    *         
*               XL1  - C'E'                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIPRV   NMOD1 0,**#OPV                                                         
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        ADD PREVIOUS BILLED BILL TO RECORD                                     
*                                                                               
         OC    0(3,R2),0(R2)       SKIP IF NO DATE                              
         BZ    OBIPRVDX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R2)),DUB   RE-FORMAT DATE                        
*                                                                               
         MVC   BIIDPBL#(2),DUB+2   BILL MONTH                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,3(R2)          BILL NUMBER                                  
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  BIIDPBL#+2(4),DUB   BILL NUMBER                                  
*                                                                               
OBIPRVDX DS    0H                                                               
*                                                                               
         MVI   BIIDPBL#+L'BIIDPBL#,FLDSEPQ                                      
*                                                                               
*        ADD PREVIOUS BILL AMOUNTS TO INVOICE DETAIL RECORD                     
*                                                                               
         LA    R0,3                3 AMOUNTS                                    
         LA    RF,5(R2)            POINT TO FIRST AMOUNT IN INPUT               
         LA    RE,BIIDPGRS         POINT TO FIRST AMOUNT IN RECORD              
*                                                                               
OBIPRVPL DS   0H                                                                
*                                                                               
         ICM   R3,15,0(RF)         GET INPUT AMOUNT                             
         BZ    OBIPRVP1                                                         
*                                                                               
         CVD   R3,DUB              CVD                                          
*                                                                               
         MVC   WORK(L'OBIPRVP),OBIPRVP SET EDIT PATTERN                         
         LA    R1,WORK+1           INIT MARKER                                  
*                                                                               
         EDMK  WORK(L'OBIPRVP),DUB+2  EDIT AND MARK DOLLARS                     
*                                                                               
         CP    DUB,=P'0'           IF NEGATIVE RESULT                           
         BNM   *+10                                                             
         BCTR  R1,0                   SET FLOATING MINUS SIGN                   
         MVI   0(R1),C'-'                                                       
*                                                                               
         LA    R4,WORK+L'OBIPRVP   CALCULATE AMOUNT TO MOVE                     
         SR    R4,R1               LENGTH OF SIGNIFICANT PART                   
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE TO RECORD                               
*                                                                               
OBIPRVP1 DS   0H                                                                
*                                                                               
         MVI   L'BIIDPGRS(RE),FLDSEPQ                                           
*                                                                               
OBIPRVPC DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            BUMP TO POINTERS                             
         LA    RE,L'BIIDPGRS+1(RE)                                              
         BCT   R0,OBIPRVPL                                                      
*                                                                               
OBIPRVPD DS    0H                                                               
*                                                                               
OBIPRVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBIPRVP  DC    X'402020202020202020204B2020'                                    
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-END OF RECORD-IBIEND'               
***********************************************************************         
*                                                                     *         
*        END OF A SORT RECORD                                         *         
*                                                                     *         
*          ADD PREVIOUS BILL DATA TO SORT RECORD                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IBIEND   NMOD1 0,**#IEND                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         ZAP   0(3,R2),=P'1'       MAKE SURE ITS A NUMERIC FIELD                
*                                                                               
IBIENDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-END OF RECORD-OBIEND'               
***********************************************************************         
*                                                                     *         
*        END OF A SORT RECORD                                         *         
*                                                                     *         
*          PRINT ANY RECORDS THAT NEED PRINTING                       *         
*                                                                     *         
*        R2==>  PL3'1'                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OBIEND   NMOD1 0,**#OEND                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
         TM    GLINDS,GLTOTLIN     IF TOTAL RECORD                              
         BNO   *+8                                                              
         MVI   PRTTOT,C'Y'         INDICATE TOTAL RECORD TO PRINT               
*                                                                               
*        DETERMINE RECORDS TO BE WRITTEN TO FILE                                
*                                                                               
         LA    R2,PRTSWS           POINT TO LIST OF PRINT SWITCHES              
         LA    R0,PRTSWSL          NUMBER OF SWITCHES                           
         LA    R3,RECTAB           POINT TO LIST OF RECORD ADDRESSES            
*                                                                               
OBIENDLP DS    0H                                                               
*                                                                               
         CLI   0(R2),C'Y'          IF SWITCH IS ON                              
         BNE   OBIENDCN                                                         
*                                                                               
         GOTO1 RECOUT              WRITE RECORD                                 
*                                                                               
         MVI   0(R2),0             CLEAR SWITCH                                 
*                                                                               
OBIENDCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT SWITCH                          
         LA    R3,4(R3)            BUMP TO NEXT ADDRESS                         
         BCT   R0,OBIENDLP                                                      
*                                                                               
OBIENDDN DS    0H                                                               
*                                                                               
OBIENDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF RECORD ADDRESSES                                              
*                                                                               
RECTAB   DS    0A                                                               
*                                                                               
         DC    A(BIAGRECA-WBIWORKD) AGENCY    RECORD                            
         DC    A(BIMERECA-WBIWORKD) MEDIA     RECORD                            
         DC    A(BICLRECA-WBIWORKD) CLIENT    RECORD                            
         DC    A(BIIHRECA-WBIWORKD) INVOICE   RECORD                            
         DC    A(BIDVRECA-WBIWORKD) DIVISION  RECORD                            
         DC    A(BIPRRECA-WBIWORKD) PRODUCT   RECORD                            
         DC    A(BIESRECA-WBIWORKD) ESTIMATE  RECORD                            
         DC    A(BIRGRECA-WBIWORKD) REGION    RECORD                            
         DC    A(BIDSRECA-WBIWORKD) DISTRICT  RECORD                            
         DC    A(BIPBRECA-WBIWORKD) PUB       RECORD                            
         DC    A(BIACRECA-WBIWORKD) AD CODE   RECORD                            
         DC    A(BIIDRECA-WBIWORKD) INSERTION RECORD                            
         DC    A(BIITRECA-WBIWORKD) TOTAL     RECORD                            
*                                                                               
RECTABL  EQU   *-RECTAB             LENGTH OF ADDRESS TABLE                     
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-RECORD WRITE-RECOUT'                
***********************************************************************         
*                                                                     *         
*        WRITE A RECORD TO FILE AFTER COLLAPSING                      *         
*                                                                     *         
*NTRY    R3 ==> DISPLACEMENT OF RECORD INTO WORKAREA                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RECOUT   NMOD1 0,**#ROUT                                                        
*                                                                               
         L     RC,GLAWORKD         RE-ESTABLISH WORKING STORAGE                 
         USING GEND,RC                                                          
*                                                                               
*        ELIMINATE TRAILING BLANKS FROM ALL FIELDS                              
*                                                                               
         XC    BIREC(256),BIREC    INIT RECORD WORK AREA                        
         XC    BIREC+256(256),BIREC+256                                         
*                                                                               
*        WILL MOVE RECORD TO WORKAREA RIGHT TO LEFT                             
*        DROPPING TRAILING BLANKS AND NULLS                                     
*                                                                               
         L     R3,0(R3)            DISPLACEMENT INTO WORKAREA                   
         LA    R3,0(R3,R6)         RELOCATE RECORD ADDRESS                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)          LENGTH OF RECORD                             
         BZ    RECOUTX             SHOULD NOT HAPPEN                            
*                                                                               
         SH    RE,=H'4'            ALLOW FOR LENGTH ET AL BYTES                 
*                                                                               
         LR    R0,RE               SAVE DATA LENGTH                             
*                                                                               
         LA    RE,4(RE,R3)         POINT TO END OF DATA                         
         BCTR  RE,0                                                             
*                                                                               
         LA    RF,BIREC+L'BIREC-1  POINT TO END OF WORKAREA                     
         SR    R2,R2               INIT TO DROP BLANKS                          
*                                                                               
ROUTMVLP DS    0H                                                               
*                                                                               
         LTR   R2,R2               IF BLANKS ARE BEING KEPT                     
         BNZ   *+12                                                             
         CLI   0(RE),C' '          OR BYTE IS NON-BLANK                         
         BNH   ROUTMVCN                                                         
*                                                                               
         MVC   0(1,RF),0(RE)          TRANSFER BYTE TO WORK RECORD              
         BCTR  RF,0                   BACK UP TO NEXT BYTE OF WORK REC          
*                                                                               
         LA    R2,1                   SET TO KEEP BLANKS                        
         CLI   0(RE),FLDSEPQ          IF A FIELD SEPARATOR                      
         BNE   *+6                                                              
         SR    R2,R2                     SET TO DROP BLANKS                     
*                                                                               
ROUTMVCN DS    0H                                                               
*                                                                               
         BCTR  RE,0                BACK UP A BYTE IN RECORD                     
         BCT   R0,ROUTMVLP                                                      
*                                                                               
         LA    RE,BIREC+L'BIREC-1  POINT TO END OF RECORD                       
*                                                                               
         CLI   0(RE),FLDSEPQ       ELIMINATE TRAILING FIELD SEPARATORS          
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         SR    RE,RF               LENGTH OF DATA                               
         LA    RE,4(RE)            ADD ON LENGTH BYTES                          
*                                                                               
         SH    RF,=H'3'            BACK UP TO LENGTH BYTE                       
         STCM  RE,3,0(RF)          SET RECORD LENGTH                            
*                                                                               
         XC    2(2,RF),2(RF)       THESE BYTES MUST BE NULLS                    
*                                                                               
*        ADD RECORD TO THE FILE                                                 
*                                                                               
         LR    R3,RF               RESET RECORD POINTER                         
*                                                                               
         L     R5,WBIFILEA         DCB ADDRESS                                  
*                                                                               
         MVC   WORK,0(R3)                                                       
*                                                                               
         PUT   (R5),(R3)                                                        
*                                                                               
         AP    TAPECNT,=P'1'       INCREMENT TAPE COUNTER                       
*                                                                               
RECOUTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-WORKING STOREAGE-WBIWORKD'          
***********************************************************************         
*                                                                     *         
*        PROGRAM WORKING STORAGE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
WBIWORKD DSECT 0D                                                               
*                                                                               
FLDSEPQ  EQU   C'º'                                                             
*                                                                               
TAPECNT  DS    PL4                 RECORDS TO TAPE COUNTER                      
*                                                                               
PBILELA  DS    A                   A(CURRENT BILL ELEMENT)                      
*                                                                               
WBIFILEA DS    A                   A(BIFILE DCB IN SPFUSER)                     
*                                                                               
WBIINVTA DS    A                   A(INVOICE RECORD TABLE)                      
WBIINVTX DS    A                   A(NEXT INVOICE RECORD)                       
WBIINVTL DS    A                   INVOICE RECORD TABLE LENGTH                  
*                                                                               
WBIGST   DS    PL8                 GST SAVEAREA                                 
WSORTSW  DS    C                   C'N' - DROP RECORD FROM SORT                 
*                                                                               
*        PREVIOUS BILLED DATA WORKAREA                                          
*                                                                               
WBIPBLSV DS    0XL17               PREVIOUS BILLED DATA                         
WBIPBLBD DS    XL3                 BILLED DATE - BINARY                         
WBIPBLB# DS    XL2                 BILL NUMBER                                  
WBIPBLGR DS    XL4                 GROSS                                        
WBIPBLNT DS    XL4                 NET                                          
WBIPBLCD DS    XL4                 CASH DISCOUNT                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PREVIOUS VALUE SAVEAREAS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBIAGYSV DS    XL(L'BIAGAGY)       AGENCY                                       
WBIMEDSV DS    XL(L'BIMEMED)       MEDIA                                        
WBICLTSV DS    XL(L'BICLCLT)       CLIENT                                       
WBIINVSV DS    XL(L'BIIHINV)       INVOICE                                      
WBIDIVSV DS    XL(L'BIDVDIV)       DIVISION                                     
WBIPRDSV DS    XL(L'BIPRPRD)       PRODUCT                                      
WBIESTSV DS    XL(L'BIESEST)       ESTIMATE                                     
WBIREGSV DS    XL(L'BIRGREG)       REGION                                       
WBIDSTSV DS    XL(L'BIDSDST)       DISTRICT                                     
WBIPUBSV DS    XL(L'BIPBPUB)       PUB     CODE                                 
WBIZNESV DS    XL(L'BIPBZNE)       ZONE    CODE                                 
WBIEDNSV DS    XL(L'BIPBEDN)       EDITION CODE                                 
WBIADCSV DS    XL(L'BIACADC)       AD CODE                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        RECORD PRINT SWITCHES                                        *         
*                                                                     *         
*          SWITCH = C'Y' MEANS PRINT IT                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRTSWS   DS    0C                                                               
PRTAGY   DS    C                   PRINT AGENCY    RECORD                       
PRTMED   DS    C                   PRINT MEDIA     RECORD                       
PRTCLT   DS    C                   PRINT CLIENT    RECORD                       
PRTINV   DS    C                   PRINT INVOICE   RECORD                       
PRTDIV   DS    C                   PRINT DIVISION  RECORD                       
PRTPRD   DS    C                   PRINT PRODUCT   RECORD                       
PRTEST   DS    C                   PRINT ESTIMATE  RECORD                       
PRTREG   DS    C                   PRINT REGION    RECORD                       
PRTDST   DS    C                   PRINT DISTRICT  RECORD                       
PRTPUB   DS    C                   PRINT PUB       RECORD                       
PRTADC   DS    C                   PRINT AD CODE   RECORD                       
PRTINS   DS    C                   PRINT INSERTION RECORD                       
PRTTOT   DS    C                   PRINT TOTAL     RECORD                       
PRTSWSL  EQU   *-PRTSWS            LENGTH OF SWITCHES                           
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PPBILEXD CONTROL BLOCK'             
       ++INCLUDE PPBILEXD                                                       
         TITLE 'PRWRI14-BILL INTERFACE TAPE-RECORD LAYOUTS'                     
***********************************************************************         
*                                                                     *         
*        RECORD LAYOUTS AND SAVEAREAS                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIRECA   DS    0CL516          *** BILL INTERFACE TAPE RECORD ***               
BIRECL   DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIREC    DS    XL512               RECORD                                       
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-AGENCY RECORD-BIAGREC'              
***********************************************************************         
*                                                                     *         
*        AGENCY RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIAGRECA DS    0X                  AGENCY RECORD AREA                           
BIAGLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIAGREC  DS    0X                  AGENCY RECORD                                
BIAGRID  DS    CL2                 AGENCY RECORD CODE                           
BIAGIDQ  EQU   C'AG'               AGENCY RECORD IDENTIFIER                     
         DS    AL1(FLDSEPQ)                                                     
BIAGNAME DS    CL33                AGENCY NAME                                  
         DS    AL1(FLDSEPQ)                                                     
BIAGADR1 DS    CL33                AGENCY ADDRESS LINE 1                        
         DS    AL1(FLDSEPQ)                                                     
BIAGADR2 DS    CL33                AGENCY ADDRESS LINE 2                        
         DS    AL1(FLDSEPQ)                                                     
BIAGADR3 DS    CL33                AGENCY ADDRESS LINE 3                        
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIAGRECL EQU   *-BIAGRECA          AGENCY RECORD LENGTH                         
*                                                                               
BIAGAGY  DS    CL2                 AGENCY CODE SAVEAREA                         
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-MEDIA RECORD-BIMEREC'               
***********************************************************************         
*                                                                     *         
*        MEDIA RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIMERECA DS    0X                  MEDIA RECORD AREA                            
BIMELREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIMEREC  DS    0X                  MEDIA RECORD                                 
BIMERID  DS    CL2                 MEDIA RECORD CODE                            
BIMEIDQ  EQU   C'ME'               MEDIA RECORD IDENTIFIER                      
         DS    AL1(FLDSEPQ)                                                     
BIMESYST DS    CL1                 SYSTEM - C'P' FOR PRINT                      
         DS    AL1(FLDSEPQ)                                                     
BIMEMED  DS    CL1                 MEDIA - STANDARD PRINT CODES                 
         DS    AL1(FLDSEPQ)                                                     
BIMENAME DS    CL20                MEDIA NAME                                   
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIMERECL EQU   *-BIMERECA          MEDIA RECORD LENGTH                          
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-CLIENT RECORD-BICLREC'              
***********************************************************************         
*                                                                     *         
*        CLIENT RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BICLRECA DS    0X                  CLIENT RECORD AREA                           
BICLLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BICLREC  DS    0X                  CLIENT RECORD                                
BICLRID  DS    CL2                 CLIENT RECORD CODE                           
BICLIDQ  EQU   C'CL'               CLIENT RECORD IDENTIFIER                     
         DS    AL1(FLDSEPQ)                                                     
BICLCLT  DS    CL3                 CLIENT CODE                                  
         DS    AL1(FLDSEPQ)                                                     
BICLNMBR DS    CL5                 CLIENT NUMBER                                
         DS    AL1(FLDSEPQ)                                                     
BICLNAME DS    CL20                CLIENT NAME                                  
         DS    AL1(FLDSEPQ)                                                     
BICLOFC  DS    CL1                 CLIENT OFFICE NUMBER                         
         DS    AL1(FLDSEPQ)                                                     
BICLAOFC DS    CL2                 CLIENT ACCOUNTING OFFICE CODE                
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BICLRECL EQU   *-BICLRECA          CLIENT RECORD LENGTH                         
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE HEADER-BIIHREC'             
***********************************************************************         
*                                                                     *         
*        INVOICE HEADER RECORD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIIHRECA DS    0X                  INVOICE HEADER RECORD AREA                   
BIIHLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIIHREC  DS    0X                  INVOICE HEADER RECORD                        
BIIHRID  DS    CL2                 INVOICE HEADER RECORD CODE                   
BIIHIDQ  EQU   C'IH'               INVOICE HEADER RECORD IDENTIFIER             
         DS    AL1(FLDSEPQ)                                                     
BIIHINV  DS    XL6                 INVOICE NUMBER - MONTH(2) PLUS NMBR          
         DS    AL1(FLDSEPQ)                                                     
BIIHINV# DS    CL10                INVOICE NUMBER EXPANDED-AS ON BILL           
         DS    AL1(FLDSEPQ)                                                     
BIIHIDTE DS    CL6                 INVOICE INVOICE DATE YYMMDD                  
         DS    AL1(FLDSEPQ)                                                     
BIIHDDTE DS    CL6                 INVOICE DUE     DATE YYMMDD                  
         DS    AL1(FLDSEPQ)                                                     
BIIHRDTE DS    CL6                 INVOICE RUN     DATE YYMMDD                  
         DS    AL1(FLDSEPQ)                                                     
BIIHBTYP DS    CL1                 INVOICE BILL TYPE                            
         DS    AL1(FLDSEPQ)                                                     
BIIHREV  DS    CL1                 REVERSAL                    Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHMAN  DS    CL1                 MANUAL                      Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHCLRD DS    CL1                 CLEARED ONLY                Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHCOMO DS    CL1                 COMMISSION ONLY             Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHCOMU DS    CL1                 UPFRONT COMMISSION BILL     Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHADJS DS    CL1                 SEPARATE ADJUSTMENT BILL    Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHCDS  DS    CL1                 SEPARATE CASH DISCOUNT BILL Y/N              
         DS    AL1(FLDSEPQ)                                                     
BIIHAOR  DS    CL1                 AGENCY OF RECORD BILL       Y/N              
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIIHRECL EQU   *-BIIHRECA          INVOICE HEADER RECORD LENGTH                 
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DIVISION RECORD-BIDVREC'            
***********************************************************************         
*                                                                     *         
*        DIVISION RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIDVRECA DS    0X                  DIVISION RECORD AREA                         
BIDVLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIDVREC  DS    0X                  DIVISION RECORD                              
BIDVRID  DS    CL2                 DIVISION RECORD CODE                         
BIDVIDQ  EQU   C'DV'               DIVISION RECORD IDENTIFIER                   
         DS    AL1(FLDSEPQ)                                                     
BIDVDIV  DS    CL3                 DIVISION CODE                                
         DS    AL1(FLDSEPQ)                                                     
BIDVNAME DS    CL20                DIVISION NAME                                
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIDVRECL EQU   *-BIDVRECA          DIVISION RECORD LENGTH                       
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PRODUCT RECORD-BIPRREC'             
***********************************************************************         
*                                                                     *         
*        PRODUCT RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIPRRECA DS    0X                  PRODUCT RECORD AREA                          
BIPRLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIPRREC  DS    0X                  PRODUCT RECORD                               
BIPRRID  DS    CL2                 PRODUCT RECORD CODE                          
BIPRIDQ  EQU   C'PR'               PRODUCT RECORD IDENTIFIER                    
         DS    AL1(FLDSEPQ)                                                     
BIPRPRD  DS    CL3                 PRODUCT CODE                                 
         DS    AL1(FLDSEPQ)                                                     
BIPRNMBR DS    CL5                 PRODUCT NUMBER                               
         DS    AL1(FLDSEPQ)                                                     
BIPRNAME DS    CL20                PRODUCT NAME                                 
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIPRRECL EQU   *-BIPRRECA          PRODUCT RECORD LENGTH                        
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-ESTIMATE RECORD-BIESREC'            
***********************************************************************         
*                                                                     *         
*        ESTIMATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIESRECA DS    0X                  ESTIMATE RECORD AREA                         
BIESLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIESREC  DS    0X                  ESTIMATE RECORD                              
BIESRID  DS    CL2                 ESTIMATE RECORD CODE                         
BIESIDQ  EQU   C'ES'               ESTIMATE RECORD IDENTIFIER                   
         DS    AL1(FLDSEPQ)                                                     
BIESEST  DS    CL4                 ESTIMATE NUMBER                              
         DS    AL1(FLDSEPQ)                                                     
BIESNAME DS    CL20                ESTIMATE DESCRIPTION                         
         DS    AL1(FLDSEPQ)                                                     
BIESNAM2 DS    CL20                ESTIMATE DESCRIPTION-2                       
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIESRECL EQU   *-BIESRECA          ESTIMATE RECORD LENGTH                       
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-REGION RECORD-BIRGREC'              
***********************************************************************         
*                                                                     *         
*        REGION RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIRGRECA DS    0X                  REGION RECORD AREA                           
BIRGLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIRGREC  DS    0X                  REGION RECORD                                
BIRGRID  DS    CL2                 REGION RECORD CODE                           
BIRGIDQ  EQU   C'RG'               REGION RECORD IDENTIFIER                     
         DS    AL1(FLDSEPQ)                                                     
BIRGREG  DS    CL3                 REGION CODE                                  
         DS    AL1(FLDSEPQ)                                                     
BIRGNAME DS    CL20                REGION NAME                                  
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIRGRECL EQU   *-BIRGRECA          REGION RECORD LENGTH                         
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-DISTRICT RECORD-BIDSREC'            
***********************************************************************         
*                                                                     *         
*        DISTRICT RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIDSRECA DS    0X                  DISTRICT RECORD AREA                         
BIDSLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIDSREC  DS    0X                  DISTRICT RECORD                              
BIDSRID  DS    CL2                 DISTRICT RECORD CODE                         
BIDSIDQ  EQU   C'DS'               DISTRICT RECORD IDENTIFIER                   
         DS    AL1(FLDSEPQ)                                                     
BIDSDST  DS    CL3                 DISTRICT CODE                                
         DS    AL1(FLDSEPQ)                                                     
BIDSNAME DS    CL20                DISTRICT NAME                                
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIDSRECL EQU   *-BIDSRECA          DISTRICT RECORD LENGTH                       
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-PUB RECORD-BIPBREC'                 
***********************************************************************         
*                                                                     *         
*        PUB RECORD                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIPBRECA DS    0X                  PUB RECORD AREA                              
BIPBLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIPBREC  DS    0X                  PUB RECORD                                   
BIPBRID  DS    CL2                 PUB RECORD CODE                              
BIPBIDQ  EQU   C'PB'               PUB RECORD IDENTIFIER                        
         DS    AL1(FLDSEPQ)                                                     
BIPBPUB  DS    CL8                 PUB CODE                                     
         DS    AL1(FLDSEPQ)                                                     
BIPBZNE  DS    CL2                 PUB ZONE                                     
         DS    AL1(FLDSEPQ)                                                     
BIPBEDN  DS    CL3                 PUB EDITION                                  
         DS    AL1(FLDSEPQ)                                                     
BIPBNAM1 DS    CL20                PUB NAME - LINE 1                            
         DS    AL1(FLDSEPQ)                                                     
BIPBNAM2 DS    CL20                PUB NAME - LINE 2                            
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIPBRECL EQU   *-BIPBRECA          PUB RECORD LENGTH                            
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-AD CODE RECORD-BIACREC'             
***********************************************************************         
*                                                                     *         
*        AD CODE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIACRECA DS    0X                  AD CODE RECORD AREA                          
BIACLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIACREC  DS    0X                  AD CODE RECORD                               
BIACRID  DS    CL2                 AD CODE RECORD CODE                          
BIACIDQ  EQU   C'AC'               AD CODE RECORD IDENTIFIER                    
         DS    AL1(FLDSEPQ)                                                     
BIACADC  DS    CL6                 AD CODE                                      
         DS    AL1(FLDSEPQ)                                                     
BIACCAP1 DS    CL25                AD CODE CAPTION PART 1                       
         DS    AL1(FLDSEPQ)                                                     
BIACCAP2 DS    CL25                AD CODE CAPTION PART 2                       
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIACRECL EQU   *-BIACRECA          AD CODE RECORD LENGTH                        
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INSERT DETAIL-BIIDREC'              
***********************************************************************         
*                                                                     *         
*        INSERTION DETAIL RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIIDRECA DS    0X                  INSERTION DETAIL RECORD AREA                 
BIIDLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIIDREC  DS    0X                  INSERTION DETAIL RECORD                      
BIIDRID  DS    CL2                 INSERTION DETAIL RECORD CODE                 
BIIDIDQ  EQU   C'ID'               INSERTION DETAIL RECORD IDENTIFIER           
         DS    AL1(FLDSEPQ)                                                     
BIIDDTE  DS    CL6                 INSERTION DATE YYMMDD                        
         DS    AL1(FLDSEPQ)                                                     
BIIDLNE  DS    CL3                 INSERTION LINE NUMBER                        
         DS    AL1(FLDSEPQ)                                                     
BIIDSHRS DS    CL6                 SPACE SHARE IF ZZZ 999.99                    
         DS    AL1(FLDSEPQ)                                                     
BIIDSHRC DS    CL6                 COST  SHARE IF ZZZ 999.99                    
         DS    AL1(FLDSEPQ)                                                     
BIIDSPC  DS    CL50                SPACE DESCRIPTION                            
         DS    AL1(FLDSEPQ)                                                     
BIIDURTE DS    CL8                 COLUMN/INCH UNIT RATE 99.99999               
         DS    AL1(FLDSEPQ)                                                     
BIIDMOS  DS    CL4                 MONTH OF SERVICE YYMM                        
         DS    AL1(FLDSEPQ)                                                     
BIIDCOM1 DS    CL50                COMMENT LINE 1                               
         DS    AL1(FLDSEPQ)                                                     
BIIDCOM2 DS    CL50                COMMENT LINE 2                               
         DS    AL1(FLDSEPQ)                                                     
BIIDCOM3 DS    CL50                COMMENT LINE 3                               
         DS    AL1(FLDSEPQ)                                                     
BIIDCOM4 DS    CL50                COMMENT LINE 4                               
         DS    AL1(FLDSEPQ)                                                     
BIIDCOM5 DS    CL50                COMMENT LINE 5                               
         DS    AL1(FLDSEPQ)                                                     
BIIDBBSE DS    CL4                 BILL BASIS       G/N                         
         DS    AL1(FLDSEPQ)                                                     
BIIDCBSE DS    CL4                 COMMISSION BASIS G/N                         
         DS    AL1(FLDSEPQ)                                                     
BIIDCAPC DS    CL8                 COMMISSION ADJUSTMENT % (-)99.9999           
         DS    AL1(FLDSEPQ)                                                     
BIIDGRS  DS    CL13                GROSS       (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDNET  DS    CL13                NET         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDCD   DS    CL13                CD          (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDACT  DS    CL13                BILL AMOUNT (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDTAX  DS    CL13                TAX         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDGST  DS    CL13                GST         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIIDPBL# DS    CL6                 PREVIOUS BILL NUMBER                         
         DS    AL1(FLDSEPQ)                                                     
BIIDPGRS DS    CL13                PREVIOUS BILL GROSS (-)999999999.99          
         DS    AL1(FLDSEPQ)                                                     
BIIDPNET DS    CL13                PREVIOUS BILL NET   (-)999999999.99          
         DS    AL1(FLDSEPQ)                                                     
BIIDPCD  DS    CL13                PREVIOUS BILL CD    (-)999999999.99          
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIIDRECL EQU   *-BIIDRECA          INSERTION DETAIL RECORD LENGTH               
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE TOTAL-BIITREC'              
***********************************************************************         
*                                                                     *         
*        INVOICE TOTAL RECORD                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BIITRECA DS    0X                  INVOICE TOTAL RECORD AREA                    
BIITLREC DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
BIITREC  DS    0X                  INVOICE TOTAL RECORD                         
BIITRID  DS    CL2                 INVOICE TOTAL RECORD CODE                    
BIITIDQ  EQU   C'IT'               INVOICE TOTAL RECORD IDENTIFIER              
         DS    AL1(FLDSEPQ)                                                     
BIITGRS  DS    CL13                GROSS       (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIITNET  DS    CL13                NET         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIITCD   DS    CL13                CD          (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIITACT  DS    CL13                BILL AMOUNT (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIITTAX  DS    CL13                TAX         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
BIITGST  DS    CL13                GST         (-)999999999.99                  
         DS    AL1(FLDSEPQ)                                                     
*                                                                               
BIITRECL EQU   *-BIITRECA          INVOICE TOTAL RECORD LENGTH                  
*                                                                               
WBIWORKX EQU   *                   END OF WORKING STORAGE                       
*                                                                               
         TITLE 'PRWRI14-BILL INTERFACE TAPE-INVOICE TABLE -INVTBLD '            
***********************************************************************         
*                                                                     *         
*        INVOICE RECORD TABLE ENTRY                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
INVTBLD  DSECT                                                                  
INVTREC  DS    CL150               INVOICE RECORD                               
INVTENTL EQU   *-INVTBLD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
*                                                                               
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*PRWRIWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRWRIWORKD                                                     
         PRINT ON                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
*PPRELEM                                                                        
         PRINT OFF                                                              
PPRELD   DSECT                    PRODUCT ELEMENT                               
PPRELQ   EQU   X'21'              PRODUCT ELEMENT ID                            
       ++INCLUDE PPRELEM                                                        
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDLOGOD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DMPRTQL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
*DDBUFFALOD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*DRGLOBAL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
*DRIVETABLE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
         PRINT ON                                                               
*DRINTRECD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DRINTRECD                                                      
         PRINT ON                                                               
*PRWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE PRWRIFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
*DDGENTWA                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*DDTWACOND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074PRWRI14   01/08/13'                                      
         END                                                                    
