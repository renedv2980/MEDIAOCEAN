*          DATA SET SPSFM08    AT LEVEL 103 AS OF 02/24/20                      
*PHASE T21708A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21708  -- CLIENT 2 RECORD MAINTENANCE               *         
*                                                                     *         
*  COMMENTS:     MAINTAINS CLIENT 2 RECORDS                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM78 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- CLIENT RECORD POINTER                                    
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21708 - CLIENT 2 RECORD MAINTENANCE AND LIST'                  
T21708   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1708**,R7,RR=R3                                              
         USING CLTHRD,R6           CLIENT REC DSECT                             
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         USING OFFICED,OFCBLK                                                   
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       LA    R2,CL2MEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   CL2MEDN,MEDNM       MEDIA NAME                                   
         OI    CL2MEDNH+6,X'80'                                                 
*                                                                               
*        CLI   ACTNUM,ACTDIS                                                    
*        BE    VK05                                                             
*        CLI   SVAPROF+7,C'C'      CANADIAN CLIENT                              
*        BNE   VK05                                                             
*        CLI   QMED,C'C'           MEDIA C AND N ONLY FOR DISPLAY               
*        BE    ERRINV                                                           
*        CLI   QMED,C'N'                                                        
*        BE    ERRINV                                                           
*                                                                               
VK05     L     RE,AIO                                                           
         USING AGYHDRD,RE          SAVING SOME IMPORTANT AGENCY INFO            
         MVC   SVAGYFL1,AGYFLAG1                                                
*                                                                               
         XC    SVRFPID,SVRFPID                                                  
         LA    RE,24(RE)                                                        
VK06     CLI   0(RE),0                                                          
         BE    VK10                                                             
         CLI   0(RE),X'71'         EXT ELEM                                     
         BE    VK07                                                             
         LLC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     VK06                                                             
         USING AGYEXTEL,RE         SAVING SOME IMPORTANT AGENCY INFO            
VK07     MVC   SVRFPID,AGYPRNID                                                 
         DROP  RE                                                               
*                                                                               
VK10     LA    R2,CL2CLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
*                                                                               
         XC    KEY,KEY             BUILD THE KEY                                
         LA    R6,KEY                                                           
         MVI   CKEYTYPE,X'00'                                                   
         MVC   CKEYAM,BAGYMD       AGENCY/MEDIA CODE                            
         MVC   CKEYCLT,BCLT        BINARY CLIENT CODE                           
*                                                                               
         OI    CL2MEDH+6,X'80'     TRANSMIT ALL KEY FIELDS                      
         OI    CL2CLTH+6,X'80'     TO ENFORCE ALL CAPS DISPLAY                  
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         L     R6,AIO                                                           
         LA    RE,CLTHDR                                                        
         LA    RF,CEXTNAME-CLTHDR                                               
         AR    RE,RF                                                            
         XC    0(L'CEXTNAME,RE),0(RE)  INIT CLIENT EXTENDED NAME                
*                                                                               
         CLI   CL2CEXNH+5,0        ANY CLIENT EXTENDED NAME                     
         BE    VR00                                                             
*                                                                               
         LA    R2,CL2CEXNH         CLIENT EXTENDED NAME                         
         GOTO1 ANY                                                              
         LA    RE,CLTHDR                                                        
         LA    RF,CEXTNAME-CLTHDR                                               
         AR    RE,RF                                                            
         MVC   0(L'CEXTNAME,RE),WORK                                            
*                                                                               
VR00     CLI   SVAPROF+7,C'C'      CANADIAN CLIENT                              
         BNE   VR01                                                             
         CLI   QMED,C'C'           MEDIA C AND N ONLY FOR DISPLAY               
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
*                                                                               
VR01     MVC   SVCLTKEY,KEY        BACK UP THE KEY                              
         OI    GENSTAT2,RETEQSEL   PAUSE AFTER CHANGE                           
*                                                                               
         L     R6,AIO                                                           
         MVC   MYBYTE1,COPT1       SAVE THE OLD OPTIONS BYTES                   
         MVC   MYBYTE2,COPT2                                                    
         MVC   MYBYTE4,COPT4                                                    
         MVI   RETFLAG,1           RETURN TO LIST IF NO CHANGE                  
*                                                                               
* NOW SET UP ADDRESSES AND USINGS FOR THE MAIN LOOP                             
*                                                                               
         LA    R1,TABLE1           A(TABLE OF WHAT GOES ON THE SCREEN)          
         USING TABLE1D,R1                                                       
         LA    R2,CL2FRSTH         A(FIRST SCREEN LITERAL FIELD)                
         USING FIELDD,R2                                                        
*                                                                               
VR10     CLI   PROF,X'FF'          AT END OF TABLE?                             
         BE    VRX                 YES - SO ALL DONE                            
*                                                                               
         LA    R3,PROFILE          A(PROFILE RECORD RESPONSES)                  
         LLC   R4,PROF             R4 = DISP INTO PROFILE RESPONSES             
         AR    R3,R4               A(THIS PROFILE REPONSE)                      
         MVC   BYTE,0(R3)          PUT IT IN BYTE                               
         CLI   BYTE,C'Y'           CLIENT USING THIS FEATURE?                   
         BNE   VR20                NO - SO CONTINUE                             
*                                                                               
         TM    DATAH+4,X'80'       ANYTHING CHANGED THIS TIME                   
         BZ    *+8                 NO - SO CONTINUE                             
         MVI   RETFLAG,0           ELSE - SET OFF RETURN TO LIST FLAG           
*                                                                               
         ICM   RF,15,VALRTN        RF = A(VALIDATE ROUTINE)                     
         A     RF,RELO             RELOCATE IT                                  
         BASR  RE,RF               AND GO                                       
         LA    R2,LFIELDD(R2)      ELSE - INC TO NEXT SCREEN FIELD              
*                                                                               
VR20     LA    R1,LTABLE1D(R1)     INC TO NEXT TABLE ENTRY                      
         B     VR10                LOOP BACK TO CHECK IT                        
*                                                                               
*---------------------------------------------------------------------*         
*              FINAL PROCESSING OF THE RECORD BEFORE OUTPUT           *         
*---------------------------------------------------------------------*         
VRX      DS    0H                  CHANGING AN EXISTING RECORD                  
         BAS   RE,PTREC            STORE THE RECORD                             
*                                  FOR CANADIAN CLIENT, RECORD NEED TO          
         BAS   RE,SPCN             BE STORED FOR MEDIA C AND N ALSO             
*                                                                               
*        ADD REQUEST RECORD --> DISPLAY CHANGED RECORD --> EXIT.                
*---------------------------------------------------------------------*         
*  ADDING REQUEST RECORD FOR TRANSACTION                              *         
*---------------------------------------------------------------------*         
REQREC   MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         XC    0(150,R6),0(R6)                                                  
         MVI   10(R6),41                                                        
         MVI   14(R6),106                                                       
         LA    R6,26(R6)                                                        
         MVI   0(R6),X'40'                                                      
         MVC   1(79,R6),0(R6)                                                   
         MVC   0(2,R6),=C'41'                                                   
         MVC   2(2,R6),14(RA)                                                   
         MVC   4(1,R6),CL2MED                                                   
         MVC   5(3,R6),CL2CLT                                                   
         OC    5(3,R6),SPACES                                                   
         MVC   68(7,R6),=C'CONTROL'                                             
         MVI   61(R6),C'C'                                                      
         MVI   63(R6),C'C'                                                      
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',0(R6),0(R6)                   
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
VRXX     B     DR                   REDISPLAY CHANGED RECORD                    
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
*                          PUTREC                                     *         
***********************************************************************         
PTREC    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',SVCLTKEY+14,AIO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
***********************************************************************         
*        EDITING RECORD FOR CLIENT CLIENTS FOR ALL MEDIA              *         
***********************************************************************         
SPCN     NTR1                                                                   
         CLI   SVAPROF+7,C'C'      CANADIAN CLIENT                              
         BNE   SPCNX                                                            
         CLI   CL2MED,C'T'         AND MEDIA T                                  
         BNE   SPCNX                                                            
*                                                                               
         LA    R2,2                LOOP COUNTER                                 
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'03'         FOR MEDIA N                                  
*                                                                               
SPCN10   MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES THE RECORD EXIST?                       
         BE    SPCNCHA             YES, THEN CHANGE IT                          
*                                                                               
         DC    H'0'                IT HAS TO BE THERE, THIS IS CL2              
*                                                                               
SPCNCHA  MVC   AIO,AIO3                                                         
         L     R1,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   0(13,R6),0(R1)      COPY THE RECORD, BUT DIFFERENT KEY           
         MVC   AIO,AIO1            WRITE THE RECORD TO FILE                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SPCN20   L     R6,AIO                                                           
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'08'         FOR MEDIA C                                  
         BCT   R2,SPCN10                                                        
*                                                                               
SPCNX    MVC   0(13,R6),SVCLTKEY   RESTORE THE MEDIA                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE ROUTINES                                      *         
***********************************************************************         
*                                  FOR ALL VALIDATE ROUTINES,                   
*                                  INPUT:  R1=A(CURRENT TABLE ENTRY)            
*                                  INPUT:  R2=A(TARGET SCR FLD GROUP)           
*                                  INPUT:  R6=A(CLT REC)                        
*                                  OUTPUT: EDIT CLT REC, OR ERROR EXIT          
*                                                                               
*---------------------------------------------------------------------*         
*              VALCOS2 - VALIDATE THE 2ND COST FEATURE                *         
*---------------------------------------------------------------------*         
VALCOS2  NTR1                                                                   
         USING FIELDD,R2                                                        
*                                                                               
* INITIATIVE CAN HAVE A 'T' IN THE COS2 FIELD EVEN IF AGY REC SAYS              
* COS2 REQUIRED (EJOR 6/20/07)                                                  
* ADDED CARAT (HWON 10/12/12)                                                   
* ADDED HAVAS (HWON 09/06/13)                                                   
         CLC   AGENCY,=C'WI'       INITIATIVE?                                  
         BE    VCOS02                                                           
         CLC   AGENCY,=C'WJ'       INITIATIVE? (WITEST)                         
         BE    VCOS02                                                           
*                                                                               
         CLC   AGENCY,=C'UB'       IS THIS CARAT/UB?                            
         BE    VCOS02               YES                                         
         CLC   AGENCY,=C'PH'       TEST AGENCY PHNY?                            
         BE    VCOS02               NO                                          
         CLC   AGENCY,=C'FM'       IS THIS HAVAS/FM?                            
         BNE   VCOS04               YES                                         
*                                                                               
VCOS02   OC    CCOST2,CCOST2       IF COS2 WAS SET, TRADE STILL NOGO            
         BNZ   VCOS04                                                           
         CLI   DATA,C'T'                                                        
         BE    VCOS08                                                           
         CLC   DATA,SPACES                                                      
         BH    VCOS04                                                           
         NI    COPT3,X'FF'-COP3COSQ                                             
         NI    COPT4,X'FF'-COP4TRD                                              
         NI    COPT1,X'FF'-COP1COSQ                                             
*                                                                               
VCOS04   TM    SVAGYFL1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BO    VCOS10              YES - SO CONTINUE                            
*                                                                               
VCOS06   NI    COPT3,X'FF'-COP3COSQ                                             
         NI    COPT4,X'FF'-COP4TRD                                              
         CLI   DATA,C'O'           OPTIONAL                                     
         BNE   VCOS08                                                           
         OI    COPT3,COP3COSQ                                                   
         NI    COPT1,X'FF'-COP1COSQ  MAKE SURE Y/N IS OFF                       
         B     VCOSX               DONE                                         
*                                                                               
VCOS08   CLI   DATA,C'T'           TRADE?                                       
         BNE   VCOS09              NO                                           
         OI    COPT4,COP4TRD                                                    
         NI    COPT1,X'FF'-COP1COSQ  MAKE SURE Y/N IS OFF                       
         B     VCOSX               DONE                                         
*                                                                               
VCOS09   BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         B     VCOSX               DONE                                         
*                                                                               
VCOS10   XC    CCOST2,CCOST2       CLEAR THE FIELD                              
         CLI   DATAH+5,X'0'        ANY INPUT?                                   
         BE    VCOSX               NO - SO ALL DONE                             
         TM    COPT4,COP4TRD       IF IT WAS TRADE, COS2 NOGO                   
         BNZ   ERRINV                                                           
*                                                                               
         LLC   R4,DATAH+5                                                       
         GOTO1 CASHVAL,DMCB,(6,DATA),(R4)                                       
         CLI   DMCB,0                                                           
         BNE   ERRCFAC             COST FACTOR REQD                             
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'      MAX 9.999999                                 
         BH    ERRCFAC             COST FACTOR REQD                             
*                                                                               
         C     R3,=F'0'             .LT. 0?                                     
         BL    VERR                YES - SO ERROR                               
*                                                                               
         MVC   CCOST2,DMCB+4       ELSE - SAVE THE COST FACTOR                  
         OC    CCOST2,CCOST2       ZERO?                                        
         BNZ   VCOSX               NO - SO CONTINUE                             
*                                                                               
         OI    CCOST2,X'80'        ELSE - SET 'ZERO WAS INPUT' BIT              
*                                                                               
VCOSX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALDBL - VALIDATE THE DOUBLE BOOK CHECK FEATURE        *         
*---------------------------------------------------------------------*         
VALDBL   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VDBLX    B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALINFO - VALIDATE THE INFOMERCIAL FEATURE             *         
*---------------------------------------------------------------------*         
VALINFO  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                                                               
         TM    MYBYTE1,COP1INFQ    WAS CLIENT INFOMERCIAL?                      
         BNO   VINFX               NO - SO CONTIUE                              
*                                                                               
         TM    COPT1,COP1INFQ      ELSE - IS IT STILL INFOMERCIAL?              
         BO    VINFX               YES - SO CONTINUE                            
*                                                                               
         B     ERRINFO             ELSE - ERROR                                 
*                                                                               
VINFX    B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALMGR - VALIDATE THE MARKET GROUP FEATURE             *         
*---------------------------------------------------------------------*         
VALMGR   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VMGRX    B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALXJ1 - VALIDATE THE J1 REPORT EXCLUDE FEATURE        *         
*---------------------------------------------------------------------*         
VALXJ1   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VXJ1X    B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALXA7 - VALIDATE THE A7 REPORT EXCLUDE FEATURE        *         
*---------------------------------------------------------------------*         
VALXA7   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VXA7X    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALCTA - VALIDATE THE CTA CLIENT FEATURE               *         
*---------------------------------------------------------------------*         
VALCTA   NTR1                                                                   
         USING FIELDD,R2                                                        
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                                                               
         CLI   DATA,C'Y'           IS RESPONSE 'Y'ES?                           
         BE    VCTAX               YES - SO CONTINUE                            
*                                                                               
         CLI   T217FFD+1,C'*'      ELSE - IS THIS A DDS TERMINAL?               
         BNE   VERR                NO - SO ERROR                                
*                                                                               
VCTAX    B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              VALGMI - VALIDATE THE GMI CLIENT FEATURE               *         
*---------------------------------------------------------------------*         
VALGMI   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                                                               
         TM    MYBYTE1,COP1GMI     WAS CLIENT GMI?                              
         BNO   VGMIX               NO - SO CONTINUE                             
*                                                                               
         TM    COPT1,COP1GMI       ELSE - IS IT STILL GMI?                      
         BO    VGMIX               YES - SO CONTINUE                            
*                                                                               
         B     ERRGMI              ELSE - ERROR                                 
*                                                                               
VGMIX    B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALUPL - VALIDATE THE UPLOAD DELETED BUYS FEATURE      *         
*---------------------------------------------------------------------*         
VALUPL   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VUPLX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALZEN - VALIDATE THE ZENITH CLIENT FEATURE            *         
*              ** NOW EXTRACT CLIENT CODE FOR ANY USER                *         
*---------------------------------------------------------------------*         
VALZEN   NTR1                                                                   
         USING FIELDD,R2                                                        
         XC    CZENCLT,CZENCLT                                                  
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VZENX               NO - SO JUST EXIT                            
*                                                                               
         CLI   DATAH+5,2           AT LEAST 2 CHARS I/P?                        
         BL    VERR                NO - SO ERROR                                
*                                                                               
         CLI   DATAH+5,3           MORE THAN 3 CHARS INPUT?                     
         BH    VERR                YES - SO ERROR                               
*                                                                               
         LLC   R3,DATAH+5          ELSE - R3 = L(DATA)                          
         LA    RE,DATA             RE = A(DATA)                                 
*                                                                               
VZEN10   LA    RF,ALPHANUM                                                      
*                                                                               
VZEN20   CLI   0(RF),0                                                          
         BE    VERR                                                             
*                                                                               
         CLC   0(1,RE),0(RF)                                                    
         BE    VZEN30                                                           
         LA    RF,1(RF)                                                         
         B     VZEN20                                                           
*                                                                               
VZEN30   LA    RE,1(RE)                                                         
         BCT   R3,VZEN10                                                        
*                                                                               
         IC    R3,DATAH+5                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CZENCLT(0),DATA     EX MOVE IN THE ZENITH CLIENT CODE            
*                                                                               
VZENX    B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              VALNPWB - VALIDATE THE DISALLOW PW BILLING FEATURE     *         
*---------------------------------------------------------------------*         
VALNPWB  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VPWBX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALPWPCT - VALIDATE THE PW PERCENT FEATURE             *         
*---------------------------------------------------------------------*         
VALPWPCT NTR1                                                                   
         USING FIELDD,R2                                                        
         XC    CPWPCT,CPWPCT                                                    
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VPCTX               NO - SO JUST EXIT                            
*                                                                               
         LLC   R4,DATAH+5          ELSE - R4 = L(DATA)                          
         GOTO1 CASHVAL,DMCB,(2,DATA),(R4)  CHECK THE INPUT                      
         CLI   DMCB,0              NUMERIC DATA INPUT?                          
         BNE   VERR                NO - SO ERROR                                
*                                                                               
         L     R3,4(R1)                                                         
         CH    R3,=H'10000'        MAX 100%                                     
         BNL   VERR                TOO BIG A NUMBER INPUT                       
*                                                                               
         MVC   CPWPCT,DMCB+5                                                    
         OC    CPWPCT,CPWPCT       0% INPUT?                                    
         BNZ   VPCTX               NO - SO CONTINUE                             
         OI    CPWPCT,X'80'        ELSE - SET SPECIAL FLAG FOR PW%=0            
*                                                                               
VPCTX    B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              VALTRADE - VALIDATE THE TRADE FEATURE                  *         
*---------------------------------------------------------------------*         
VALTRADE NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    SVAGYFL1,AGYTRDQ    TRADE AGENCY?                                
         BZ    VTRD10              NO - SO CONTINUE                             
*                                                                               
         MVI   DATA,C'Y'           ELSE - FORCE TRADE ON                        
         OI    DATAH+6,X'80'                                                    
*                                                                               
VTRD10   CLC   =C'DIY',DATA        USER ENTERED 'DIY'?                          
         BNE   VTRD20              NO - CHECK Y/N                               
*                                                                               
         OI    COPT2,COP2DIY       TURN ON DIY TRADE CLIENT                     
         NI    COPT2,X'FF'-COP2TRAD     TURN OFF TRADE CLIENT                   
         B     VTRDX               DONE                                         
*                                                                               
VTRD20   BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                                                               
         TM    MYBYTE2,COP2TRAD    WAS CLIENT TRADE?                            
         BNO   VTRD30              NO - SO CONTINUE                             
*                                                                               
         TM    COPT2,COP2TRAD      ELSE - IS IT STILL TRADE?                    
         BZ    ERRTRD              NO - ERROR                                   
*                                                                               
VTRD30   NI    COPT2,X'FF'-COP2DIY TURN OFF DIY TRADE CLIENT                    
*                                                                               
VTRDX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALFRZ - VALIDATE THE FREEZE FEATURE - Y/N OR MON/YR   *         
*---------------------------------------------------------------------*         
VALFRZ   NTR1                                                                   
         LR    R3,R1               SAVE THE A(CURRENT TABLE ENTRY)              
         USING FIELDD,R2                                                        
*                                                                               
* CHECK FOR DATE FORMAT FIRST, IF THERE IS DATE, SET FREEZE BIT OFF,            
* AND BRANCH AROUND VALYN.                                                      
*                                                                               
         MVI   WORK,0              CAN ENTER MMM/YY+ OR MMM/YY-                 
         CLI   DATA+6,C'-'                                                      
         BNE   *+8                                                              
         MVI   WORK,X'80'                                                       
         CLI   DATA+6,C'+'                                                      
         BNE   *+8                                                              
         MVI   WORK,X'40'                                                       
         MVI   DATA+6,C' '         CLEAR FOR DATVAL                             
*                                                                               
         GOTO1 DATVAL,DMCB,(2,DATA),WORK+10                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VFRZ10                                                           
         GOTO1 DATCON,DMCB,(0,WORK+10),(3,WORK+20)                              
         OC    WORK+21(1),WORK     SET +/- BITS                                 
         MVC   CLOCKYM,WORK+20                                                  
*                                                                               
         LR    R1,R3               RESTORE THE A(CURRENT TABLE ENTRY)           
         USING TABLE1D,R1          SET FREEZE BIT OFF                           
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,(3)      R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         MVC   MYBYTE2,0(R4)       SAVE OFF ORIGINAL VALUE                      
         MVC   HALF(1),OPTEQU      SAVE THE OPTIONS MASK                        
         XI    HALF,X'FF'          TURN ON EVERYTHING BUT THE BIT               
         NC    BYTE,HALF           RE-SET THE BIT TO START                      
         MVC   0(1,R4),BYTE        PUT RESULT BACK IN RECORD                    
         DROP  R1                                                               
         B     VFRZ20                                                           
*                                                                               
VFRZ10   LR    R1,R3               RESTORE THE A(CURRENT TABLE ENTRY)           
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                  WI IS NOT ALLOWED TO CHANGE FREEZE           
VFRZ20   CLC   AGENCY,=C'WI'       'WI' AGENCY?                                 
         BNE   VFRZX               NO - SO JUST CONTINUE                        
*                                                                               
*                                  T217FFD+12 BIT X'04' ON, CHANGE OK           
         TM    T217FFD+12,X'04'    IS IT SET?                                   
         BO    VFRZX               YES - SO CONTINUE                            
*                                                                               
         MVC   HALF(1),COPT2       ELSE - STORE THE CURRENT BYTE                
         MVC   HALF+1(1),MYBYTE2   STORE THE ORIGINAL BYTE                      
         NI    HALF,COP2FRZ        TURN OFF EVERYTHING BUT FRZ BIT              
         NI    HALF+1,COP2FRZ      TURN OFF EVERYTHING BUT FRZ BIT              
         CLC   HALF(1),HALF+1      HAS BIT CHANGED?                             
         BE    VFRZX               NO - SO OK                                   
*                                                                               
         B     ERRFRZ              ELSE - ERROR                                 
*                                                                               
VFRZX    OC    DMCB(4),DMCB        DATE ENTERED?                                
         BNZ   *+10                YES - EXIT                                   
         XC    CLOCKYM,CLOCKYM     CLEAR FREEZE DATE WHEN N/Y ENTERED           
         B     XIT                                                              
*---------------------------------------------------------------------*         
*              VALXEST - VALIDATE THE CROSS ESTIMATE REPORTING FEATURE*         
*---------------------------------------------------------------------*         
* THIS CAN ONLY BE USED IF THE CLIENT IS A PROFIT W/IN CLIENT.                  
VALXEST  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
*                                                                               
         TM    COPT2,COP2XEST      CROSS EST SET?                               
         BZ    VXESX               NO - SO CONTINUE                             
*                                                                               
         OC    CPWPCT,CPWPCT       ANY PROFIT W/IN?                             
         BNZ   VXESX               YES - SO CONTINUE                            
*                                                                               
         B     ERRXEST             ELSE - ERROR                                 
*                                                                               
VXESX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        VALACS - VALIDATE THE LIMITED ACCESS CODE (A NEW ADDITION)   *         
*---------------------------------------------------------------------*         
VALACS   NTR1                                                                   
*                                                                               
         USING FIELDD,R2                                                        
         LA    R5,CACCESS                                                       
         XC    CACCESS,CACCESS                                                  
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VACSX               NO - SO JUST EXIT                            
*                                                                               
         XC    ELEM,ELEM           BUILD OFFICE CODES HERE                      
         LA    R3,ELEM                                                          
         USING SCANBLKD,R3                                                      
         GOTO1 SCANNER,DMCB,DATAH,(R3)                                          
         LLC   R4,DMCB+4           NUMBER OF ENTRIES                            
         LTR   R4,R4               ANY ENTRIES?                                 
         BZ    VERR                NO, ERROR                                    
         CHI   R4,3                MORE THAN 3 ENTRIES?                         
         BH    VERR                YES, ERROR                                   
*                                                                               
VACS10   CLI   SC1STLEN,1          HAVE 1 CHAR?                                 
         BE    *+8                 YES                                          
         CLI   SC1STLEN,2          HAVE 2 CHAR?                                 
         BNE   VERR                NO, INVALID FILTER VALUE ERROR               
*                                                                               
         CLI   SC1STFLD,C' '       CAN'T BE BLANK                               
         BE    VERR                INVALID FILTER VALUE ERROR                   
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCLMT,T217FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,SC1STFLD                                                 
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   ERROFC               YES                                         
         CLI   DMCB,0                                                           
         BNE   ERROFC                                                           
*                                                                               
         MVC   0(1,R5),OFCOFC      MOVE 1 CHAR OFFICE TO OFFICE LIST            
         LA    R5,1(R5)            BUMP OFFICE LIST IN CLIENT RECORD            
         LA    R3,SCBLKLQ(R3)      BUMP SCANNER BLOCK                           
         BCT   R4,VACS10           LOOP BACK AND PROCESS NEXT OFFICE            
*                                                                               
VACSX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALBP - VALIDATE THE BUY PROGRAMMING                   *         
*---------------------------------------------------------------------*         
VALBP    NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
VBPX     B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALBRD- VALIDATE IF BRD ESTIMATES ALLOWED              *         
*---------------------------------------------------------------------*         
VALBRD   NTR1                                                                   
         CLI   T217FFD+1,C'*'      DDS TERMINAL                                 
         BNE   XIT                                                              
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALSPCAL - VALIDATE THE SPECIAL FIELD (MIDAS AND P&G)  *         
*---------------------------------------------------------------------*         
VALSPCAL NTR1                                                                   
*                                                                               
         USING FIELDD,R2           FIELD USING DATA                             
         NI    COPT4,X'FF'-COP4PG-COP4MIDS                                      
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VSP10               NO                                           
         CLI   DATA,C'N'           NO OPTION?                                   
         BE    VSP10               YES                                          
         CLI   DATA,C'P'           P&G?                                         
         BNE   *+12                NO                                           
         OI    COPT4,COP4PG        TURN ON P&G                                  
         B     VSP10               GO TEST MIDAS                                
         CLI   DATA,C'M'           MIDAS?                                       
         BNE   VERR                NO - SO ERROR                                
         OI    COPT4,COP4MIDS      TURN ON MIDAS                                
*                                                                               
VSP10    TM    MYBYTE4,COP4MIDS    WAS CLIENT MIDAS?                            
         BZ    XIT                 NO - EXIT                                    
         TM    COPT4,COP4MIDS      ELSE - IS IT STILL MIDAS?                    
         BZ    ERRMIDAS            NO - ERROR                                   
         B     XIT                 EXIT                                         
         DROP  R2                                                               
*                                                                               
DISPSPCL NTR1                                                                   
         USING FIELDD,R2           FIELD DATA USING                             
         MVI   DATA,C'N'           DEFAULT TO NO OPTION                         
         TM    COPT4,COP4PG        P&G SET?                                     
         BZ    *+8                 NO                                           
         MVI   DATA,C'P'           YES - SET TO A P                             
         TM    COPT4,COP4MIDS      MIDAS SET?                                   
         BZ    *+8                 NO                                           
         MVI   DATA,C'M'           YES - SET TO AN N                            
         B     XIT                 EXIT                                         
         DROP  R2                  DROP USING                                   
*---------------------------------------------------------------------*         
*              VALYN - VALIDATE THE Y OR N INPUT                      *         
*---------------------------------------------------------------------*         
VALYN    NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         CLI   DATAH+5,1           INPUT 1 BYTE?                                
         BH    VERR                NO - SO ERROR                                
*                                                                               
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,(3)      R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         MVC   HALF(1),OPTEQU      SAVE THE OPTIONS MASK                        
         XI    HALF,X'FF'          TURN ON EVERYTHING BUT THE BIT               
         NC    BYTE,HALF           RE-SET THE BIT TO START                      
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VYN10               NO - SO TREAT AS 'N'O                        
         CLI   DATA,C'N'           ELSE - IS IT 'N'O?                           
         BE    VYN10               YES - SO CONTINUE                            
*                                                                               
         CLI   DATA,C'Y'           ELSE - IS IT 'Y'ES?                          
         BNE   VERR                NO - SO ERROR                                
         OC    BYTE,OPTEQU         ELSE - SET THE BIT                           
*                                                                               
VYN10    MVC   0(1,R4),BYTE        PUT RESULT BACK IN RECORD                    
VYNX     B     XIT                                                              
         DROP  R1,R2                                                            
*---------------------------------------------------------------------*         
*              VALCTAX - VALIDATE CANADIAN NETWORK TAX                *         
*---------------------------------------------------------------------*         
VALCTAX  NTR1                                                                   
         USING FIELDD,R2                                                        
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BE    *+12                YES - CONTINUE                               
*                                                                               
         CLI   DATA,C'0'           ELSE - CAN NET TAX MUST BE 0                 
         BNE   VERR                                                             
*                                                                               
         BAS   RE,VALANY                                                        
VCTAXX   B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              VALMKGD - VALIDATE MKGDS IN MISSED MONTHS              *         
*---------------------------------------------------------------------*         
VALMKGD  NTR1                                                                   
         USING FIELDD,R2                                                        
         CLI   SVAPROF+12,C'Y'     SEE IF MGD IN MISSED MTH ALLOWED             
         BE    *+12                YES - CONTINUE                               
*                                                                               
         CLI   DATA,C'N'           ELSE - MKFD MUST BE N                        
         BNE   VERR                                                             
*                                                                               
         BAS   RE,VALANY                                                        
VMKGDX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALUC - VALIDATE UCOM BILL CONTROL                     *         
*---------------------------------------------------------------------*         
VALUC    NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         CLI   DATAH+5,1           INPUT 1 BYTE?                                
         BH    VERR                NO - SO ERROR                                
*                                                                               
         MVI   UCOMNUM,0           init UCOM BILL CONTROL NUMBER                
*                                                                               
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,(3)      R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         MVC   HALF(1),OPTEQU      SAVE THE OPTIONS MASK                        
         XI    HALF,X'FF'          TURN ON EVERYTHING BUT THE BIT               
         NC    BYTE,HALF           RE-SET THE BIT TO START                      
*                                                                               
* VALID ENTRIES ARE 1-4 OR Y/N                                                  
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VALUC10             NO - SO TREAT AS 'N'O                        
         CLI   DATA,C'N'           ELSE - IS IT 'N'O?                           
         BE    VALUC10             YES - SO CONTINUE                            
*                                                                               
         CLI   DATA,C'Y'           ELSE - IS IT 'Y'ES?                          
         BE    VALUC05                                                          
*                                                                               
         CLI   DATA,C'1'                                                        
         BL    VERR                                                             
         CLI   DATA,C'4'                                                        
         BH    VERR                                                             
*                                                                               
         PACK  DUB,DATA(1)         NUMBER OF UCOMMS                             
         CVB   R0,DUB                                                           
         STC   R0,UCOMNUM          SAVE UCOM BILL CONTROL NUMBER                
*                                                                               
VALUC05  OC    BYTE,OPTEQU         ELSE - SET THE BIT                           
*                                                                               
VALUC10  MVC   0(1,R4),BYTE        PUT RESULT BACK IN RECORD                    
         B     XIT                                                              
         DROP  R1,R2                                                            
*---------------------------------------------------------------------*         
*              VALANY - VALIDATE BOX,BILF,BILE,CTAX,CAMP,MKGD         *         
*---------------------------------------------------------------------*         
VALANY   NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         CLI   DATAH+5,1           INPUT 1 BYTE?                                
         BH    VERR                NO - SO ERROR                                
*                                                                               
         L     RF,AVLST            DISP OF VAL LIST                             
         A     RF,RELO             RF = A(VAL LIST)                             
*                                                                               
VANY10   CLI   0(RF),0                                                          
         BE    VERR                                                             
*                                                                               
         CLC   DATA(1),0(RF)                                                    
         BE    VANY20                                                           
         LA    RF,1(RF)                                                         
         B     VANY10                                                           
*                                                                               
VANY20   LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,RECDIS,2         R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   0(1,R4),DATA        MOVE IN WHATEVER INTO REC                    
*                                                                               
VANYX    B     XIT                                                              
         DROP  R1,R2                                                            
*---------------------------------------------------------------------*         
*              VRFP - VALIDATE T/A GROUP RFP ID                       *         
*---------------------------------------------------------------------*         
VRFP     NTR1                                                                   
         USING FIELDD,R2                                                        
         MVC   CRFPGRP,SPACES                                                   
         CLI   DATAH+5,0           NO INPUT? THAT'S FINE                        
         BE    VRFPX                                                            
*                                                                               
         OC    SVRFPID,SVRFPID                                                  
         BNZ   VRFP05                                                           
         LA    R2,DATAH                                                         
         B     ERRARFP             NEED RFP ID ON AGY REC                       
*                                                                               
VRFP05   L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SVSYS,FASYS         CONNECTED SYSTEM                             
         DROP  R1                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'                                                       
         GOTO1 (RF),DMCB                                                        
*                                                                               
         LA    R3,MYCTKEY                                                       
         USING CT$GRPKEYD,R3                                                    
*                                                                               
         XC    MYCTKEY,MYCTKEY                                                  
         MVC   MYCTKEY(2),=X'002F'                                              
         MVI   CT$GRPKSYST,C'S'                                                 
         MVC   CT$GRPKAGY,TWAAGY                                                
         MVC   CT$GRPKUSER,SVRFPID      USE RFP ID FROM AGY REC                 
***NOP   MVC   CT$GRPKUSER,TWAORIG                                              
         MVC   CT$GRPKGRP,DATA                                                  
         OC    CT$GRPKGRP,SPACES                                                
         MVC   KEYSAVE,MYCTKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',MYCTKEY,MYCTKEY               
         CLC   KEYSAVE(13),MYCTKEY                                              
         BNE   VERR                                                             
         MVC   CRFPGRP,DATA        MOVE IN WHATEVER INTO REC                    
         OC    CRFPGRP,SPACES                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVSYS                                                    
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 (RF),DMCB,,0                                                     
*                                                                               
VRFPX    B     XIT                                                              
         DROP  R2,R3                                                            
*---------------------------------------------------------------------*         
*              VALCTYP - VALIDATE TEST CLIENT TYPE                    *         
*---------------------------------------------------------------------*         
VALCTYP  NTR1                                                                   
         USING FIELDD,R2                                                        
         MVI   CCLTTYPE,0                                                       
         CLI   DATAH+5,0           NO INPUT? THAT'S FINE                        
         BE    *+10                                                             
         MVC   CCLTTYPE,DATA                                                    
         B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
         USING FIELDD,R2                                                        
VERR     LA    R2,DATAH            INVALID DATA                                 
         B     ERRINV                                                           
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
*              VALC2CD - VALIDATE COST2 CONVERSION DATE               *         
*---------------------------------------------------------------------*         
VALC2CD  NTR1                                                                   
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------*         
*              VALCPPR - VALIDATE CPPRS OPTION                        *         
*---------------------------------------------------------------------*         
VALCPPR  NTR1                                                                   
         USING FIELDD,R2                                                        
*                                                                               
         CLI   DATAH+5,0                                                        
         BE    ERRMIS                                                           
         CLI   DATAH+5,1                                                        
         BNE   ERRINV                                                           
*                                                                               
         CLI   DATA,C'Y'                                                        
         BNE   VALCP10                                                          
         MVI   CCPPRS,C'Y'                                                      
         B     XIT                                                              
*                                                                               
VALCP10  DS    0H                                                               
         CLI   DATA,C'N'                                                        
         BNE   ERRINV                                                           
         MVI   CCPPRS,C'N'                                                      
*                                                                               
         DROP  R2                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
*  CLEAR FIELDS BEFORE DISPLAY                                                  
*                                                                               
DR       MVC   CL2CEXN,SPACES      CLEAR EXTENDED CLIENT NAME FIELD             
         OI    CL2CEXNH+6,X'80'    TRANSMIT                                     
                                                                                
         LA    R2,CL2FRSTH         A(1ST SCREEN FIELD TO CLEAR)                 
         LA    R3,CL2ENDH          R3 = A(LAST FLD TO CLEAR)                    
*                                                                               
DR10     LLC   RE,0(R2)            GET L(SCREEN FIELD)                          
         SHI   RE,9                L(DATA)                                      
         EX    RE,*+8              EXECUTE THE CLEAR                            
         B     *+10                                                             
         MVC   8(0,R2),SPACES      CLEAR THE FIELD                              
         OI    1(R2),X'20'         PROTECT THE FIELD                            
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                  RE = L(SCREEN FIELD)-9                       
         LA    R2,9(RE,R2)         INC TO NEXT FIELD HEADER                     
         CR    R2,R3               PAST LAST FIELD TO CLEAR?                    
         BNH   DR10                NO - SO LOOP BACK UP                         
*                                                                               
         L     R6,AIO                                                           
         MVC   CL2CLTN,CNAME       MOVE CLT NAME TO SCREEN                      
         OI    CL2CLTNH+6,X'80'    TRANSMIT THE FIELD                           
*                                                                               
         MVC   CL2CEXN,CEXTNAME    MOVE EXTENDED CLIENT NAME TO SCREEN          
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
* GET THE CLIENT PROFILES                                                       
*                                                                               
         MVC   KEY(4),=C'S0C2'     OFFLINE SPOT PROGRAM NUMBER                  
         MVC   KEY+4(2),AGENCY     AGENCY CODE                                  
         MVC   KEY+6(6),SPACES     NO MED, CLIENT, OFFICE                       
         L     RF,ACOMFACS         SET UP FOR GETPROF CALL                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',KEY),PROFILE,DATAMGR                            
         CLC   PROFILE(16),SPACES  DID WE GET ANYTHING?                         
         BH    *+6                 YES - SO CONTINUE                            
         DC    H'0'                ELSE - HUH?                                  
*                                                                               
         MVC   KEY(4),=C'S0C3'     OFFLINE SPOT PROGRAM NUMBER                  
         MVC   KEY+4(2),AGENCY     AGENCY CODE                                  
         MVC   KEY+6(6),SPACES     NO MED, CLIENT, OFFICE                       
         GOTO1 (RF),DMCB,(X'C0',KEY),PROFILE+16,DATAMGR                         
         CLC   PROFILE+16(16),SPACES       DID WE GET ANYTHING?                 
         BH    *+6                 YES - SO CONTINUE                            
         DC    H'0'                ELSE - HUH?                                  
*                                                                               
         MVC   KEY(4),=C'S0C4'     OFFLINE SPOT PROGRAM NUMBER                  
         MVC   KEY+4(2),AGENCY     AGENCY CODE                                  
         MVC   KEY+6(6),SPACES     NO MED, CLIENT, OFFICE                       
         GOTO1 (RF),DMCB,(X'C0',KEY),PROFILE+32,DATAMGR                         
         CLC   PROFILE+32(16),SPACES       DID WE GET ANYTHING?                 
         BH    *+6                 YES - SO CONTINUE                            
         DC    H'0'                ELSE - HUH?                                  
*                                                                               
* NOW SET UP ADDRESSES AND USINGS FOR THE MAIN LOOP                             
*                                                                               
         LA    R1,TABLE1           A(TABLE OF WHAT GOES ON THE SCREEN)          
         USING TABLE1D,R1                                                       
         LA    R2,CL2FRSTH         A(FIRST SCREEN LITERAL FIELD)                
         USING FIELDD,R2                                                        
*                                                                               
DR20     CLI   PROF,X'FF'          AT END OF TABLE?                             
         BE    DR40                YES - SO ALL DONE                            
*                                                                               
         LA    R3,PROFILE          A(PROFILE RECORD RESPONSES)                  
         LLC   R4,PROF             R4 = DISP INTO PROFILE RESPONSES             
         AR    R3,R4               A(THIS PROFILE REPONSE)                      
         MVC   BYTE,0(R3)          PUT IT IN BYTE                               
         CLI   BYTE,C'Y'           CLIENT USING THIS FEATURE?                   
         BNE   DR30                NO - SO CONTINUE                             
*                                                                               
         MVC   SCRNLIT(21),LITERAL MOVE LITERAL TO SCREEN                       
         MVI   NOPROTFL,C'N'                                                    
         ICM   RF,15,DISPRTN       RF = A(DISPLAY ROUTINE)                      
         A     RF,RELO             RELOCATE IT                                  
         BASR  RE,RF               AND GO                                       
         CLI   NOPROTFL,C'Y'                                                    
         BE    *+8                                                              
         NI    DATAH+1,X'FF'-X'20' UNPROTEST THE DISPLAYED DATA FIELD           
*                                                                               
         LA    R2,LFIELDD(R2)      INC TO NEXT SCREEN FIELD                     
*                                                                               
DR30     LA    R1,LTABLE1D(R1)     INC TO NEXT TABLE ENTRY                      
         B     DR20                LOOP BACK TO CHECK IT                        
*                                                                               
DR40     MVI   DISFLAG,0           RESET DIS FROM LIST FLAG                     
         CLI   SELSTFLG,1          SEL FROM LIST?                               
         BE    *+12                YES                                          
         MVI   RETFLAG,0           RESET RETURN FLAG                            
         B     DRX                 EXIT                                         
*                                                                               
         MVI   DISFLAG,1           SET DIS FROM LIST FLAG ON                    
         CLI   ACTNUM,ACTCHA       CHANGE ACTION                                
         BNE   *+12                NO - SET RETURN FLAG ON                      
*                                  PUT CUR AT 1ST DATA FLD WHEN CHANGE          
         OI    CL2FRSTH+6+L'SCRNLITH+L'SCRNLIT,X'C0'                            
         B     DRX                                                              
*                                                                               
         MVI   RETFLAG,1           SET RETURN TO LIST FLAG ON                   
*                                                                               
DRX      B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
*              DISPLAY ROUTINES                                       *         
***********************************************************************         
*                                  FOR ALL DISPLAY ROUTINES,                    
*                                  INPUT:  R1=A(CURRENT TABLE ENTRY)            
*                                  INPUT:  R2=A(TARGET SCR FLD GROUP)           
*                                  OUTPUT: MVC SOMETHING TO TARGET FLD          
*                                                                               
*---------------------------------------------------------------------*         
*              DISPUC - DISPLAY UCOMM VALUE TO THE SCREEN             *         
*---------------------------------------------------------------------*         
DISPUC   NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         MVI   DATA,C'N'           MOVE OUT DEFAULT                             
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,2        R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         NC    BYTE,OPTEQU         BIT SET?                                     
         BZ    DISPUX              NO - SO CONTINUE                             
*                                                                               
         MVI   DATA,C'Y'           ELSE - PRESET TO 'Y'                         
         LA    RE,CLTHDR                 (CLIENT RECORD)                        
         LA    RF,UCOMNUM-CLTHDR                                                
         AR    RE,RF                                                            
         OC    0(L'UCOMNUM,RE),0(RE) ANY UCOM NUMBER?                           
         BZ    DISPUX              NO, MUST BE Y                                
*                                                                               
         LLC   R0,0(RE)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DATA(L'UCOMNUM),DUB   MOVE UCOMM NUMBER                          
*                                                                               
DISPUX   B     XIT                 AND RETURN TO CALLER                         
         DROP  R1,R2                                                            
*                                                                               
*---------------------------------------------------------------------*         
*              DISPYN - DISPLAY Y/N TO THE SCREEN                     *         
*---------------------------------------------------------------------*         
DISPYN   NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         MVI   DATA,C'N'           MOVE OUT DEFAULT                             
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,2        R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         NC    BYTE,OPTEQU         BIT SET?                                     
         BZ    *+8                 NO - SO CONTINUE                             
         MVI   DATA,C'Y'           ELSE - MOVE OUT 'Y'                          
*                                                                               
DYNX     B     XIT                 AND RETURN TO CALLER                         
         DROP  R1,R2                                                            
*---------------------------------------------------------------------*         
*              DISPCOS2 - DISPLAY COST FACTOR OR Y/N BASED ON SVAGYFL1*         
*---------------------------------------------------------------------*         
DISPCOS2 NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    SVAGYFL1,AGYCOS2Q   COST FACTOR?                                 
         BO    DCOS10              YES - SO CONTINUE                            
*                                                                               
         TM    COPT4,COP4TRD       TRADE?                                       
         BNO   DCOS08              NO                                           
         MVI   DATA,C'T'                                                        
         B     DCOSX                                                            
*                                                                               
DCOS08   TM    SVAGYFL1,AGYCOS2Q   COST2 FACTOR REQUIRED?                       
         BO    DCOS10                CARAT/UB ONLY ALLOWED FOR TRADE            
         TM    COPT3,COP3COSQ      COS2 OPTIONAL                                
         BNO   DCOS09                                                           
         MVI   DATA,C'O'                                                        
         B     DCOSX                                                            
*                                                                               
DCOS09   BAS   RE,DISPYN           ELSE - SHOW Y/N                              
         B     DCOSX               AND EXIT                                     
*                                                                               
DCOS10   OC    CCOST2,CCOST2       ANY COST FACTOR?                             
         BNZ   DCOS12              NO - SO ALL DONE                             
*                                                                               
* INITIATIVE CAN HAVE A 'T' IN THE COS2 FIELD EVEN IF AGY REC SAYS              
* COS2 REQUIRED (EJOR 6/20/07)                                                  
* ADDED CARAT (HWON 10/12/12)                                                   
* ADDED HAVAS (HWON 09/06/13)                                                   
         CLC   AGENCY,=C'WI'       INITIATIVE?                                  
         BE    DCOS11                                                           
         CLC   AGENCY,=C'WJ'       INITIATIVE? (WITEST)                         
         BE    DCOS11                                                           
*                                  YES                                          
         CLC   AGENCY,=C'UB'       IS THIS CARAT/UB?                            
         BE    DCOS11               YES                                         
         CLC   AGENCY,=C'PH'       TEST AGENCY PHNY?                            
         BE    DCOS11               NO                                          
         CLC   AGENCY,=C'FM'       IS THIS HAVAS/FM?                            
         BNE   DCOSX                YES                                         
*                                  YES                                          
DCOS11   TM    COPT4,COP4TRD       TRADE?                                       
         BNO   DCOSX               NO                                           
         MVI   DATA,C'T'                                                        
         B     DCOSX               NO                                           
*                                                                               
DCOS12   CLI   CCOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   *+14                NO - SO CONTINUE                             
         MVC   DATA(3),=C'0.0'     ELSE - MOVE OUT ZERO                         
         B     DCOSX               AND CONTINUE                                 
*                                                                               
         EDIT  CCOST2,(8,DATA),6,ALIGN=LEFT,ZERO=NOBLANK,FILL=0,DROP=5          
*                                                                               
DCOSX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPTRAD - DISPLAY THE TRADE CLIENT                    *         
*---------------------------------------------------------------------*         
DISPTRAD NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    COPT2,COP2DIY       DIY TRADE CLIENT?                            
         BNZ   *+12                YES - SO CONTINUE                            
         BAS   RE,DISPYN           ELSE - SHOW Y/N                              
         B     DTRDX               AND EXIT                                     
*                                                                               
         MVC   DATA(3),=C'DIY'                                                  
*                                                                               
DTRDX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPFRZ - DISPLAY THE FREEZE FEATURE BASED ON CLOCKYM  *         
*---------------------------------------------------------------------*         
DISPFRZ  NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CLOCKYM,CLOCKYM     FREEZE DATE?                                 
         BNZ   *+12                YES - SO CONTINUE                            
         BAS   RE,DISPYN           ELSE - SHOW Y/N                              
         B     DFRZX               AND EXIT                                     
*                                                                               
         MVC   WORK(2),CLOCKYM                                                  
         MVI   WORK+2,X'01'                                                     
         NI    WORK+1,X'FF'-X'80'-X'40'                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,DATA)                                    
         TM    CLOCKMON,X'80'      LOCK EVERYTHING PRIOR TO DATE                
         BZ    *+8                                                              
         MVI   DATA+6,C'-'                                                      
         TM    CLOCKMON,X'40'      LOCK EVERYTHING AFTER DATE                   
         BZ    *+8                                                              
         MVI   DATA+6,C'+'                                                      
*                                                                               
DFRZX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPWPCT - DISPLAY THE PROFIT WITHIN % TO THE SCREEN   *         
*---------------------------------------------------------------------*         
DISPWPCT NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CPWPCT,CPWPCT       ANY PROFIT WITHIN?                           
         BZ    DPCTX               NO - EXIT                                    
*                                                                               
         CLC   CPWPCT,=X'800000'   ELSE - CHECK IF ZERO %                       
         BNE   *+12                NO - SO CONTINUE                             
         MVI   DATA,C'0'           ELSE - MOVE OUT 0                            
         B     DPCTX               AND EXIT                                     
*                                                                               
         ZICM  R3,CPWPCT,3                                                      
         EDIT  (R3),(6,DATA),2,ALIGN=LEFT                                       
*                                                                               
DPCTX    B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              DISPZEN - DISPLAY THE ZENITH CLIENT CODE TO THE SCREEN *         
*              ** NOW THE 'EXTRACT' CLIENT CODE                       *         
*              (IT'S NOT JUST FOR ZENITH ANYMORE)                     *         
*---------------------------------------------------------------------*         
DISPZEN  NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CZENCLT,CZENCLT     ANY ZENITH CLIENT TO DISPLAY?                
         BZ    DZENX               NO - EXIT                                    
*                                                                               
         LA    RE,1                ELSE - SET LENGTH FOR 2 CHARS                
         CLI   CZENCLT+2,X'00'     TEST IF IT'S 2 OR 3 CHARACTERS               
         BE    *+8                 YES - SO CONTINUE                            
         LA    RE,2                ELSE - SET LENGTH FOR 3 CHARS                
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DATA(0),CZENCLT     EX MOVE OUT THE ZENITH CLIENT CODE           
*                                                                               
DZENX    B     XIT                                                              
         DROP  R2                                                               
*---------------------------------------------------------------------*         
*              DISANY - DISPLAY WHATEVER IN REC TO THE SCREEN         *         
*---------------------------------------------------------------------*         
DISPANY  NTR1                                                                   
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,RECDIS,2         R3 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   DATA(1),0(R4)       MOVE OUT WHATEVER IN REC                     
*                                                                               
DANYX    B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPMKGD - DISPLAY MKGDS IN MISSED MTH.(Y/N) TO SCREEN*          
*---------------------------------------------------------------------*         
DISPMKGD NTR1                                                                   
         USING FIELDD,R2                                                        
         MVI   DATA,C'N'           MOVE OUT DEFAULT                             
         CLI   CEXTRA+7,C'Y'       CHK IF MKGDS = Y                             
         BNE   DMKGDX              NO - EXIT                                    
         MVI   DATA,C'Y'           ELSE - MOVE OUT 'Y'                          
*                                                                               
DMKGDX   B     XIT                 AND RETURN TO CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPACS - DISPLAY LIMITED ACCESS CODE*                           
*---------------------------------------------------------------------*         
DISPACS  NTR1                                                                   
         USING FIELDD,R2                                                        
         LA    R3,CACCESS          OFFICE LIST                                  
         LA    R4,3                MAX NUMBER OF OFFICE CODES                   
         LA    R5,DATA             MOVE 2 BYTE OFFICE CODES HERE                
*                                                                               
DACS05   CLI   0(R3),0             END OF OFFICE LIST?                          
         BE    DISACSX             YES, EXIT                                    
*                                                                               
         CHI   R4,3                FIRST TIME IN?                               
         BE    DACS10              YES                                          
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)            BUMP PAST OFFICE CODE                        
*                                                                               
DACS10   MVC   0(2,R5),=C'??'                                                   
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,0(R3)                                                     
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS)                     
         CLI   0(R1),0                                                          
         BNE   DISACSX                                                          
*                                                                               
         MVC   0(2,R5),OFCOFC2     MOVE IN THE CHARACTER OFFICE CODE            
         LA    R5,1(R5)            BUMP BY 1                                    
         CLI   0(R5),C' '          HAVE 2 CHARACTER OFFICE CODE?                
         BNH   *+8                 NO                                           
         LA    R5,1(R5)            BUMP PAST OFFICE CODE                        
         LA    R3,1(R3)            BUMP OFFICE LIST                             
*                                                                               
         BCT   R4,DACS05                                                        
*                                                                               
DISACSX  B     XIT                 AND RETURN TO CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPRFP - DISPLAY T/A GROUP RFP ID                               
*---------------------------------------------------------------------*         
DISPRFP  NTR1                                                                   
*                                                                               
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         MVC   DATA(8),CRFPGRP     MOVE OUT WHATEVER IN REC                     
*                                                                               
DISRFPX  B     XIT                                                              
         DROP  R1,R2                                                            
*---------------------------------------------------------------------*         
*              DISCTYP - DISPLAY TEST CLIENT TYPE                     *         
*---------------------------------------------------------------------*         
DISCTYP  NTR1                                                                   
         USING FIELDD,R2                                                        
         MVC   DATA(1),CCLTTYPE                                                 
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
*             DISPC2CD - DISPLAY  COST2 CONVERSION DATE               *         
*---------------------------------------------------------------------*         
DISPC2CD NTR1                                                                   
*                                                                               
         USING FIELDD,R2                                                        
*                                                                               
         OI    DATAH+1,X'20'       PROTECT THE FIELD                            
         MVI   NOPROTFL,C'Y'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,CC2CONV),(11,DATA)                                
*                                                                               
         TM    COPT3,COP3CONV                                                   
         BNO   DIS2XIT                                                          
         MVI   DATA+5,C'*'                                                      
*                                                                               
DIS2XIT  DS    0H                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
*---------------------------------------------------------------------*         
*             DISPCPPR - DISPLAY  CPPRS OPTION                        *         
*---------------------------------------------------------------------*         
DISPCPPR NTR1                                                                   
*                                                                               
         USING FIELDD,R2                                                        
         MVI   DATA,C'Y'                                                        
         CLI   CCPPRS,C'N'                                                      
         BNE   *+8                                                              
         MVI   DATA,C'N'                                                        
         OI    DATAH+6,X'80'                                                    
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*                       ERROR MESSAGES                                *         
***********************************************************************         
*                                                                               
ERRARFP  MVC   ERRNUM,=AL2(NOAGYRFP)                                            
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     ERRFLD                                                           
ERRINFO  MVC   ERRNUM,=AL2(NCHAINFO)                                            
         B     ERRFLD                                                           
ERRGMI   MVC   ERRNUM,=AL2(NCHAGMI)                                             
         B     ERRFLD                                                           
ERRTRD   MVC   ERRNUM,=AL2(NCHATRD)                                             
         B     ERRFLD                                                           
ERRFRZ   MVC   ERRNUM,=AL2(NCHAFRZ)                                             
         B     ERRFLD                                                           
ERRCFAC  MVC   ERRNUM,=AL2(COSFACT)         COST FACTOR REQD                    
         B     ERRFLD                                                           
ERROFC   MVC   ERRNUM,=AL2(544)    INVALID OFFICE                               
         B     ERRFLD                                                           
ERRXEST  MVC   ERRNUM,=AL2(ESTWOPRO)                                            
         B     ERRFLD                                                           
ERRMIDAS MVC   ERRNUM,=AL2(1379)                                                
         B     ERRFLD                                                           
         USING FIELDD,R2                                                        
ERRFLD   LA    R2,DATAH            INVALID DATA FIELD                           
         B     SPERREX                                                          
         DROP  R2                                                               
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         MVI   RETFLAG,0           RESET RETURN FLAG FOR ALL ERRORS             
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR ACTION                    
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
NCHAINFO EQU   1113                CANNOT UNDO INFOMERCIAL                      
NCHAGMI  EQU   556                 CANNOT UNDO GMI                              
NCHATRD  EQU   1112                CANNOT UNDO TRADE                            
NCHAFRZ  EQU   557                 CANNOT UNDO FREEZE                           
COSFACT  EQU   819                 COST FACTOR REQD                             
ESTWOPRO EQU   558                 CROSS ESTIMATE W/O PROFIT WITHIN             
NOAGYRFP EQU   809                 NEED RFP ID ON AGY REC                       
         EJECT                                                                  
***********************************************************************         
*                       SETUP                                         *         
***********************************************************************         
SETUP    NTR1                                                                   
*                                                                               
         LA    R2,CONACTH          ACTION FIELD                                 
         CLI   T217FFD+1,C'*'      DDS TERMINAL                                 
         BE    SETUP01                                                          
         TM    T217FFD+12,X'40'                                                 
         BNO   SETUP01                                                          
         CLI   ACTNUM,ACTCHA                                                    
         BE    ERRSEC2                                                          
*                                                                               
SETUP01  OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         MVI   IOOPT,C'Y'          DON'T LET GENCON DO PUTREC                   
*                                                                               
         MVI   SELSTFLG,0          RESET SEL FROM LIST FLAG                     
         CLI   CALLSTCK,X'77'      PFKEY CALL FROM LIST SCR(CALLSTCK)           
         BNE   SETUP10             NO - LEAVE SEL FROM LIST FLAG OFF            
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    SETUP10             NO - LEAVE SEL FROM LIST FLAG OFF            
         MVI   SELSTFLG,1          SET SEL FROM LIST FLAG ON                    
*                                                                               
* GENCON PROHIBITES THE CURSOR POSITION ON ACT FLD WHEN DISPLAY                 
*        CLI   ACTNUM,ACTDIS       ACTION DISPLAY?                              
*        BNE   *+8                 NO                                           
*        OI    CONACTH+6,X'C0'     PLACE CURSOR AT ACT FLD WHEN DISPLAY         
*                                                                               
         CLI   DISFLAG,1           SCREEN WAS DISPLAYED, PF FROM LIST           
         BNE   SETUP10             NO                                           
         TM    CONACTH+4,X'80'     ANY CHANGE ON ACTION FIELD                   
         BZ    SETUP10             NO                                           
*                                                                               
         MVI   CALLSP,0            DON'T RETURN TO ANYWHERE                     
*        MVI   1(R3),0             CALLSP = 0                                   
         MVI   RETFLAG,0           RESET RETURN FLAG                            
*                                                                               
SETUP10  OI    CL2REH+1,X'0C'      HIDE PF12=RETURN FIELD                       
*        CLI   0(R3),X'77'         PFKEY CALL FROM LIST SCR(CALLSTCK)           
         CLI   CALLSTCK,X'77'      PFKEY CALL FROM LIST SCR(CALLSTCK)           
         BE    SETUP15                                                          
*                                                                               
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
*        CLI   1(R3),0             CALLSP==0?                                   
         BE    *+8                 NO                                           
         NI    CL2REH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
SETUP15  OI    CL2REH+6,X'80'      TRANSMIT THE RESULT                          
*                                                                               
         XC    CL2PFKY+52(12),CL2PFKY+52  CLEAR PF6=CLT/LIST FIELD              
         OI    CL2PFKYH+6,X'80'     TRANSMIT THE RESULT                         
         CLI   CALLSTCK,X'77'         CALLSTCK=X'77', FROM CLT/LIST             
*        CLI   0(R3),X'77'         CALLSTCK=X'77', FROM CLT/LIST                
         BE    *+14                YES                                          
         MVC   CL2PFKY+52(12),=CL12'PF6=Clt/List'  PF6=CLT/LIST FIELD           
         B     SETUP17                                                          
*                                                                               
         CLI   PFKEY,6                                                          
         BNE   *+12                                                             
         MVI   PFKEY,X'FF'         PF6 IS INVALID IN THIS CASE                  
         B     SETUP20                                                          
*                                                                               
SETUP17  CLI   PFKEY,12                                                         
         BNE   SETUP18                                                          
         TM    CL2REH+1,X'0C'      PF12 IS ALLOWED?                             
         BNO   *+12                YES                                          
         MVI   PFKEY,X'FF'         INVALID PF12 IN THIS CASE                    
         B     SETUP20                                                          
*                                                                               
SETUP18  OC    PFKEY,PFKEY                                                      
         BNZ   SETUP20                                                          
         CLI   RETFLAG,1           READY TO RETURN TO LIST                      
         BNE   SETUPX              NO                                           
         CLI   SELSTFLG,1          SEL FROM LIST?                               
         BNE   SETUPX              NO                                           
         MVI   RETFLAG,0           RESET RETURN FLAG                            
         MVI   PFKEY,12            RETURN TO LIST                               
*                                                                               
SETUP20  GOTO1 INITPFKY,DMCB,PFTABLE     MAINT PF TABLE                         
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LTORG                                         *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       CONSTANTS                                     *         
***********************************************************************         
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
BILFVLST DC    C'0123459',X'00'                                                 
BILEVLST DC    C'012',X'00'                                                     
CTAXVLST DC    C'01',X'00'                                                      
CAMPVLST DC    C'0E',X'00'                                                      
MKGDVLST DC    C'NY',X'00'                                                      
         EJECT                                                                  
***********************************************************************         
*                       TABLES                                        *         
***********************************************************************         
TABLE1   EQU   *                                                                
         DC    AL1(PROFBOX-PROFILED),CL21'Lock Box Number'                      
         DC    AL4(VALANY,DISPANY),AL2(CPROF+1-CLTHDR),AL4(ALPHANUM)            
*                                                                               
         DC    AL1(PROFBILF-PROFILED),CL21'Bill Formula Control'                
         DC    AL4(VALANY,DISPANY),AL2(CPROF+4-CLTHDR),AL4(BILFVLST)            
*                                                                               
         DC    AL1(PROFBILE-PROFILED),CL21'Bill Estimate Control'               
         DC    AL4(VALANY,DISPANY),AL2(CPROF+5-CLTHDR),AL4(BILEVLST)            
*                                                                               
         DC    AL1(PROFCTAX-PROFILED),CL21'Canadian Network Tax'                
         DC    AL4(VALCTAX,DISPANY),AL2(CEXTRA+1-CLTHDR),AL4(CTAXVLST)          
*                                                                               
         DC    AL1(PROFCAMP-PROFILED),CL21'Campaigns'                           
         DC    AL4(VALANY,DISPANY),AL2(CEXTRA+4-CLTHDR),AL4(CAMPVLST)           
*                                                                               
         DC    AL1(PROFMKGD-PROFILED),CL21'MKGDS in Missed Month'               
         DC    AL4(VALMKGD,DISPMKGD),AL2(CEXTRA+7-CLTHDR),AL4(MKGDVLST)         
*                                                                               
         DC    AL1(PROFCOS2-PROFILED),CL21'2nd Cost Required?'                  
         DC    AL4(VALCOS2,DISPCOS2),AL2(COPT1-CLTHDR),AL1(COP1COSQ)            
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFDBL-PROFILED),CL21'Supp DBL Book Test?'                  
         DC    AL4(VALDBL,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1DBLQ)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFINFO-PROFILED),CL21'Infomercial?'                        
         DC    AL4(VALINFO,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1INFQ)              
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFMGR-PROFILED),CL21'Market Groups?'                       
         DC    AL4(VALINFO,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1MGRQ)              
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFXJ1-PROFILED),CL21'Exclude from J1 RPT?'                 
         DC    AL4(VALXJ1,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2EXJ1)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFXA7-PROFILED),CL21'Exclude from A7 RPT?'                 
         DC    AL4(VALXA7,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2EXA7)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFCTA-PROFILED),CL21'CTA Client?'                          
         DC    AL4(VALCTA,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1CTAQ)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFGMI-PROFILED),CL21'GMI Client?'                          
         DC    AL4(VALGMI,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1GMI)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFUPL-PROFILED),CL21'Upload Deleted Buys?'                 
         DC    AL4(VALUPL,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1UPLQ)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFZEN-PROFILED),CL21'Extract Client Code'                  
         DC    AL4(VALZEN,DISPZEN),XL2'0',XL1'0'                                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFNPWB-PROFILED),CL21'Disallow PW Billing?'                
         DC    AL4(VALNPWB,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2NPWB)              
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFPWP-PROFILED),CL21'PW %'                                 
         DC    AL4(VALPWPCT,DISPWPCT),XL2'0',XL1'0'                             
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFTRAD-PROFILED),CL21'Trade Client?'                       
         DC    AL4(VALTRADE,DISPTRAD),AL2(COPT2-CLTHDR),AL1(COP2TRAD)           
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFFRZ-PROFILED),CL21'Freeze Client?'                       
         DC    AL4(VALFRZ,DISPFRZ),AL2(COPT2-CLTHDR),AL1(COP2FRZ)               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFXEST-PROFILED),CL21'Cross Est Reporting?'                
         DC    AL4(VALXEST,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2XEST)              
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFACS-PROFILED),CL21'Limited Access Code'                  
         DC    AL4(VALACS,DISPACS),XL2'0',XL1'0'                                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFBP-PROFILED),CL21'Buy Programming'                       
         DC    AL4(VALBP,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2BP)                  
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFRFP-PROFILED),CL21'T/A RFP Group'                        
         DC    AL4(VRFP,DISPRFP),XL2'0',XL1'0'                                  
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFCTY-PROFILED),CL21'TEST CLT TYPE'                        
         DC    AL4(VALCTYP,DISCTYP),XL2'0',XL1'0'                               
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFSEC-PROFILED),CL21'Prod. Level Security'                 
         DC    AL4(VALYN,DISPYN),AL2(COPT3-CLTHDR),AL1(COP3PSEC)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFC2CD-PROFILED),CL21'Cost2 Conversion Date'               
         DC    AL4(VALC2CD,DISPC2CD),AL2(COPT3-CLTHDR),AL1(COP3CONV)            
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFCPPR-PROFILED),CL21'CPPRS'                               
         DC    AL4(VALCPPR,DISPCPPR),XL2'0',XL1'0'                              
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFBRD-PROFILED),CL21'Allow BRD Estimates'                  
         DC    AL4(VALBRD,DISPYN),AL2(COPT3-CLTHDR),AL1(COP3BRD)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFSPOD-PROFILED),CL21'SPODS Allowed'                       
         DC    AL4(VALYN,DISPYN),AL2(COPT3-CLTHDR),AL1(COP3SPOD)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFUCOM-PROFILED),CL21'UComm Bill Control'                  
         DC    AL4(VALUC,DISPUC),AL2(COPT4-CLTHDR),AL1(COP4UCOM)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFTBEX-PROFILED),CL21'Exclude from Timebank'               
         DC    AL4(VALYN,DISPYN),AL2(COPT4-CLTHDR),AL1(COP4TBEX)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFBPCT-PROFILED),CL21'Bill PCT Split'                      
         DC    AL4(VALYN,DISPYN),AL2(COPT4-CLTHDR),AL1(COP4BPCT)                
         DC    XL3'0'                                                           
*                                                                               
         DC    AL1(PROFMDAS-PROFILED),CL21'Special Options'                     
         DC    AL4(VALSPCAL,DISPSPCL),XL2'0',XL1'0'                             
         DC    XL3'0'                                                           
*                                                                               
         DC    X'FF'               E.O.T. FLAG                                  
         EJECT                                                                  
*                                                                               
PFTABLE DS    0H                                                                
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                    
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF02     DC   AL1(KEYTYTWA,L'CL2MED-1),AL2(CL2MED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CL2CLT-1),AL2(CL2CLT-T217FFD)                      
PF02X    EQU  *                                                                 
*                                                                               
*        PRODUCT LIST                                                           
         DC   AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                    
         DC   CL3'PL '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
PF03     DC   AL1(KEYTYTWA,L'CL2MED-1),AL2(CL2MED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CL2CLT-1),AL2(CL2CLT-T217FFD)                      
PF03X    EQU  *                                                                 
*                                                                               
*        CLIENT  MAINT DISPLAY                                                  
         DC   AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                    
         DC   CL3'C1 '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF04     DC   AL1(KEYTYTWA,L'CL2MED-1),AL2(CL2MED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CL2CLT-1),AL2(CL2CLT-T217FFD)                      
PF04X    EQU  *                                                                 
*                                                                               
*        CLIENT LIST                                                            
         DC   AL1(PF06X-*,06,PFTCPROG,(PF06X-PF06)/KEYLNQ,0)                    
         DC   CL3'CL'                  MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
PF06     DC   AL1(KEYTYTWA,L'CL2MED-1),AL2(CL2MED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CL2CLT-1),AL2(CL2CLT-T217FFD)                      
PF06X    EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
TABLE1D  DSECT                     DSECT TO COVER MAIN TABLE                    
PROF     DS    XL1                 DISPLACEMENT INTO PROFILE                    
LITERAL  DS    CL21                SCREEN LITERAL                               
VALRTN   DS    XL4                 VALIDATION ROUTINE                           
DISPRTN  DS    XL4                 DISPLAY ROUTINE                              
RECDIS   DS    0XL2                DISP OF CPROF/CEXTRA IN CLT REC              
OPTBYTE  DS    XL2                 DISP OF OPTION BYTE IN CLIENT REC            
AVLST    DS    0XL4                ADDRESS OF VAL LIST                          
OPTEQU   DS    XL1                 OPTION BIT MASK                              
         DS    3XL1                                                             
*                                                                               
LTABLE1D EQU   *-TABLE1D           L(TABLE ENTRY)                               
*                                                                               
PROFILED DSECT                     DSECT TO COVER PROFILE RECORD Y/N'S          
PROFCOS2 DS    C                   CLIENT USE 2ND COST FEATURE?                 
PROFDBL  DS    C                     "     "   DBL BOOK TEST?                   
PROFINFO DS    C                     "     "   INFOMERCIAL?                     
PROFMGR  DS    C                     "     "   MARKET GROUPS?                   
PROFXJ1  DS    C                     "     "   EXCLUDE J1?                      
PROFXA7  DS    C                     "     "   EXCLUDE A7?                      
PROFCTA  DS    C                     "     "   CTA CLIENT?                      
PROFGMI  DS    C                     "     "   GMI CLIENT?                      
PROFUPL  DS    C                     "     "   EXCLUDE DELETED BUYS?            
PROFZEN  DS    C                     "     "   EXTRACT  CLIENT?                 
PROFNPWB DS    C                     "     "   NO PW BILLING?                   
PROFPWP  DS    C                     "     "   PW PERCENT?                      
PROFTRAD DS    C                     "     "   TRADE CLIENT?                    
PROFFRZ  DS    C                     "     "   FREEZE?                          
PROFXEST DS    C                     "     "   CROSS EST RPT'ING?               
PROFACS  DS    C                     "     "   LIMITED ACCESS CODE              
PROFBOX  DS    C                     "     "   LOCK BOX NUMBER                  
PROFBILF DS    C                     "     "   BILL FORMULA CONTROL?            
PROFBILE DS    C                     "     "   BILL ESTIMATE CONTROL?           
PROFCTAX DS    C                     "     "   CANADIAN NETWORK TAX             
PROFCAMP DS    C                     "     "   CAMPAIGNS                        
PROFMKGD DS    C                     "     "   MKGDS IN MISSED MTH              
PROFBP   DS    C                     "     "   BUY PROGRAMMING                  
PROFRFP  DS    C                     "     "   T/A RFP GROUP?                   
PROFCTY  DS    C                     "     "   TEST CLT TYPE                    
PROFSEC  DS    C                     "     "   PRODUCT LEVEL SECURITY           
PROFC2CD DS    C                     "     "   COST2 CONVERSION DATE            
PROFCPPR DS    C                     "     "   CPPRS OPTION                     
PROFBRD  DS    C                     "     "   ALLOW BRD ESTIMATES              
PROFSPOD DS    C                     "     "   ALLOW SPODS                      
PROFUCOM DS    C                     "     "   UCOM BILL CONTROL                
PROFTBEX DS    C                     "     "   TIMEBANK EXCLUSION               
PROFBPCT DS    C                     "     "   BILL PCT SPLIT                   
PROFMDAS DS    C                     "     "   MIDAS                            
*                                                                               
FIELDD   DSECT                     DSECT TO COVER SCREEN FIELDS                 
SCRNLITH DS    XL8                 LITERAL FIELD HEADER                         
SCRNLIT  DS    CL21                LITERAL                                      
DATAH    DS    XL8                 DATA FIELD HEADER                            
DATA     DS    CL8                 DATA (Y/N, ETC.)                             
*                                                                               
LFIELDD  EQU   *-FIELDD            L(SCREEN FIELD)                              
         EJECT                                                                  
*                                                                               
CLTHRD   DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
*++INCLUDE SPSFMWORKD                                                           
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
SVAGYFL1 DS    XL1                 IMPORTAN FLAG BITS IN THE AGY RECORD         
SVCLTKEY DS    CL48                SAVE CLT KEY                                 
MYCTKEY  DS    CL48                KEY TO READ RFP GROUP REC FROM CTFIL         
PROFILE  DS    CL48                32 BYTE PROFILE AREA                         
MYBYTE1  DS    XL1                 SAVED OPTION BYTE 1                          
MYBYTE2  DS    XL1                 SAVED OPTION BYTE 2                          
MYBYTE4  DS    XL1                 SAVED OPTION BYTE 4                          
RETFLAG  DS    XL1                 RETURN TO LIST FLAG                          
DISFLAG  DS    XL1                 SCREEN DISPLAYED FLAG, LIST PF CALL          
SELSTFLG DS    XL1                 SELECT FROM LIST FLAG                        
SVRFPID  DS    XL2                 SAVE RFP ID FROM AGY REC                     
NOPROTFL DS    X                   PROTECT FLAG                                 
SVDATE   DS    XL3                                                              
OFCBLK   DS    XL(OFCLENQ)         OFFICER BLOCK                                
*                                                                               
*                                                                               
         PRINT  OFF                                                             
         EJECT                                                                  
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM78D          MAINTENACE SCREEN                            
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
       ++INCLUDE CTGENFILE         FOR CTSYSD IN VALACC                         
*PREFIX=AC$                                                                     
       ++INCLUDE ACGENFILE         FOR CPYELD & OFFRECD IN VALACC               
*PREFIX=                                                                        
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDPSTBLK          FOR PSTVAL                                   
       ++INCLUDE DDCOREQUS         FOR PSTVAL                                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENGRP          FOR DELETING CLIENTS                         
       ++INCLUDE SPGENNDEF         FOR DELETING CLIENTS                         
*PREFIX=CT$                                                                     
       ++INCLUDE CTGENRFP                                                       
*PREFIX=                                                                        
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103SPSFM08   02/24/20'                                      
         END                                                                    
