*          DATA SET TAGEN39    AT LEVEL 008 AS OF 01/10/07                      
*PHASE T70239A                                                                  
         TITLE 'T70239 - USAGE HISTORY MAINTENANCE'                             
T70239   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70239,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         TM    TGSYSTAT,TASYSPID   CHANGE SCREEN IF SHOWING PID                 
         BZ    UH02                                                             
         MVC   SUHHED(3),=C'Pid'                                                
         OI    SUHHEDH+6,X'80'                                                  
                                                                                
UH02     CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   UH10                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
UH10     CLI   MODE,DISPREC                                                     
         BE    UH15                                                             
         CLI   MODE,XRECPUT        IF MODE IS RECORD CHANGED                    
         BNE   UH20                                                             
         GOTO1 ADDPTRS,DMCB,PTRS   ADD PASSIVE PTRS (NONE)                      
         SPACE                                                                  
UH15     BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
UH20     CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   XIT                                                              
         SPACE                                                                  
         GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE PTRS (NONE)                     
         SPACE                                                                  
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND BUILDS THE KEY                  
         SPACE                                                                  
VKEY     NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SUHAGYH),SUHAGYNH  AGY NAME           
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SUHCIDH),SUHCIDNH COMM TITLE         
         SPACE                                                                  
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
         SPACE                                                                  
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    VKA                 VERSIONS                                     
         CLI   TLCOIVER,26         AND VERSION IS GREATER THAN 26               
         BNH   VKA                 READ MAIN COMMERCIAL RECORD                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'AC',0),SUHCIDNH                          
         BE    VKA                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
         SPACE                                                                  
VKA      L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TACOD,R4                                                         
         GOTO1 MEDVAL,DMCB,TACOMED SET MEDIA EQUATE                             
         SPACE                                                                  
         LA    R2,SUHUSEH                                                       
         TM    4(R2),X'20'         TEST IF USE CODE PREV VALIDATED              
         BO    VK2                                                              
         CLI   5(R2),0             IF NO USE CODE INPUT                         
         BNE   VK1                                                              
         OC    TGUSCDE,TGUSCDE     USE GLOBAL IF AROUND                         
         BZ    FLDMISS             IF NOT, REQUIRE INPUT                        
         MVC   SUHUSE,TGUSCDE                                                   
         OI    6(R2),X'80'         DISPLAY GLOBAL USE CODE                      
         SPACE                                                                  
VK1      GOTO1 USEVAL,DMCB,(X'40',SUHUSE) VALIDATE IT WITHOUT TYPE              
         BNE   FLDINV                                                           
         OI    4(R2),X'20'         SET VALIDATED                                
         MVC   SUHUSEN,TGUSNAME    DISPLAY USE NAME                             
         OI    SUHUSENH+6,X'80'                                                 
         SPACE                                                                  
VK2      TM    SUHSSNH+4,X'20'     IF SSN FIELD PREV VALIDATED                  
         BZ    VK2B                                                             
         TM    SUHCATH+4,X'20'     AND CAT FIELD PREV VALIDATED                 
         BO    VK3                 THEN DON'T BOTHER, USE SAME CSEQ             
         SPACE                                                                  
VK2B     XC    CSEQ,CSEQ           PRE-CLEAR CAST INPUT SEQUENCE                
         CLI   SUHSSNH+5,0         IF NO SSN INPUT                              
         BNE   VK2D                                                             
         CLI   SUHCATH+5,0         THEN CAT INPUT NOT ALLOWED                   
         BE    VK3                                                              
         LA    R2,SUHCATH                                                       
         B     NOINPUT                                                          
                                                                                
VK2D     TM    TGSYSTAT,TASYSPID   USING PIDS?                                  
         BZ    VK2F                NO                                           
         CLI   SUHSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK2F                RECVAL CALL DOES NOT CHECK FOR               
         CLI   SUHSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SUHSSN        UNPACK IF PID ENTERED                        
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK2F                                                             
         MVC   SUHSSN,TGSSN                                                     
         MVI   SUHSSNH+5,9                                                      
                                                                                
VK2F     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SUHSSNH),SUHSSNNH  W4 NAME            
         BAS   RE,VALCAT           VALIDATE CATEGORY                            
                                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK3                 NO                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SUHSSN,SPACES                                                    
         MVC   SUHSSN(L'TGPID),TGPID                                            
         MVI   SUHSSNH+5,6                                                      
         OI    SUHSSNH+6,X'80'                                                  
                                                                                
VK3      CLI   SUHINVH+5,0         IF NO INVOICE INPUT                          
         BNE   VK5                                                              
         XC    TGINV,TGINV         CLEAR GLOBAL INVOICE                         
         LA    R2,SUHCYCH                                                       
         TM    4(R2),X'20'         TEST CYCLE PREV VALIDATED                    
         BO    VK6                                                              
         XC    STRT(6),STRT        CLEAR PACKED START AND END DATE              
         CLI   5(R2),0                                                          
         BE    VK4                                                              
         GOTO1 DATVAL,DMCB,SUHCYC,DUB  VALIDATE START DATE ONLY                 
         OC    0(4,R1),0(R1)                                                    
         BZ    DATEINV                                                          
         XC    SUHCYC,SUHCYC                                                    
         GOTO1 DATCON,DMCB,DUB,(1,STRT)  SAVE PACKED START                      
         GOTO1 (RF),(R1),,(8,SUHCYC)     REDISPLAY START DATE                   
         OI    6(R2),X'80'                                                      
VK4      OI    4(R2),X'20'         SET VALIDATED                                
         B     VK6                                                              
         SPACE                                                                  
VK5      LA    R2,SUHINVH                                                       
         TM    4(R2),X'20'         TEST INVOICE PREV VALIDATED                  
         BO    VK6                                                              
         GOTO1 TINVCON,DMCB,SUHINV,INV,DATCON  VALIDATE IT                      
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TGINV,INV                                                        
         XC    TGINV,HEXFFS        COMPLEMENT GLOBAL INVOICE NUMBER             
         OI    SUHINVH+4,X'20'     SET VALIDATED                                
         SPACE                                                                  
         USING TLUHD,R3                                                         
VK6      LA    R3,KEY              R3=A(KEY)                                    
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHCSEQ,CSEQ       CAST INPUT SEQ NUMBER                        
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         MVC   TLUHINV,TGINV       INVOICE                                      
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     VK10                                                             
VK9      GOTO1 SEQ                 GET NEXT USAGE HISTORY RECORD                
         SPACE                                                                  
VK10     CLC   TLUHKEY(TLUHUSE+L'TLUHUSE-TLUHD),KEYSAVE  TEST RIGHT USE         
         BNE   VK11                                                             
         CLI   SUHINVH+5,0         IF INVOICE INPUT                             
         BE    VK12                                                             
         CLC   TGINV,TLUHINV       CHECK IT                                     
         BE    VK12                                                             
VK11     LA    R2,SUHAGYH          IF NOT OK, GIVE ERROR IN CASE FOUND          
         B     NOREC               AT LEAST 1 RECORD WITH RIGHT USE             
         SPACE                                                                  
VK12     CLI   SUHCYCH+5,0         TEST HAVE DATES TO FILTER ON                 
         BE    XIT                 IF NOT, THEN DONE                            
         CLI   SUHINVH+5,0                                                      
         BNE   XIT                 IF HAVE INVOICE, THEN DONE                   
         SPACE                                                                  
         USING TAUHD,R4                                                         
         GOTO1 GETREC              ELSE GET THE RECORD                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   VK9                                                              
         CLC   TAUHSTRT,STRT       TEST START SAME AS IN ELEMENT                
         BNE   VK9                 IF NOT, GET NEXT                             
         B     XIT                 ELSE, DONE - FOUND RECORD                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE CATEGORY FIELD                               
         SPACE 1                                                                
VALCAT   NTR1                                                                   
         LA    R2,SUHCATH                                                       
         XC    TGCAT,TGCAT                                                      
         CLI   5(R2),0             OK TO LEAVE BLANK - I'LL LOOK UP             
         BE    VC10                                                             
         OC    8(3,R2),SPACES                                                   
         GOTO1 CATVAL,DMCB,8(R2)   VALIDATE CATEGORY CODE                       
         BE    VC10                                                             
         SPACE 1                                                                
         MVC   FULL(3),=3C'0'      IF NOT VALID TEST IF VALID HEX               
         ZIC   R1,5(R2)                                                         
         LA    RF,L'FULL                                                        
         SR    RF,R1                                                            
         LA    RF,FULL(RF)         SET TO RIGHT-ALIGN IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,FULL,CSEQ,4                                           
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         SPACE 1                                                                
VC10     XC    KEY,KEY             BUILD CAST KEY                               
         LA    R3,KEY                                                           
         USING TLCAPD,R3                                                        
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TGCOM      INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VC20     CLC   TLCAPKEY(TLCACCAT-TLCAPD),KEYSAVE  STILL SAME COMML              
         BNE   FLDINV                                                           
         OC    TGCAT,TGCAT         IF NO CATEGORY INPUT, TAKE FIRST ONE         
         BZ    VC30                                                             
         CLC   TLCACCAT,TGCAT      ELSE TAKE FIRST MATCH                        
         BE    VC50                                                             
         B     VC40                                                             
         SPACE 1                                                                
VC30     CLI   5(R2),0             IF NO INPUT TAKE FIRST ONE FOUND             
         BE    VC50                                                             
         CLC   TLCACSEQ,CSEQ       ELSE SCAN FOR MATCHING SEQUENCE NO.          
         BE    VC52                                                             
         SPACE 1                                                                
VC40     GOTO1 SEQ                 GET NEXT CAST RECORD                         
         B     VC20                AND KEEP ON TRYING                           
         SPACE 1                                                                
VC50     MVC   CSEQ,TLCACSEQ       SAVE CAST SEQ                                
         SPACE 1                                                                
VC52     MVC   8(3,R2),TLCACCAT    DISPLAY ACTUAL CATEGORY                      
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
         USING TLUHD,R3                                                         
DISPLAY  NTR1                                                                   
         TWAXC SUHTYPH                                                          
         GOTO1 FLDVAL,DMCB,(X'10',SUHTYPH),999 INVALIDATE ALL DATA FLDS         
         XC    SUHTYPN,SUHTYPN     CLEAR PROTECTED DATA FIELDS                  
         OI    SUHTYPNH+6,X'80'                                                 
         XC    SUHVERS,SUHVERS                                                  
         OI    SUHVERSH+6,X'80'                                                 
         XC    SUHDUE,SUHDUE                                                    
         OI    SUHDUEH+6,X'80'                                                  
         XC    SUHSPUN,SUHSPUN                                                  
         OI    SUHSPUNH+6,X'80'                                                 
         MVI   SUHUPGR,0                                                        
         OI    SUHUPGRH+6,X'80'                                                 
         GOTO1 FLDVAL,DMCB,(X'01',SUHCSYSH),SUHLSBH                             
         GOTO1 FLDVAL,DMCB,(X'01',SUHLPSBH),1                                   
         GOTO1 FLDVAL,DMCB,(X'01',SUHCUNH),SUHCSUBH                             
         GOTO1 FLDVAL,DMCB,(X'01',SUHPNYH),SUHPUNTH                             
         GOTO1 FLDVAL,DMCB,(X'01',SUHPCUNH),SUHCPREH                            
         GOTO1 FLDVAL,DMCB,(X'01',SUHUPGNH),SUHUPGCH                            
         L     R3,AIO                                                           
         MVC   TGINV,TLUHINV       SET GLOBAL INVOICE                           
         MVC   INV,TLUHINV                                                      
         XC    INV,HEXFFS          UNCOMPLEMENT INVOICE NUMBER                  
         GOTO1 TINVCON,DMCB,INV,SUHINV,DATCON  DISPLAY IT                       
         OI    SUHINVH+4,X'20'     SET VALIDATED                                
         OI    SUHINVH+6,X'80'                                                  
         SPACE                                                                  
         USING TAUHD,R4                                                         
         LR    R4,R3                                                            
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         OC    TAUHSTRT(6),TAUHSTRT                                             
         BZ    DISP5                                                            
         GOTO1 DATCON,DMCB,(X'11',TAUHSTRT),(8,SUHCYC)  DISPLAY CYCLE           
         OI    SUHCYCH+6,X'80'                                                  
         OI    SUHCYCH+4,X'20'     SET VALIDATED                                
         MVC   STRT(6),TAUHSTRT    SAVE PACKED START AND END                    
         SPACE                                                                  
DISP5    GOTO1 USEVAL,DMCB,TGUSCDE,TAUHTYPE  USE TYPE                           
         BNE   DISP7                                                            
         OI    SUHTYPH+4,X'20'     SET VALIDATED                                
         MVC   SUHTYP,TGUSTYCD     DISPLAY USE TYPE CODE                        
         CLC   TGUSTYCD,SPACES                                                  
         BE    *+10                                                             
         MVC   SUHTYPN,TGUSNAME    DISPLAY NAME IF CODE NOT SPACES              
         SPACE                                                                  
DISP7    TM    TGUSSTA3,NWKUSE     TEST IF NETWORK USE                          
         BZ    *+12                                                             
         BAS   RE,DISCLA           SPECIAL DISPLAY ROUTINE                      
         B     DISP10                                                           
         CLI   TGUSEQU,USNW        TEST IF SNW                                  
         BNE   *+12                                                             
         BAS   RE,DISSNW           SPECIAL DISPLAY ROUTINE                      
         B     DISP10                                                           
         CLI   TGUSEQU,ULCB        TEST IF LCB                                  
         BNE   *+12                                                             
         BAS   RE,DISLCB           SPECIAL DISPLAY ROUTINE                      
         B     DISP10                                                           
         CLI   TGUSEQU,UCBL        TEST IF CBL                                  
         BE    DISP8                                                            
         CLI   TGUSEQU,USCB        OR SCB                                       
         BNE   *+12                                                             
DISP8    BAS   RE,DISCBL           SPECIAL DISPLAY ROUTINE                      
         B     DISP10                                                           
         CLI   TGUSEQU,UWSP        TEST IF WSP                                  
         BE    DISP9                                                            
         CLI   TGUSEQU,UWSC        OR WSP                                       
         BE    DISP9                                                            
         CLI   TGUSEQU,UWSM        OR WSM                                       
         BE    DISP9                                                            
         CLI   TGUSEQU,UNET        OR NET                                       
         BE    DISP9                                                            
         CLI   TGUSEQU,UADC        OR ADC                                       
         BE    DISP9                                                            
         CLI   TGUSEQU,UADW        OR ADW                                       
         BE    DISP9                                                            
         CLI   TGUSEQU,USWS        OR SWS                                       
         BNE   *+8                                                              
DISP9    BAS   RE,DISWSP           SPECIAL DISPLAY ROUTINE                      
         SPACE                                                                  
         USING TADDD,R4                                                         
DISP10   L     R4,AIO                                                           
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP15                                                           
         GOTO1 DATCON,DMCB,(1,TADDDATE),(8,SUHDUE)  DISPLAY DUE DATE            
         SPACE                                                                  
         USING TAVRD,R4                                                         
DISP15   L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      GET VERSIONS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         MVC   SUHVERS(1),TAVRVERS    SHOW VERSION CODE PAID                    
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    DISPX                                                            
         EDIT  TAVRVERS,SUHVERS,ALIGN=LEFT                                      
         SPACE                                                                  
DISPX    GOTO1 ACTVOUT,DMCB,SUHLCHGH  LAST CHANGED                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS USAGE HISTORY DETAILS FOR CLA                   
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
DISCLA   NTR1                                                                   
         EDIT  TAUHUSN,(3,SUHTU),ZERO=NOBLANK,ALIGN=LEFT  TOTAL USES            
         OI    SUHTUH+4,X'20'      SET VALIDATED                                
         EDIT  TAUHUSNL,(3,SUHLU),ZERO=BLANK,ALIGN=LEFT   LIFT USES             
         OI    SUHLUH+4,X'20'      SET VALIDATED                                
         MVC   CLADATA,TAUHUSN     SAVE INFO                                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DISPLAYS USAGE HISTORY DETAILS FOR SNW                   
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
DISSNW   NTR1                                                                   
         EDIT  TAUHUNT,(4,SUHSUNT),ZERO=NOBLANK  CURRENT UNITS                  
         MVI   BYTE,C'Y'          USE Y FOR UPGRADE                             
         TM    TAUHSTAT,TAUHSFUP                                                
         BZ    *+8                                                              
         MVI   BYTE,C'F'          OR F IF FORCED                                
         SPACE                                                                  
         TM    TAUHSTAT,TAUHSUPN  TEST FOR UPGRADE                              
         BZ    *+10                                                             
         MVC   SUHUPGN,BYTE                                                     
         TM    TAUHSTAT,TAUHSUPW                                                
         BZ    *+10                                                             
         MVC   SUHUPGW,BYTE                                                     
         TM    TAUHSTAT,TAUHSUPC                                                
         BZ    *+10                                                             
         MVC   SUHUPGC,BYTE                                                     
         SPACE 1                                                                
         OC    TAUHIUNT,TAUHIUNT                                                
         BZ    DISSX                                                            
         EDIT  TAUHIUNT,(4,SUHSPUN),ZERO=NOBLANK   PREVIOUS UNITS               
         SPACE                                                                  
DISSX    OI    SUHSUNTH+4,X'20'    SET UNIT FIELD VALID                         
         MVC   WSPDATA,TAUHWAUT    SAVE INFO                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS USAGE HISTORY DETAILS FOR WILDSPOTS             
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
DISWSP   NTR1                                                                   
         CLI   TAUHMAJ,0                                                        
         BE    DISW5                                                            
         TM    TAUHMAJ,NY          CURRENT MAJORS                               
         BZ    *+8                                                              
         MVI   SUHNY,C'Y'                                                       
         TM    TAUHMAJ,LA                                                       
         BZ    *+8                                                              
         MVI   SUHLA,C'Y'                                                       
         TM    TAUHMAJ,CHI                                                      
         BZ    *+8                                                              
         MVI   SUHCHI,C'Y'                                                      
         SPACE 1                                                                
DISW5    EDIT  TAUHUNT,(4,SUHUNT),ZERO=NOBLANK  CURRENT UNITS                   
         SPACE 1                                                                
         TM    TAUHSTAT,TAUHSFIN                                                
         BZ    *+8                                                              
         MVI   SUHUPGR,C'I'        OR I IF FORCED INITIAL                       
         SPACE 1                                                                
         CLI   TAUHIMAJ,0                                                       
         BE    DISW10                                                           
         MVI   SUHUPGR,C'Y'        SET UPGRADE=Y                                
         TM    TAUHSTAT,TAUHSFUP                                                
         BZ    *+8                                                              
         MVI   SUHUPGR,C'F'        OR F IF FORCED                               
         SPACE                                                                  
         TM    TAUHIMAJ,NY         PREVIOUS MAJORS                              
         BZ    *+8                                                              
         MVI   SUHPNY,C'Y'                                                      
         TM    TAUHIMAJ,LA                                                      
         BZ    *+8                                                              
         MVI   SUHPLA,C'Y'                                                      
         TM    TAUHIMAJ,CHI                                                     
         BZ    *+8                                                              
         MVI   SUHPCHI,C'Y'                                                     
         SPACE 1                                                                
DISW10   OC    TAUHIUNT,TAUHIUNT                                                
         BZ    DISWX                                                            
         EDIT  TAUHIUNT,(4,SUHPUNT),ZERO=NOBLANK   PREVIOUS UNITS               
         MVI   SUHUPGR,C'Y'                        SET UPGRADE=Y                
         TM    TAUHSTAT,TAUHSFUP                                                
         BZ    *+8                                                              
         MVI   SUHUPGR,C'F'                        OR F IF FORCED               
         SPACE                                                                  
DISWX    GOTO1 FLDVAL,DMCB,(X'20',SUHNYH),SUHUNTH  SET WSP FLDS VALID           
         MVC   WSPDATA,TAUHWAUT    SAVE INFO                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS USAGE HISTORY DETAILS FOR LCB                   
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
DISLCB   NTR1                                                                   
         CLI   TAUHLEN,TAUHLNQ                                                  
         BL    DLCB10                                                           
         EDIT  TAUHCSYS,SUHCSYS,ZERO=NOBLANK         CURRENT CSYS               
         SPACE                                                                  
DLCB10   CLI   TAUHFRTY,0                            UPGRADE?                   
         BE    *+8                                                              
         MVI   SUHLUP,C'Y'                                                      
         SPACE                                                                  
         TM    TAUHLCST,TAUHLSFU                     FORCED UPGRADE?            
         BZ    *+8                                                              
         MVI   SUHLUP,C'F'                                                      
         SPACE                                                                  
         TM    TAUHLCST,TAUHLSFI                     FORCED INITIAL?            
         BZ    *+8                                                              
         MVI   SUHLUP,C'I'                                                      
         SPACE                                                                  
         LA    RE,SUBSTAB                                                       
DLCB20   CLI   0(RE),X'FF'                           CURRENT SUBS               
         BE    DLCB40                                (OLD USAGE HISTORY         
         CLC   TAUHTYPE,11(RE)                        ELEMENT FORMAT)           
         BE    DLCB30                                                           
         LA    RE,L'SUBSTAB(RE)                                                 
         B     DLCB20                                                           
DLCB30   MVC   SUHLSB,4(RE)                                                     
         SPACE                                                                  
DLCB40   CLI   TAUHLEN,TAUHLNQ                                                  
         BL    DLCB50                                                           
         EDIT  TAUHSUBS,SUHLSB,ZERO=NOBLANK          CURRENT SUBS               
         SPACE                                                                  
         EDIT  TAUHISUB,SUHLPSB                      PREVIOUS SUBS              
         SPACE                                                                  
DLCB50   GOTO1 FLDVAL,DMCB,(X'20',SUHCSYSH),SUHLSBH  SET LCB FLDS VALID         
         MVC   LCBDATA,TAUHCSYS                                                 
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE DISPLAYS USAGE HISTORY DETAILS FOR CBL/SCB               
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
DISCBL   NTR1                                                                   
         EDIT  TAUHCBUN,SUHCUN,ZERO=NOBLANK          CURRENT UNITS              
         SPACE                                                                  
         TM    TAUHCSTA,TAUHSFIN                     FORCED INITIAL?            
         BZ    *+8                                                              
         MVI   SUHCUP,C'I'                                                      
         SPACE                                                                  
         OC    TAUHICBU,TAUHICBU                     UPGRADE?                   
         BZ    *+8                                                              
         MVI   SUHCUP,C'Y'                                                      
         SPACE                                                                  
         TM    TAUHCSTA,TAUHSFUP                     FORCED UPGRADE?            
         BZ    *+8                                                              
         MVI   SUHCUP,C'F'                                                      
         SPACE                                                                  
         CLI   TAUHLEN,TAUHLNQ                                                  
         BL    DCBL10                                                           
         EDIT  TAUHCSUB,SUHCSUB,ZERO=NOBLANK         SUBSCRIBERS                
         SPACE                                                                  
         CLI   TAUHLCBU,0                            UPGRADED FROM LCB?         
         BE    *+10                                                             
         MVC   SUHCPRE,PREVLCB                                                  
         SPACE                                                                  
DCBL10   EDIT  TAUHICBU,SUHPCUN                      PREVIOUS UNITS             
         SPACE                                                                  
         GOTO1 FLDVAL,DMCB,(X'20',SUHCUNH),SUHCSUBH  SET CBL FLDS VALID         
         MVC   CBLDATA,TAUHCBUN                                                 
         B     XIT                                                              
         SPACE 2                                                                
PREVLCB  DC    CL(L'SUHCPRE)'(LCB Payment)'                                     
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         USING TAUHD,R4                                                         
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVI   BYTE,0                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      USAGE HISTORY ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   BLD2                                                             
         MVC   TGBYTE,TAUHLFT      SAVE LIFT BYTE                               
         MVC   TGBYTE2,TAUHSTAT    SAVE STATUS BYTE                             
         GOTO1 REMELEM             DELETE CURRENT                               
         SPACE                                                                  
BLD2     LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TAUHEL,TAUHELQ                                                   
         MVI   TAUHLEN,TAUHLNQ                                                  
         MVC   TAUHSTRT(6),STRT    START AND END DATES                          
         MVC   TAUHLFT,TGBYTE      LIFT                                         
         MVC   TAUHSTAT,TGBYTE2    OLD STATUS BYTE                              
         SPACE                                                                  
BLD3     LA    R2,SUHTYPH          USE TYPE                                     
         TM    4(R2),X'20'         TEST IF PREV VALIDATED                       
         BO    BLD5                                                             
         CLI   TGUSEQU,ULCB                                                     
         BE    BLD5                                                             
         CLI   5(R2),0             TEST IF ANY INPUT                            
         BNE   BLD4                                                             
         MVC   WORK(L'TGUSTYCD),SPACES                                          
         B     BLD4A                                                            
BLD4     GOTO1 ANY                 PAD INPUT WITH SPACES                        
BLD4A    GOTO1 USEVAL,DMCB,(X'10',SUHUSE),WORK                                  
         BNE   FLDINV                                                           
         MVC   BYTE,TGUSTYMD       VALID MEDIAS FOR THIS USE TYPE               
         NC    BYTE,TGMEEQU        CURRENT MEDIA EQUATE                         
         BZ    MEDERR              USE TYPE NOT VALID FOR THIS MEDIA            
         SPACE                                                                  
         GOTO1 UPGRVAL,DMCB,TGUSCDE,TGUSTYP                                     
         BE    FLDINV              INVALID IF TYPE IS AN UPGRADE                
         SPACE                                                                  
         OI    4(R2),X'20'         SET VALIDATED                                
         XC    SUHTYPN,SUHTYPN                                                  
         OI    SUHTYPNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   SUHTYPN,TGUSNAME    DISPLAY TYPE NAME IF CODE NOT SPACES         
         SPACE                                                                  
BLD5     MVC   TAUHTYPE,TGUSTYP    PUT TYPE EQUATE IN ELEMENT                   
         SPACE                                                                  
         TM    TGUSSTA3,NWKUSE     TEST IF NETWORK USE                          
         BZ    *+12                                                             
         BAS   RE,BLDCLA           SPECIAL BUILD ROUTINE                        
         B     BLDX                                                             
         CLI   TGUSEQU,USNW        TEST IF SNW                                  
         BNE   *+12                                                             
         BAS   RE,BLDSNW           SPECIAL BUILD ROUTINE                        
         B     BLDX                                                             
         CLI   TGUSEQU,ULCB        TEST IF LCB                                  
         BNE   *+12                                                             
         BAS   RE,BLDLCB           SPECIAL BUILD ROUTINE                        
         B     BLDX                                                             
         CLI   TGUSEQU,UCBL        TEST IF CBL                                  
         BE    BLD6                                                             
         CLI   TGUSEQU,USCB        OR SCB                                       
         BNE   *+12                                                             
BLD6     BAS   RE,BLDCBL           SPECIAL BUILD ROUTINE                        
         B     BLDX                                                             
         CLI   TGUSEQU,UWSP        TEST IF WSP                                  
         BE    BLD7                                                             
         CLI   TGUSEQU,UWSC        OR WSC                                       
         BE    BLD7                                                             
         CLI   TGUSEQU,UWSM        OR WSM                                       
         BE    BLD7                                                             
         CLI   TGUSEQU,UNET        OR NET                                       
         BE    BLD7                                                             
         CLI   TGUSEQU,UADW        OR ADW                                       
         BE    BLD7                                                             
         CLI   TGUSEQU,UADC        OR ADC                                       
         BE    BLD7                                                             
         CLI   TGUSEQU,USWS        OR SWS                                       
         BNE   *+8                                                              
BLD7     BAS   RE,BLDWSP           SPECIAL BUILD ROUTINE                        
         SPACE                                                                  
BLDX     GOTO1 ADDELEM               ADD NEW USAGE HISTORY ELEMENT              
         GOTO1 ACTVIN,DMCB,SUHLCHGH  LAST CHANGED                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY DETAILS FOR CLA                     
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
BLDCLA   NTR1                                                                   
         LA    R2,SUHTUH           TOTAL USES                                   
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BCLA5                                                            
         XC    USN,USN                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BCLA4                                                            
         BAS   RE,VALNUM           VALIDATE IT                                  
         STH   R1,USN              SAVE BINARY NUMBER                           
BCLA4    OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
BCLA5    LA    R2,SUHLUH           LIFT USES                                    
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BCLA10                                                           
         XC    USNL,USNL                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BCLA9                                                            
         BAS   RE,VALNUM                                                        
         STH   R1,USNL             SAVE BINARY NUMBER                           
BCLA9    OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
BCLA10   LH    RE,USN                                                           
         CH    RE,USNL                                                          
         BL    FLDINV              INVALID IF TOTAL USES < LIFT USES            
         MVC   TAUHUSN(L'CLADATA),CLADATA  SAVE IN ELEMENT                      
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES A NUMBER FIELD AT R2                           
*              RETURNS BINARY OF INPUT IN R1                                    
VALNUM   NTR1                                                                   
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         TEST FOR NUMERIC                             
         BZ    NUMINV                                                           
         ZIC   R1,5(R2)            CONVERT TO BINARY                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         * EXECUTED *                                 
         CVB   R1,DUB                                                           
         XIT1  REGS=(R1)           RETURN # OF UNITS IN R1                      
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY DETAILS FOR WSP                     
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
BLDWSP   NTR1                                                                   
         CLI   TGUSEQU,USWS        IF SWS                                       
         BE    BWSP5                                                            
         CLI   TGUSEQU,UADC        OR ADC                                       
         BE    BWSP5                                                            
         CLI   TGUSEQU,UADW        OR ADW, SKIP MAJORS                          
         BE    BWSP5                                                            
         GOTO1 FLDVAL,DMCB,(X'40',SUHNYH),SUHCHIH  TEST MAJORS VALID            
         BE    BWSP5                                                            
         MVI   MAJ,0               VALIDATE MAJORS - ANY INPUT IS OK            
         CLI   SUHNYH+5,0                                                       
         BE    *+8                                                              
         OI    MAJ,NY                                                           
         CLI   SUHLAH+5,0                                                       
         BE    *+8                                                              
         OI    MAJ,LA                                                           
         CLI   SUHCHIH+5,0                                                      
         BE    *+8                                                              
         OI    MAJ,CHI                                                          
         GOTO1 FLDVAL,DMCB,(X'20',SUHNYH),SUHCHIH  SET VALIDATED                
         SPACE 1                                                                
BWSP5    LA    R2,SUHUNTH          R2=A(UNITS FIELD)                            
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BWSP10                                                           
         XC    UNT,UNT                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BWSP9                                                            
         BAS   RE,VALNUM           VALIDATE UNITS                               
         STH   R1,UNT              SAVE N'UNITS                                 
BWSP9    OI    4(R2),X'20'         SET VALIDATED                                
         SPACE 1                                                                
BWSP10   B     BWSPX                                                            
*WSP10   CLI   MAJ,0               TEST THEY INPUT SOMETHING                    
* NO-OP  BNE   BWSP12                                                           
         BNE   BWSPX                                                            
         OC    UNT,UNT                                                          
         BZ    NEEDUNIT                                                         
         SPACE                                                                  
BWSPX    MVC   TAUHWAUT(L'WSPDATA),WSPDATA SAVE IN ELEMENT                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THAT WSP DATA IS A VALID UPGRADE             
         SPACE 1                                                                
*&&UK                                                                           
VALUPGR  NTR1                                                                   
         LA    R2,SUHNYH                                                        
         MVI   BYTE,0                                                           
         LA    R1,IMAJ             COUNT PREVIOUS MAJORS                        
         BAS   RE,COUNTMAJ                                                      
         LR    R4,R0               SAVE COUNT IN R4                             
         SPACE                                                                  
         LA    R1,MAJ              COUNT CURRENT MAJORS                         
         BAS   RE,COUNTMAJ                                                      
         CR    R4,R0                                                            
         BH    NOTUPG              CURRENT MAJORS CAN'T DECREASE                
         BE    *+8                                                              
         MVI   BYTE,C'Y'           SET CURRENT MAJORS INCREASED                 
         SPACE                                                                  
         LA    R2,SUHUNTH                                                       
         LH    R1,IUNT             COMPARE PREVIOUS UNITS                       
         CH    R1,UNT              AGAINST CURRENT UNITS                        
         BH    NOTUPG              CURRENT UNITS CAN'T DECREASE                 
         BL    XIT                 OK IF CURRENT UNITS INCREASED                
         CLI   BYTE,C'Y'                                                        
         BE    XIT                 OR IF CURRENT MAJORS INCREASED               
         B     NOTUPG              ELSE NOT AN UPGRADE                          
         SPACE 3                                                                
*              ROUTINE COUNTS NUMBER OF MAJORS AT R1                            
*              RETURNS COUNT IN R0                                              
         SPACE                                                                  
COUNTMAJ NTR1                                                                   
         SR    R0,R0                                                            
         LA    R2,MAJTAB           R2=A(TABLE OF MAJORS)                        
         SR    RE,RE                                                            
         SPACE                                                                  
CNTMAJ5  IC    RE,0(R2)                                                         
         LTR   RE,RE               TEST FOR END OF TABLE                        
         BZ    CNTMAJX                                                          
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0             TEST IF IT CONTAINS THIS MAJOR               
         BZ    *+8                                                              
         AH    R0,=H'1'            INCREMENT COUNT                              
         LA    R2,1(R2)            BUMP TO NEXT ENTRY IN MAJTAB                 
         B     CNTMAJ5                                                          
CNTMAJX  XIT1  REGS=(R0)                                                        
*&&                                                                             
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY DETAILS FOR SNW                     
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
BLDSNW   NTR1                                                                   
         LA    R2,SUHSUNTH         R2=A(UNITS FIELD)                            
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BSNWX                                                            
         XC    UNT,UNT                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BSNW9                                                            
         BAS   RE,VALNUM           VALIDATE UNITS                               
         STH   R1,UNT              SAVE N'UNITS                                 
BSNW9    OI    4(R2),X'20'         SET VALIDATED                                
         SPACE 1                                                                
BSNWX    MVC   TAUHWAUT(L'WSPDATA),WSPDATA  SAVE IN ELEMENT                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE BUILDS USAGE HISTORY DETAILS FOR LCB                     
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
BLDLCB   NTR1                                                                   
         LA    R2,SUHCSYSH         R2=A(CSYS FIELD)                             
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BLCB10                                                           
         XC    CSYS,CSYS                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BLCB10                                                           
         BAS   RE,VALNUM           VALIDATE UNITS                               
         STH   R1,CSYS             SAVE N'CSYS                                  
BLCB10   OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
         LA    R2,SUHLSBH          R2=A(SUBSRIBER FIELD)                        
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BLCB20                                                           
         XC    SUBS,SUBS                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BLCB20                                                           
         BAS   RE,VALNUM           VALIDATE SUBSCRIBERS                         
         ST    R1,SUBS             SAVE N'SUBS                                  
BLCB20   OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
         MVC   TAUHCSYS(L'CBLDATA),LCBDATA                                      
         SPACE                                                                  
         LA    RE,SUBSTAB        FIND NUMBER OF SUBSCRIBERS IN                  
BLCB30   CLI   0(RE),X'FF'       TABLE                                          
         BE    BLCBX                                                            
         CLC   SUBS,0(RE)                                                       
         BNH   BLCB40                                                           
         LA    RE,L'SUBSTAB(RE)                                                 
         B     BLCB30                                                           
BLCB40   MVC   TAUHTYPE,11(RE)   AND ASSIGN CURRENT TYPE                        
         SPACE 1                                                                
BLCBX    B     XIT                                                              
         SPACE 2                                                                
*              SUBSCRIBER TABLE                                                 
*              FIRST FIELD:  HEXIDECIMAL NUMBER OF SUBSCRIBERS                  
*              SECOND FIELD: FULLWORD NUMBER OF SUBSCRIBERS                     
*              THIRD FIELD:  USE TYPE EQUATE                                    
         SPACE 1                                                                
         DC    0F                                                               
SUBSTAB  DC    0XL12                                                            
         DC    F'50000',C'50000  ',AL1(ULCB50)                                  
         DC    F'100000',C'100000 ',AL1(ULCB100)                                
         DC    F'150000',C'150000 ',AL1(ULCB150)                                
         DC    F'200000',C'200000 ',AL1(ULCB200)                                
         DC    F'250000',C'250000 ',AL1(ULCB250)                                
         DC    F'500000',C'500000 ',AL1(ULCB500)                                
         DC    F'750000',C'750000 ',AL1(ULCB750)                                
         DC    F'1000000',C'1000000',AL1(ULCB1M)                                
         DC    F'20000000',C'1000001',AL1(ULCBMAX)                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY DETAILS FOR CBL/SCB                 
*              R4=A(TAUHEL)                                                     
         USING TAUHD,R4                                                         
         SPACE                                                                  
BLDCBL   NTR1                                                                   
         LA    R2,SUHCUNH          R2=A(UNITS FIELD)                            
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BCBL10                                                           
         XC    CBUN,CBUN                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BCBL10                                                           
         BAS   RE,VALNUM           VALIDATE UNITS                               
         STH   R1,CBUN             SAVE N'UNITS                                 
BCBL10   OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
         LA    R2,SUHCSUBH         R2=A(SUBSRIBER FIELD)                        
         TM    4(R2),X'20'         TEST IF VALIDATED                            
         BO    BCBL20                                                           
         XC    CSUB,CSUB                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    BCBL20                                                           
         BAS   RE,VALNUM           VALIDATE SUBSCRIBERS                         
         ST    R1,CSUB             SAVE N'SUBS                                  
BCBL20   OI    4(R2),X'20'         SET VALIDATED                                
         SPACE                                                                  
         MVC   TAUHCBUN(L'CBLDATA),CBLDATA                                      
         B     XIT                                                              
         EJECT                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
DATEINV  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEEND                                                           
NOREC    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
NUMINV   MVI   ERROR,NOTNUM        NOT VALID NUMERIC DATA                       
         B     THEEND                                                           
NOTUPG   MVI   ERROR,ERNOTUPG      THIS IS NOT AN UPGRADE                       
         B     THEEND                                                           
NEEDUNIT MVI   ERROR,ERNEEDUN      AT LEAST ONE UNIT REQUIRED                   
         B     THEEND                                                           
MEDERR   MVI   ERROR,ERUSEMED      USE TYPE INVALID FOR THIS MEDIA              
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CGROUP',CL8'LIST'                                     
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CLIENT',CL8'LIST'                                     
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGCLG-1),AL2(TGCLG-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 3                                                                
*              TABLE OF MAJORS                                                  
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR39D                                                       
         SPACE 3                                                                
         ORG SUHWORK                                                            
STRT     DS    PL3                 PACKED START                                 
END      DS    CL3                 AND END                                      
INV      DS    CL6                 INVOICE                                      
TYPE     DS    XL1                 USE TYPE EQUATE                              
CSEQ     DS    XL2                 CAST INPUT SEQUENCE NUMBER                   
         DS    0H                                                               
DATA     DS    CL(L'TAUHDATA)      DATA (MUST BE SAME ORDER AS TAUHEL)          
         ORG   DATA                                                             
CLADATA  DS    0CL4                CLASS A                                      
USN      DS    H                   LAST USE NUMBER                              
USNL     DS    H                   LAST USE NUMBER LIFT                         
         ORG   DATA                                                             
WSPDATA  DS    0CL9                WILDSPOT                                     
AUNT     DS    H                   AUTO UNITS                                   
UNT      DS    H                   UNITS                                        
IUNT     DS    H                   INITIAL UNITS                                
MAJ      DS    XL1                 MAJORS                                       
IMAJ     DS    XL1                 INITIAL MAJORS                               
STAT     DS    XL1                 STATUS                                       
         ORG   DATA                                                             
CBLDATA  DS    0CL16               CABLE                                        
CBUN     DS    H                   UNITS                                        
ICBU     DS    H                   INITIAL UNITS                                
CAUT     DS    H                   AUTOMATIC UNITS CALCULATION                  
LCBU     DS    X                   LCB UPGRADE TYPE                             
         DS    XL5                 SPARE                                        
CSUB     DS    F                   SUBSCRIBER COUNT                             
         ORG   DATA                                                             
LCBDATA  DS    0CL16               LOCAL CABLE                                  
CSYS     DS    H                   NUMBER OF CSYS PAID                          
FRTY     DS    XL1                 UPGRADE FROM TYPE                            
LCST     DS    XL1                                                              
SUBS     DS    F                   SUBSCRIBERS                                  
SUB      DS    F                   INITIAL SUBSCRIBERS                          
ASUB     DS    F                   AUTOMATIC SUBSCRIBERS CALCULATION            
         SPACE 1                                                                
PTRS     DS    CL(5*L'TLDRREC+1)                                                
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008TAGEN39   01/10/07'                                      
         END                                                                    
