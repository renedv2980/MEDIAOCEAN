*          DATA SET TAGEN11    AT LEVEL 055 AS OF 05/29/15                      
*PHASE T70211C,*                                                                
         TITLE 'T70211 - STAFF MAINTENANCE'                                     
T70211   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70211                                                         
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
*                                                                               
         OI    SSTADHH+1,X'0C'     INIT ADD AGY HEADER TO ZERO INTENS           
         OI    SSTADAH+1,X'2C'     INIT ADD AGY INPUT TO PROTECTED              
         OI    SSTACHH+1,X'0C'     INIT ADD CGR HEADER TO ZERO INTENS           
         OI    SSTACGH+1,X'2C'     INIT ADD CGR INPUT TO PROTECTED              
         GOTO1 FLDVAL,DMCB,(X'02',SSTUSERH),999                                 
         EJECT                                                                  
* SAVE OLD PASSIVE POINTERS OR ADD NEW PASSIVE POINTER DEPENDING ON             
* THE MODE.                                                                     
*                                                                               
         CLI   MODE,VALREC         IF MODE VALREC, RECDEL, OR RECREST           
         BE    PP10                                                             
         CLI   MODE,RECDEL                                                      
         BE    PP10                                                             
         CLI   MODE,RECREST                                                     
         BNE   PP20                                                             
*                                  THEN SAVE CURRENT PASSIVE POINTER(S)         
PP10     GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         B     PPX                                                              
*                                                                               
PP20     CLI   MODE,XRECADD        ELSE IF MODE XRECADD, XRECPUT,               
         BE    PP30                    XRECDEL, OR XRECREST                     
         CLI   MODE,XRECPUT                                                     
         BE    PP30                                                             
         CLI   MODE,XRECDEL                                                     
         BE    PP30                                                             
         CLI   MODE,XRECREST                                                    
         BNE   PPX                                                              
*                                  THEN ADD PASSIVE POINTER(S)                  
PP30     GOTO1 ADDPTRS,DMCB,PPBLOCK                                             
*                                                                               
PPX      DS    0H                                                               
*                                                                               
* BRANCH TO CORRECT ROUTINE FOR THE GIVEN MODE.                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    VR                                                               
         CLI   MODE,XRECADD        DISPLAY THE RECORD                           
         BE    DR                                                               
         CLI   MODE,XRECPUT        DISPLAY THE RECORD                           
         BE    DR                                                               
         CLI   MODE,XRECDEL        DISPLAY THE RECORD                           
         BE    DR                                                               
         CLI   MODE,XRECREST       DISPLAY THE RECORD                           
         BE    DR                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
         GOTO1 FLDVAL,DMCB,(X'42',SSTUSERH),(X'80',SSTSTAFH)                    
         BE    VK10                                                             
         XC    SSTADA,SSTADA                                                    
         MVI   SSTADAH+5,0                                                      
         XC    SSTACG,SSTACG                                                    
         MVI   SSTACGH+5,0                                                      
*                                  VALIDATE USER                                
VK10     GOTO1 USERVAL,DMCB,(X'40',SSTUSERH),SSTUNAMH                           
*                                                                               
*                                  VALIDATE STAFF CODE                          
         USING TLSTD,R3                                                         
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VK20                                                             
         LA    R3,KEY              BUILD STAFF KEY                              
         XC    KEY,KEY                                                          
         MVI   TLSTCD,TLSTCDQ                                                   
         MVC   TLSTUSER,TGUSER                                                  
         MVC   TLSTSTAF,SSTSTAF                                                 
         OC    TLSTSTAF,SPACES                                                  
         MVC   TGSTAF,TLSTSTAF                                                  
         B     VKX                                                              
         DROP  R3                                                               
*                                                                               
VK20     GOTO1 RECVAL,DMCB,TLSTCDQ,(X'20',SSTSTAFH)                             
*                                                                               
         MVC   SVSTF,SSTSTAF                                                    
         CLI   ACTNUM,ACTDEL       IF DELETING                                  
         BNE   VK30                                                             
         BRAS  RE,STFINRLP         CHECK IF STAFF IN RLP REQUESTS               
         BNE   ERREXIT             YES, ERROR                                   
*                                                                               
VK30     CLI   ACTNUM,ACTADD       DONE IF ADDING                               
         BE    VKX                                                              
*                                                                               
         CLC   TWAORIG,TGUSER      IF USER IS SAME AS RECORD THEN OK            
         BNE   *+14                                                             
         CLC   TGCTSTAF,TGSTAF                                                  
         BE    VKX                                                              
*                                                                               
         L     R4,AIO              ELSE POINT TO STAFF ELEMENT                  
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R4                                                         
         GOTO1 STAFVAL,DMCB,TASTTYPE,TGCTSTLV  TEST USER CAN ACCESS             
         BNE   ERRLOCK                         THIS STAFF TYPE                  
*                                                                               
VKX      GOTO1 FLDVAL,DMCB,(X'20',SSTUSERH),(X'80',SSTSTAFH)                    
         MVC   MYSTAF,TGSTAF       SAVE GLOBAL STAFF CODE                       
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY THE KEY                                                               
*                                                                               
DK       DS    0H                                                               
*                                                                               
         LA    R4,KEY              POINT TO STAFF KEY                           
         USING TLSTD,R4                                                         
*                                  DISPLAY USER ID AND NAME                     
         MVC   MYKEY,KEY           BACK UP KEY AND DISK ADDRESS                 
         MVC   AIO,AIO2            USE AIO2 FOR GETREC                          
         XC    WORK,WORK           SET UP WORK AREA FOR USERVAL CALL            
         MVC   WORK+8(2),TLSTUSER                                               
         GOTO1 USERVAL,DMCB,(X'E0',WORK),SSTUNAMH                               
         MVC   SSTUSER,TGUSERID    MOVE EBCDIC USER ID TO SCREEN                
         OI    SSTUSERH+6,X'80'    TRANSMIT                                     
         MVC   KEY,MYKEY           RESTORE KEY AND DISK ADDRESS                 
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         OC    TLSTSTAF,TLSTSTAF   IF STAFF CODE IS ZEROS                       
         BNZ   DK10                                                             
         MVC   SSTSTAF(3),=C'ALL'  THEN DISPLAY ALL                             
         B     DK20                                                             
*                                                                               
DK10     MVC   SSTSTAF,TLSTSTAF    ELSE DISPLAY STAFF CODE                      
*                                                                               
DK20     OI    SSTSTAFH+6,X'80'    TRANSMIT STAFF CODE                          
*                                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY THE RECORD                                                            
*                                                                               
DR       DS    0H                                                               
         TWAXC SSTFSTH             CLEAR FIELDS                                 
*                                                                               
         CLI   THISLSEL,C'D'       IF SELECTED FOR DELETION                     
         BNE   DR10                THEN SKIP DISPLAYING OF RECORD               
         GOTO1 FLDVAL,DMCB,(X'03',SSTTYPNH),1                                   
         GOTO1 FLDVAL,DMCB,(X'03',SSTMGRNH),1                                   
         GOTO1 FLDVAL,DMCB,(X'03',SSTLCHGH),1                                   
         B     DRX                                                              
*                                                                               
DR10     XC    SSTMGRN,SSTMGRN                                                  
         OI    SSTMGRNH+6,X'80'                                                 
         XC    SSTTYPN,SSTTYPN                                                  
         OI    SSTTYPNH+6,X'80'                                                 
*                                                                               
         L     R4,AIO              POINT TO STAFF ELEMENT                       
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R4                                                         
*                                                                               
         MVC   SSTFST,TASTFST      DISPLAY FIRST NAME                           
         MVC   SSTLST,TASTLST      DISPLAY LAST NAME                            
*                                                                               
         NI    SSTPWDH+1,X'F3'     SET TO NORMAL INTENSITY                      
         NI    SSTPWDH+1,X'DE'     SET UNPROTECTED                              
         MVC   SSTPWD,TASTPWD      DISPLAY PASSWRD                              
         CLI   TGCTSTTY,TASTTYPP   IF NOT P,2,3                                 
         BE    DR30                                                             
         CLI   TGCTSTTY,TASTTYP2                                                
         BE    DR30                                                             
         CLI   TGCTSTTY,TASTTYP3                                                
         BE    DR30                                                             
         CLC   TGCTSTAF,TGSTAF     OR YOURSELF                                  
         BE    DR30                                                             
         OI    SSTPWDH+1,X'20'     PROTECT                                      
         OI    SSTPWDH+1,X'0C'     AND SET ZERO INTENSITY                       
*                                                                               
DR30     OI    SSTPWDH+6,X'80'     TRANSMIT                                     
         OI    SSTPWDH+4,X'20'     SET PREVIOUSLY VALIDATED                     
*                                                                               
         MVC   SSTTYPE,TASTTYPE    DISPLAY TYPE                                 
         GOTO1 STAFVAL,DMCB,SSTTYPE,0                                           
         MVC   SSTTYPN,TGSTNAME    DISPLAY STAFF TYPE NAME                      
*                                                                               
         MVC   SSTMGR,TASTMGR      DISPLAY MANAGER                              
*                                  DISPLAY MANAGER NAME                         
         MVC   AIO,AIO2            USE AIO2 FOR RECVAL CALL                     
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'88',SSTMGR),SSTMGRNH                      
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         MVC   SSTTEL,TASTTEL      DISPLAY TELEPHONE EXT.                       
*                                                                               
*                                  DISPLAY SHORT NAME                           
         GOTO1 CHAROUT,DMCB,TASNELQ,SSTSNH                                      
*                                                                               
*                                  DISPLAY EMAIL ADDRESS                        
         GOTO1 CHAROUT,DMCB,TACMELQ,SSTEADDH,TACMTYPI                           
*                                  DISPLAY LAST CHANGED ELEMENT                 
         GOTO1 ACTVOUT,DMCB,SSTLCHGH                                            
*                                                                               
DRX      MVC   TGSTAF,MYSTAF       RESTORE GLOBAL STAFF CODE                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE THE RECORD                                                           
*                                                                               
VR       DS    0H                                                               
         MVC   MYKEY,KEY           BACK UP ACTIVE KEY                           
*                                                                               
         XC    SPASSWRD,SPASSWRD                                                
         L     R4,AIO              R4=A(STAFF RECORD)                           
         USING TASTD,R4                                                         
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VR15                                                             
         MVC   SPASSWRD,TASTPWD    SAVE OLD PASSWORD                            
         GOTO1 REMELEM             REMOVE OLD STAFF ELEMENT                     
*                                                                               
VR15     LA    R4,ELEM             BUILD STAFF ELEMENT FROM SCRATCH             
         USING TASTD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TASTEL,TASTELQ                                                   
         MVI   TASTLEN,TASTLNQ                                                  
*                                                                               
         LA    R2,SSTFSTH          VALIDATE FIRST NAME                          
         GOTO1 ANY                                                              
         MVC   TASTFST,WORK                                                     
*                                                                               
         LA    R2,SSTLSTH          VALIDATE LAST NAME                           
         GOTO1 ANY                                                              
         MVC   TASTLST,WORK                                                     
*                                                                               
         MVC   SHORTNAM,SPACES     BUILD SHORT NAME FROM FIRST/LAST             
         MVC   SHORTNAM(12),TASTFST                                             
         MVC   SHORTNAM+13(12),TASTLST                                          
         GOTO1 SQUASHER,DMCB,SHORTNAM,25                                        
         MVC   SNLENGTH,4(R1)                                                   
*                                                                               
         LA    R2,SSTPWDH          VALIDATE PASSWRD                             
         TM    4(R2),X'20'         IF PREVIOUSLY VALIDATED                      
         BO    *+12                                                             
         TM    1(R2),X'20'         OR IF PASSWRD PROTECTED                      
         BZ    *+14                                                             
         MVC   TASTPWD,SPASSWRD    RESET OLD PASSWORD                           
         B     VR20                                                             
         GOTO1 ANY                                                              
         MVC   TASTPWD,WORK        SET NEW CURRENT PASSWORD                     
*                                                                               
         MVC   SVELEM,ELEM         SAVE ELEMENT                                 
         GOTO1 PWRDVAL,DMCB,WORK   CHECKS PASSWRD NOT RECENTLY USED             
         BNE   ERRDUP              AND UPDATES TAPWD ELEMENTS                   
         MVC   ELEM,SVELEM         RESET ELEMENT                                
*                                                                               
VR20     LA    R2,SSTTYPEH         VALIDATE TYPE                                
         GOTO1 ANY                                                              
         GOTO1 STAFVAL,DMCB,8(R2),0                                             
         BNE   ERRINV                                                           
*                                                                               
         GOTO1 STAFVAL,DMCB,TGSTEQU,TGCTSTLV  TEST USER HAS ACCESS TO           
         BNE   ERRLOCK                        THIS LEVEL                        
*                                                                               
         MVC   TASTTYPE,TGSTEQU    MOVE IN TYPE                                 
*                                                                               
         LA    R2,SSTMGRH          VALIDATE MANAGER                             
         CLI   TASTTYPE,TASTTYPC   CLIENTS DON'T REQUIRE MGR                    
         BE    VR24                                                             
         CLI   TASTTYPE,TASTTYPD                                                
         BE    VR24                                                             
         CLI   TASTTYPE,TASTTYPF                                                
         BE    VR24                                                             
         TM    TGSTLVL,X'F8'       IF NOT HIGH LEVEL RECORD                     
         BZ    VR30                THEN MANAGER REQUIRED                        
*                                                                               
VR24     CLI   5(R2),0             ELSE FIELD IS OPTIONAL                       
         BE    VR40                                                             
*                                                                               
VR30     GOTO1 ANY                 VALIDATE FIELD                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(R2)                                         
         MVC   TASTMGR,8(R2)                                                    
         OC    TASTMGR,SPACES                                                   
*                                                                               
VR40     LA    R2,SSTTELH          VALIDATE OPTIONAL TELEPHONE EXT.             
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         MVC   TASTTEL,SSTTEL                                                   
         OC    TASTTEL,SPACES                                                   
*                                                                               
VR50     GOTO1 ADDELEM             ADD STAFF ELEMENT                            
*                                                                               
         LA    R2,SSTSNH           VALIDATE SHORT NAME                          
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VR60                                                             
         CLC   SNLENGTH,=F'16'     AND DEFAULT IS NOT TOO BIG                   
         BH    VR60                                                             
*                                                                               
         MVC   SSTSN,SHORTNAM      THEN MOVE DEFAULT INTO FIELD                 
         MVC   5(1,R2),SNLENGTH+3  INSERT FAKE INPUT LENGTH                     
         OI    6(R2),X'80'         AND TRANSMIT FIELD                           
*                                                                               
*                                  VALIDATE AND BUILD ELEMENT                   
VR60     GOTO1 NAMIN,DMCB,TASNELQ,(R2)                                          
*                                                                               
         BAS   RE,VREMAIL          VALIDATE EMAIL ADDRESS                       
*                                                                               
         BAS   RE,CLILIM           CLIENTS MUST HAVE LIMITS                     
*                                                                               
*                                  UPDATE LAST CHANGED ELEMENT                  
         GOTO1 ACTVIN,DMCB,SSTLCHGH                                             
*                                                                               
VRX      MVC   KEY,MYKEY           RESTORE ACTIVE KEY                           
         MVC   TGSTAF,MYSTAF       RESTORE GLOBAL STAFF CODE                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE REQUIRED EMAIL ADDRESS                                               
*                                                                               
VREMAIL  NTR1                                                                   
         LA    R2,SSTEADDH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMISS             EMAIL ADDRESS REQUIRED                       
         LA    RE,50                                                            
         LA    RF,SSTEADD                                                       
VREM20   CLI   0(RF),C'@'          THERE MUST BE A '@'                          
         BE    VREM30                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM20                                                        
         B     ERRINV                                                           
*                                                                               
VREM30   CLI   0(RF),C'.'          THERE MUST BE A '.'                          
         BE    VREM40                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM30                                                        
         B     ERRINV                                                           
*                                                                               
VREM40   GOTO1 NAMIN,DMCB,TACMELQ,(X'40',SSTEADDH),TACMTYPI                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ENSURE THAT CLIENTS HAVE AT LEAST ONE                 
*              AGENCY LIMIT SET                                                 
         SPACE 1                                                                
CLILIM   NTR1                                                                   
         CLI   TGSTEQU,TASTTYPC    CLIENTS REQUIRE LIMIT                        
         BE    CL10                                                             
         CLI   TGSTEQU,TASTTYPD                                                 
         BE    CL10                                                             
         CLI   TGSTEQU,TASTTYPF                                                 
         BNE   CLX                                                              
         SPACE 1                                                                
CL10     L     R4,AIO             EXIT IF LIMIT ALREADY SET                     
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    XIT                                                              
         SPACE 1                                                                
         NI    SSTADHH+1,X'FF'-X'04'  SET HEADER TO NORMAL INTENSITY            
         NI    SSTADAH+1,X'FF'-X'24'  UNPROTECT INPUT FIELD                     
         SPACE 1                                                                
         NI    SSTACHH+1,X'FF'-X'04'  SET HEADER TO NORMAL INTENSITY            
         NI    SSTACGH+1,X'FF'-X'24'  UNPROTECT INPUT FIELD                     
         SPACE 1                                                                
         XC    ADDAGY,ADDAGY      INTIALIZE VARIABLES                           
         SPACE 1                                                                
         USING TAVAD,R4                                                         
         LA    R4,ELEMENT         INITIALIZE LIMITED AGENCY/CLIENT              
         XC    ELEMENT,ELEMENT    ELEMENT                                       
         MVI   TAVAEL,TAVAELQ                                                   
         MVI   TAVALEN,TAVALNQ                                                  
         MVI   TAVASTAT,TAVANDEF                                                
         SPACE 1                                                                
         LA    R2,SSTADAH         R2=A(ADD AGENCY FIELD)                        
         CLI   5(R2),0            ADD AGENCY FIELD                              
         BNE   CL20                                                             
         CLI   SSTACGH+5,0        OR ADD CLIENT GROUP FIELD                     
         BE    ERRMISS            MUST BE POPULATED                             
         B     CL40                                                             
         SPACE 1                                                                
CL20     GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)    VALIDATE AGENCY                      
         MVC   ADDAGY,TGAGY                                                     
         SPACE 1                                                                
CL30     CLI   SSTACGH+5,0        IF NOT ADDING A CLIENT GROUP                  
         BNE   CL40                                                             
         MVC   TAVAAGY,ADDAGY     PUT AGENCY INTO LIMITED AGENCY/               
         B     CL100              CLIENT ELEMENT AND GO ADD IT                  
         DROP  R4                                                               
         SPACE 1                                                                
CL40     GOTO1 RECVAL,DMCB,TLCGCDQ,SSTACGH VALIDATE CLIENT GROUP                
         SPACE 1                                                                
         USING TASTD,R4                                                         
         L     R4,AIO             R4=A(STAFF ELEMENT)                           
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TASTCLG,TGCLG      SAVE CLIENT GROUP                             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVAD,R4                                                         
         LA    R4,ELEMENT         INITIALIZE VALID AGENCY/CLIENT                
         LA    R5,TAVACLI         ELEMENT                                       
         SPACE 1                                                                
         USING TLCLPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCLPCD,TLCLGCDQ                                                 
         MVC   TLCLGCLG,TGCLG                                                   
         MVC   TLCLGAGY,ADDAGY                                                  
         GOTO1 HIGH                                                             
         B     CL60                                                             
CL50     GOTO1 SEQ                                                              
CL60     CLC   KEY(TLCLGAGY-TLCLPD),KEYSAVE                                     
         BNE   CL100                                                            
         OC    TLCLGAGY,TLCLGAGY SKIP GLOBAL CLIENTS                            
         BZ    CL50                                                             
         OC    ADDAGY,ADDAGY     IF ADD AGENCY IS POPULATED                     
         BZ    CL70                                                             
         CLC   TLCLGAGY,ADDAGY   ONLY ADD CLIENTS BELONGING TO                  
         BNE   CL50              THAT AGENCY                                    
         SPACE 1                                                                
CL70     OC    TAVAAGY,TAVAAGY   IF ELEMENT ALREADY BEING BUILT                 
         BZ    CL90                                                             
         CLI   TAVALEN,255       AND HAS REACHED MAXIMUM LENGTH                 
         BNL   CL80                                                             
         CLC   TAVAAGY,TLCLGAGY  OR CURRENT CLIENT DOES NOT MATCH               
         BE    CL90                                                             
CL80     GOTO1 ADDELEM           ADD BUILT ELEMENT                              
         MVI   TAVALEN,TAVALNQ   AND INITIALIZE NEXT ELEMENT                    
         LA    R5,TAVACLI                                                       
         SPACE 1                                                                
CL90     ZIC   RE,TAVALEN                                                       
         AHI   RE,L'TAVACLI                                                     
         STC   RE,TAVALEN                                                       
         MVC   TAVAAGY,TLCLGAGY                                                 
         MVC   0(L'TAVACLI,R5),TLCLGCLI                                         
         LA    R5,L'TAVACLI(R5)                                                 
         B     CL50                                                             
         SPACE 1                                                                
CL100    OC    TAVAAGY,TAVAAGY   IF ELEMENT WAS BEING BUILT                     
         BZ    CL110                                                            
         GOTO1 ADDELEM           ADD BUILT ELEMENT                              
         DROP  R4                                                               
         SPACE 1                                                                
CL110    LA    R2,SSTACGH                                                       
         L     R4,AIO            LIMITS MUST BE SET BY THIS POINT               
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
         SPACE 1                                                                
         XC    SSTADA,SSTADA     CLEAR ADD AGENCY FIELD                         
         MVI   SSTADAH+5,0                                                      
         SPACE 1                                                                
         XC    SSTACG,SSTACG     CLEAR ADD CLIENT GROUP FIELD                   
         MVI   SSTACGH+5,0                                                      
CLX      B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ROUTINE TO CHECK IF STAFF IS USED IN RLP GROUPS                  
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING GRPKEYD,R3                                                       
STFINRLP NTR1                                                                   
                                                                                
         MVC   MYKEY,KEY           BACK UP KEY                                  
                                                                                
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   GRPKSYS,GRPKSYSQ       READ RLP GROUP X'002F'                    
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVI   GRPKSYST,C'T'          TALENT SYSTEM                             
         MVC   GRPKAGY,=C'DS'         ALL OF TALENT PARTNERS                    
         CLI   TGTALNUM,1             IF NOT TAL1                               
         BE    *+10                                                             
         MVC   GRPKAGY,=C'D2'         CHECK ON DPS2                             
                                                                                
         XR    R1,R1                                                            
         IC    R1,TGTALNUM                                                      
         SLL   R1,4                                                             
         STC   R1,SAVSE                                                         
                                                                                
         MVI   DMCB,X'0A'             SWITCH TO CONTROL SYSTEM                  
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                   ABEND IF CAN'T SWITCH                     
                                                                                
         GOTO1 DATAMGR,DMCB,(X'00',DMRDHI),=CL8'GENDIR',KEY,XDIR                
         J     SRLP20                                                           
                                                                                
SRLP10   GOTO1 DATAMGR,DMCB,(X'00',DMRSEQ),=CL8'GENDIR',KEY,XDIR                
                                                                                
SRLP20   TM    DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XR    R7,R7                                                            
         LA    R3,XDIR                                                          
         CLI   GRPKSYS,GRPKSYSQ       READ RLP GROUP X'002F'                    
         BNE   SRLP800                                                          
         CLI   GRPKSTYP,GRPKSTYQ                                                
         BNE   SRLP800                                                          
         CLI   GRPKSYST,C'T'          TALENT SYSTEM                             
         BNE   SRLP800                                                          
         CLI   TGTALNUM,1             IF NOT TAL1                               
         BNE   SRLP25                                                           
         CLC   GRPKAGY,=C'DS'         ALL OF TALENT PARTNERS                    
         BNE   SRLP800                                                          
         B     SRLP29                                                           
                                                                                
SRLP25   CLC   GRPKAGY,=C'D2'         CHECK ON DPS2                             
         BNE   SRLP800                                                          
                                                                                
SRLP29   MVC   SVUSRID,GRPKUSER       SAVE USERID                               
         MVC   SVGRP,GRPKGRP          SAVE GROUP                                
                                                                                
SRLP30   DS    0H                                                               
         MVC   DISKA,XDIR+36                                                    
         MVC   SVDISKA,DISKA                                                    
         L     R3,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(X'00',DMGET),=CL8'GENFILE',DISKA,(R3),XWRK         
                                                                                
         USING GRPRD,R4                                                         
SRLP200  LR    R4,R3                                                            
         MVI   DATADISP+1,GRPDISPQ                                              
         MVI   ELCODE,GRPRCDQ      LOOK FOR REQUEST ELEMENTS                    
         BRAS  RE,GETEL                                                         
         J     SRLP310                                                          
SRLP300  BRAS  RE,NEXTEL                                                        
SRLP310  JNE   SRLP10              NO MORE, NEXT RLP GROUP                      
                                                                                
         CLI   GRPRCRDN,0          BEGINNING OF REQUEST                         
         JNE   SRLP500                                                          
         MVI   SKIP,C'N'                                                        
*                                  COMMENT - IN CASE REQ ENABLED LATER          
*        TM    GRPRSTAT,GRPRDISQ   REQUEST DISABLED?                            
*        JZ    SRLP300                                                          
*        MVI   SKIP,C'Y'                                                        
         J     SRLP300                                                          
                                                                                
SRLP500  CLI   GRPRCRDN,1                                                       
         JH    SRLP300                                                          
         CLI   SKIP,C'Y'                                                        
         JE    SRLP300                                                          
                                                                                
         LA    R2,GRPCARD                                                       
         LA    RF,GRPRFXID                                                      
                                                                                
SRLP510  CLC   0(4,R2),=C'0408'    LOOK FOR STAFF ID                            
         JE    SRLP550                                                          
         AHI   R2,1                                                             
         CR    R2,RF                                                            
         JL    SRLP510                                                          
         J     SRLP300                                                          
                                                                                
SRLP550  DS    0H                                                               
         CLC   SVSTF,4(R2)                                                      
         JNE   SRLP300                                                          
         LHI   R7,1                                                             
                                                                                
         USING CTIREC,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,SVUSRID                                                  
         GOTO1 DATAMGR,DMCB,(X'00',DMRDHI),=CL8'CTFILE',KEY,AIO2                
         MVI   DATADISP+1,X'1C'                                                 
                                                                                
         USING CTDSCD,R4                                                        
         L     R4,AIO2                                                          
         MVI   ELCODE,CTDSCELQ                                                  
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
SRLP700  MVI   ERROR,X'FE'                                                      
         MVC   CONHEAD(40),=C'** ERROR ** CANNOT DELETE, USED IN RLP: '         
         MVC   CONHEAD+40(10),CTDSC                                             
         LA    RF,CONHEAD+40                                                    
SRLP730  CLI   0(RF),C' '                                                       
         BNH   SRLP750                                                          
         AHI   RF,1                                                             
         B     SRLP730                                                          
                                                                                
SRLP750  MVC   1(2,RF),=C'/ '                                                   
         MVC   3(8,RF),SVGRP                                                    
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
                                                                                
SRLP800  DS    0H                                                               
SRLPX    MVI   DATADISP+1,TLRCELEM-TLRCD                                        
         MVC   DMCB(1),SAVSE       SWITCH BACK TO ORIGINAL SYSTEM               
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY,MYKEY                                                        
         LTR   R7,R7                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ERRORS                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRLOCK  MVI   ERROR,SECLOCK       SECURITY LOCK OUT                            
         LA    R2,SSTUSERH                                                      
         B     ERREXIT                                                          
*                                                                               
ERRDUP   MVI   ERROR,ERDUPASS      NEW PASSWORD RECENTLY USED                   
         B     ERREXIT                                                          
*                                                                               
ERRFLD   MVI   ERROR,INVALID       INVALID WITH DISP INTO FIELD                 
         STC   R3,ERRDISP                                                       
         B     ERREXIT                                                          
*                                                                               
ERRDEL   MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMGET    DC    C'GETREC  '                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0X                  PF KEY TABLE                                 
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'STAFF   ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'STAFF2  ',CL8'DISPLAY '                               
PF14     DC    AL1(KEYTYTWA,L'SSTUSER-1),AL2(SSTUSER-T702FFD)                   
         DC    AL1(KEYTYTWA,L'SSTSTAF-1),AL2(SSTSTAF-T702FFD)                   
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR11D                                                       
         EJECT                                                                  
* LOCAL VARIABLES                                                               
SHORTNAM DS    CL25                DEFAULT SHORT NAME                           
SNLENGTH DS    F                   LENGTH OF DEFAULT SHORT NAME                 
PPBLOCK  DS    XL(6*38+1)          BLOCK FOR 2 DIRECTORY POINTERS               
MYKEY    DS    XL40                BACK UP OF KEY FOR DISPLAY KEY               
MYSTAF   DS    CL8                 BACK UP OF GLOBAL STAFF CODE                 
SPASSWRD DS    CL8                 SAVED PASSWORD                               
NUMAGY   DS    X                   NUMBER OF LIMITED AGENCIES                   
SVELEM   DS    CL(TASTLNQ)         SAVED TASTD ELEMENT                          
ADDAGY   DS    CL(L'TGAGY)                                                      
XDIR     DS    CL60                                                             
DISKA    DS    XL4                                                              
SVDISKA  DS    XL4                                                              
XWRK     DS    12D                                                              
SAVSE    DS    XL1                                                              
SVUSRID  DS    XL2                                                              
SVGRP    DS    CL8                                                              
SVSTF    DS    CL8                                                              
SKIP     DS    C                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055TAGEN11   05/29/15'                                      
         END                                                                    
