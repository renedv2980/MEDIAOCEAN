*          DATA SET SPSFM71    AT LEVEL 096 AS OF 08/25/10                      
*PHASE T21771A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21771 - MAINTENANCE/LIST OF BUYERS                                   
*                                                                               
*  COMMENTS: MAINTAINS BUYERS                                                   
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21700), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPSFM35 (T29735) -- MAINTENANCE                              
*                  SPSFM36 (T29736) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYERS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
T21771   TITLE 'SPOMS01 - MAINTENANCE OF BUYERS'                                
T21771   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21771*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 INITPFKY,DMCB,0     INITIALIZE PFKEYS                            
*                                                                               
         MVI   ACTELOPT,C'Y'       ADD ACTIVITY ELEMENT                         
*                                                                               
         CLI   ACTNUM,ACTDEL       DELETE?                                      
         BE    INVLACT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       NOW REPORT?                                  
         BNE   EXIT                NO                                           
         LA    R1,HDHOOK           A(HEADHOOK)                                  
         ST    R1,HEADHOOK         SAVE IN HEADHOOK                             
         LA    RE,HEDSPECS         A(SPECS)                                     
         ST    RE,SPECS            SAVE IN SPECS                                
         B     LR                  REPORT OUT OF LR                             
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING BYRKEY,R4                                                        
         MVI   BYRKTYP,BYRKTYPQ    TYPE                                         
         MVI   BYRKSUB,BYRKSUBQ    SUB-TYPE                                     
*                                                                               
         LA    R2,BUYMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         MVC   BYRKAM,BAGYMD                                                    
*                                                                               
         CLI   TWASCR,X'F1'                                                     
         BNE   VK05                                                             
         OI    BUYBRPFH+1,X'20'    MAKE IT PROTECTED                            
         XC    BUYBRPL,BUYBRPL                                                  
         OI    BUYBRPLH+6,X'80'                                                 
         OI    BUYBRPFH+6,X'80'                                                 
         CLI   QMED,C'R'                                                        
         BNE   VK05                                                             
         NI    BUYBRPFH+1,X'FF'-X'20'    UNPROTECT IT                           
         MVC   BUYBRPL,=C'Browse Pref (S/P)'                                    
*                                                                               
VK05     LA    R2,BUYIDH           VALIDATE BUYER ID                            
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
         B     MISSFLD             MISSING INPUT                                
*                                                                               
VK10     OC    BUYID,SPACES        BLANK PADDED                                 
         MVC   DMCB+4(4),=X'D9000ABC'   RCPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',BUYID),(X'80',WORK)                              
         BNE   INVLFLD                                                          
         MVC   BYRKBYR,BUYID                                                    
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
*                                                                               
         MVC   PREVKEY,KEY         SAVE FOR LIST SELECT                         
         MVI   PREVFLAG,1                                                       
         MVC   AIOSAVE,AIO         SAVE AIO                                     
         MVC   AIO,AIO1                                                         
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         LA    R4,SVKEY                                                         
         USING BYRKEY,R4                                                        
*                                                                               
         MVI   RDUPDATE,C'N'       READ ONLY                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DK10     BAS   RE,NEXTEL                                                        
         BNE   DKX                                                              
         CLC   BYRKAM,3(R6)                                                     
         BNE   DK10                                                             
*                                                                               
         MVC   BUYMED,2(R6)                                                     
         OI    BUYMEDH+6,X'80'     XMIT                                         
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
*                                                                               
         MVC   BUYID,BYRKBYR                                                    
         OI    BUYIDH+6,X'80'      XMIT                                         
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         OI    BUYBRPFH+1,X'20'    MAKE IT PROTECTED                            
         OI    BUYBRPLH+6,X'80'                                                 
         CLI   QMED,C'R'                                                        
         BNE   DR01                                                             
         NI    BUYBRPFH+1,X'FF'-X'20'    UNPROTECT IT                           
         MVC   BUYBRPL,=C'Browse Pref (S/P)'                                    
*                                                                               
DR01     TWAXC BUYNAMEH            MACRO CLEARS ALL UNPROTECTED FIELDS          
*                                  AND TRANSMITS THEM                           
         L     R4,AIO                                                           
         USING BYRRECD,R4                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT CODE               
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ELEMENT                            
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   BUYNAME,BYRFNAME    BUYER FULL NAME                              
*                                                                               
         MVC   BUYOFID,BYROFFID    OFFICE ID                                    
*                                                                               
         MVC   BUYTELE,BYRPHONE    TELEPHONE                                    
*                                                                               
         MVC   BUYEXT,BYRPHEXT     TELEPHONE EXTENSION                          
*                                                                               
         MVC   BUYFAX,BYRFAX       FAX NUMBER                                   
*                                                                               
         MVC   BUYBLS1,BYRBLIST    BUY LIST                                     
         MVC   BUYBLS2,BYRBLIST+3                                               
         MVC   BUYBLS3,BYRBLIST+6                                               
         MVC   BUYBLS4,BYRBLIST+9                                               
***                                                                             
* CODE FOR INBOX/SORT AND TO DO ADDED 2/15/02                                   
***                                                                             
         MVC   BUYINSR(7),SPACES   CLEAR INBOX/SORT OPTIONS FIELD               
         CLI   BYRINSRT,C'I'       IS INBOX OPTION ON?                          
         BNE   DR02                NO                                           
         MVC   BUYINSR(6),=C'INBOX='                                            
         MVC   BUYINSR+6(1),BYRINSRT+1                                          
         B     DR03                                                             
*                                                                               
DR02     CLI   BYRINSRT,C'S'       IS SORT OPTION ON?                           
         BNE   DR03                NO...DISPLAY TO DO OPTIONS                   
         MVC   BUYINSR(5),=C'SORT='                                             
         MVC   BUYINSR+5(1),BYRINSRT+1                                          
*                                                                               
DR03     LA    R1,BYRTODO          RECORD                                       
         LA    R2,BUYTODO          SCREEN                                       
         LA    R3,3                MAX 3 TO DO'S                                
*                                                                               
DR04     CLI   0(R1),X'40'         <= SPACE?                                    
         BNH   DR05                YES...DONE DISPLAYING TO DO                  
         MVC   0(1,R2),0(R1)       MOVE FROM RECORD TO SCREEN                   
         CLI   1(R1),X'40'         <= SPACE?                                    
         BNH   DR05                YES...DONE DISPLAYING TO DO                  
         CHI   R3,1                DON'T MOVE + INTO NEXT FIELD HEADER!         
         BE    *+8                                                              
         MVI   1(R2),C'+'          MOVE A PLUS IN                               
         AHI   R1,1                BUMP RECORD                                  
         AHI   R2,2                BUMP SCREEN                                  
         BCT   R3,DR04                                                          
         DROP  R6                                                               
                                                                                
DR05     L     R6,AIO                                                           
         MVI   ELCODE,BYRDCD2Q     BUYER DESCRIPTION2 ELEMENT CODE              
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   DR10                                                             
*                                                                               
         USING BYRDSCD2,R6                                                      
         XC    BUYEMAL,BUYEMAL                                                  
         MVC   BUYEMAL(L'BYREMAIL),BYREMAIL    E-MAIL                           
         MVC   BUYBRPF,BYRBRWPF                                                 
*                                                                               
         OI    BUYDACTH+1,X'0C'    ZERO INTENSITY                               
         TM    BYRMFLG1,BYRMFDAC   IS IT DEACTIVATED?                           
         BZ    DR10                 - NOPE                                      
         NI    BUYDACTH+1,X'FF'-X'04'   HIGH INTENSITY                          
         DROP  R6                                                               
***                                                                             
* DISPLAY COMMENTS                                                              
***                                                                             
DR10     L     R6,AIO                                                           
         MVI   ELCODE,BYRCMCDQ     COMMENT ELEMENT X'15'                        
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   DR20                YES                                          
                                                                                
         USING BYRCMTD,R6                                                       
         MVC   BUYSCMT,BYRSCMNT    STANDARD 8-BYTE COMMENT                      
         LA    R2,BUYOCMTH                                                      
         MVC   8(L'BUYOCMT,R2),BYROCMT1                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   8(L'BUYOCMT,R2),BYROCMT2                                         
         DROP  R6                                                               
*                                                                               
DR20     MVC   BUYCDAT,SPACES      PROTECTED FIELD TWAXC DON'T CLEAR            
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT X'F1'                       
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   DRX                 YES                                          
***                                                                             
* DATE LAST CHANGED AND WHO CHANGED IT                                          
***                                                                             
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(X'2A',BUYCDAT)                         
*        MVC   BUYCDAT+17(8),ACTVCHID                                           
         OI    BUYCDATH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         OI    GENSTAT2,NEXTSEL                                                 
         L     R4,AIO              A(BUYER RECORD)                              
         USING BYRKEY,R4                                                        
*                                                                               
         LA    R2,BUYNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,BUYOFIDH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,BUYTELEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         BAS   RE,VALNUM           VALIDATE PHONE NUMBER                        
*                                                                               
         LA    R2,BUYEXTH          OPTIONAL TELEPHONE EXTENSION                 
         CLI   5(R2),0                                                          
         BE    VR04                                                             
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
*                                                                               
VR04     LA    R2,BUYFAXH          FAX OPTIONAL                                 
         CLI   5(R2),0                                                          
         BE    VR05                                                             
*                                                                               
         BAS   RE,VALNUM           VALIDATE FAX                                 
***                                                                             
* MUST INTERRUMPT CURRENT ELEMENT TO GET E-MAIL IN ANOTHER ELEMENT              
* VALIDATE E-MAIL SO VALIDATION CAN OCCUR IN THE ORDER SEEN ON SCREEN           
***                                                                             
VR05     LA    R2,BUYEMALH         BUYER'S E-MAIL                               
         CLI   5(R2),0                                                          
         BE    MISSFLD             REQUIRED!                                    
         BAS   RE,VALEMAIL         VALIDATE IT                                  
*                                                                               
VR06     MVC   MEBUYER,BYRKBYR                                                  
         MVC   SAVEKEY2,KEY                                                     
         MVC   AIOSAVE,AIO         CHECK IF ASSISTANT BUYER EXISTS              
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R3,4                MAX BUYER LIST                               
         LA    R2,BUYBLS1H         BUYER LIST                                   
         LA    R7,BUYLIST          TEMP BUY LIST TO CHECK FOR DUPS              
         XC    BUYLIST,BUYLIST                                                  
VR10     ZICM  R1,5(R2),1          ANY INPUT?                                   
         BNZ   VR20                YES                                          
VR10NXT  ZIC   R0,0(R2)            BUMP TO NEXT BUYER INPUT LINE                
         AR    R2,R0                                                            
         BCT   R3,VR10                                                          
         B     VR25                                                             
*                                                                               
VR20     OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),MEBUYER     BUYER CANNOT BE ITS ASST. BUYER              
         BE    INVLFLD                                                          
*                                  CHECK IF ASSISTANT BUYER EXISTS              
         LA    R4,KEY              A(BUYER RECORD)                              
         USING BYRKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BYRKEY),SAVEKEY2                                           
         MVC   BYRKBYR,8(R2)                                                    
         OC    BYRKBYR,SPACES      BLANK PADDED                                 
         MVC   0(3,R7),BYRKBYR                                                  
         LA    R7,3(R7)                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BNE   INVLBYR             DOES NOT EXIST                               
         B     VR10NXT                                                          
*                                                                               
VR25     MVC   AIO,AIOSAVE         RESTORE AIO                                  
         MVC   KEY,SAVEKEY2                                                     
         DROP  R4                                                               
***                                                                             
* CHECK FOR DUPLICATE BUYERS                                                    
* COMPARES = {(N-1)*N}/2  ALGORITHM                                             
***                                                                             
         LA    R1,BUYLIST                                                       
         LA    R2,BUYLIST+3                                                     
         LA    R7,BUYLIST+L'BUYLIST-1                                           
*                                                                               
VR26     CLC   0(3,R2),SPACES       BLANK ENTRY?                                
         BNH   VR29                 YES                                         
         CLC   0(3,R1),0(R2)        DUPLICATE BUYER?                            
         BNE   VR29                 NO                                          
***                                                                             
* DUPLICATE BUYER....ERROR, POINT CURSOR AT SECOND DUP FROM LEFT                
***                                                                             
         XC    DUPFLAG,DUPFLAG                                                  
         LR    R1,R2                                                            
         OC    0(3,R1),SPACES                                                   
         LA    R2,BUYBLS1H                                                      
*                                                                               
VR27     OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),0(R1)                                                    
         BNE   VR28                                                             
         TM    DUPFLAG,X'01'                                                    
         BO    ERRBLIST                                                         
         OI    DUPFLAG,X'01'                                                    
VR28     ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         B     VR27                                                             
*                                                                               
VR29     LA    R2,3(R2)             BUMP 2ND PTR                                
         CR    R2,R7                LIMIT?                                      
         BL    VR26                 NO                                          
         LA    R1,3(R1)             REACHED LIMIT...RESET                       
         LA    R2,3(R1)                                                         
         CR    R2,R7                                                            
         BL    VR26                                                             
***                                                                             
* FINISHED ALL VALIDATION...NOW BUILD AND ADD RECORDS                           
***                                                                             
VR35     L     R4,AIO              A(BUYER RECORD)                              
         USING BYRKEY,R4                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VR39                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT CODE               
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR39     LA    R6,ELEM                                                          
         USING BYRDSCD,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   BYRDCDE,BYRDCDQ     BUYER DESCRIPTION ELEMENT CODE               
         MVI   BYRDSLN,BYRDSLQ     BUYER DESCRIPTION ELEMENT LENGTH             
         MVC   BYRFNAME,BUYNAME    BUYER FULL NAME                              
         MVC   BYROFFID,BUYOFID    OFFICE ID                                    
         MVC   BYRPHONE,BUYTELE    TELEPHONE                                    
         MVC   BYRPHEXT,BUYEXT     TELEPHONE EXTENSION                          
         MVC   BYRFAX,BUYFAX       FAX NUM                                      
*                                                                               
         LA    R3,4                BUYER LIST                                   
         LA    R2,BUYBLS1H                                                      
         LA    R4,BYRBLIST                                                      
VR40A05  CLI   5(R2),0                                                          
         BE    VR40A10                                                          
         MVC   0(3,R4),8(R2)                                                    
         LA    R4,3(R4)                                                         
VR40A10  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R3,VR40A05                                                       
         OC    BYRBLIST,SPACES                                                  
***                                                                             
* CODE TO VALIDATE INBOX/SORT AND TO DO OPTIONS ADDED 2/15/02                   
***                                                                             
         LA    R1,BUYINSRH          INBOX/SORT FILED HEADER                     
         LR    R2,R1                POINT CURSOR IN CASE OF ERROR               
         CLI   5(R1),0              ANY INPUT?                                  
         BE    VR41                 NO                                          
         CLC   =C'INBOX=',8(R1)     OPTION INBOX?                               
         BNE   VR40A                NO                                          
         LA    R1,14(R1)            POINT 1 BYTE PAST = SIGN                    
         B     VR40B                VALIDATE OPTION                             
*                                                                               
VR40A    CLC   =C'SORT=',8(R1)      OPTION SORT?                                
         BNE   ERROPT1              NO...ERROR                                  
         CLI   14(R2),X'40'         ILLEGAL INPUT?                              
         BH    ERROPT1              YES                                         
         LA    R1,13(R1)            POINT 1 BYTE PAST = SIGN                    
*                                                                               
VR40B    MVC   BYRINSRT(1),BUYINSR  INBOX/SORT                                  
         CLI   0(R1),C'A'           OPTION A?                                   
         BE    VR40C                YES                                         
         CLI   0(R1),C'B'           OPTION B?                                   
         BE    VR40C                YES                                         
         CLI   0(R1),C'C'           OPTION C?                                   
         BE    VR40C                YES                                         
         CLI   0(R1),C'D'           OPTION D?                                   
         BNE   ERROPT1              NO...ERROR                                  
VR40C    MVC   BYRINSRT+1(1),0(R1)                                              
***                                                                             
* VALIDATE TO DO                                                                
***                                                                             
VR41     LA    R1,BUYTODOH          TO DO FIELD HEADER                          
         LR    R2,R1                POINT CURSOR IN CASE OF ERROR               
         CLI   5(R1),0              ANY INPUT?                                  
         BE    VR49                 NO...SKIP TO DO VALIDATION                  
         XC    TODOFLAG,TODOFLAG    NO DUPLICATE TO DO ENTRIES                  
         ZIC   R3,5(R1)             INPUT LENGTH                                
         LA    R1,8(R1)             TO DO FIELD                                 
         LA    R7,0(R1,R3)          TO DO FIELD + LENGTH                        
         LA    R3,BYRTODO           TO HOLD OPTIONS                             
*                                                                               
VR42A    CLI   0(R1),C'B'           IS TO DO OPTION B?                          
         BNE   VR42B                NO                                          
         TM    TODOFLAG,TODOOPTB    ALREADY HAD 'B' OPTION?                     
         BO    ERRDUPOP             YES...ERROR                                 
         OI    TODOFLAG,TODOOPTB    NO...TURN ON 'B' OPTION                     
         MVI   0(R3),C'B'           STORE IT IN RECORD                          
         B     VR42CA                                                           
VR42B    CLI   0(R1),C'C'           IS TO DO OPTION C?                          
         BNE   VR42C                NO                                          
         TM    TODOFLAG,TODOOPTC    ALREADY HAD 'C' OPTION?                     
         BO    ERRDUPOP             YES...ERROR                                 
         OI    TODOFLAG,TODOOPTC    NO...TURN ON 'C' OPTION                     
         MVI   0(R3),C'C'           STORE IT IN RECORD                          
         B     VR42CA                                                           
VR42C    CLI   0(R1),C'S'           IS TO DO OPTION S?                          
         BNE   ERRTODO              NO...ERROR                                  
         TM    TODOFLAG,TODOOPTS    ALREADY HAD 'S' OPTION?                     
         BO    ERRDUPOP             YES...ERROR                                 
         OI    TODOFLAG,TODOOPTS    NO...TURN ON 'S' OPTION                     
         MVI   0(R3),C'S'           STORE IT IN RECORD                          
*                                                                               
VR42CA   AHI   R3,1                 BUMP RECORD PTR                             
         CLI   1(R1),X'40'          MORE DATA?                                  
         BNH   VR49                 NO                                          
         CLI   1(R1),C'+'           MUST BE + BETWEEN OPTIONS                   
         BNE   ERRPLUS                                                          
         LA    R1,2(R1)             BUMP FIELD                                  
         CR    R1,R7                MAX DATA?                                   
         BL    VR42A                NO                                          
         DROP  R6                                                               
*                                                                               
VR49     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR50                                                             
         DC    H'0'                                                             
*                                                                               
VR50     CLI   ACTNUM,ACTADD                                                    
         BE    VR60                                                             
*                                                                               
         MVI   ELCODE,BYRDCD2Q     DESCRIPTION ELEMENT CONTINUED X'11'          
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR60     LA    R6,ELEM                                                          
         USING BYRDSCD2,R6                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   BYRDCDE2,BYRDCD2Q   BUYER DESCRIPTION ELEMENT CODE               
         MVI   BYRDSLN2,BYRDSLQ2   BUYER DESCRIPTION ELEMENT LENGTH             
         MVC   BYREMAIL,BUYEMAL    BUYER E-MAIL                                 
*                                                                               
* VALIDATE BROWSE PREFERENCE                                                    
*                                                                               
VR65     CLI   QMED,C'R'                                                        
         BNE   VR69                                                             
         LA    R2,BUYBRPFH                                                      
         CLI   5(R2),0                                                          
         BE    VR69                                                             
         CLI   8(R2),C'S'                                                       
         BE    VR65B                                                            
         CLI   8(R2),C'P'                                                       
         BNE   INVLFLD                                                          
VR65B    MVC   BYRBRWPF,8(R2)                                                   
         DROP  R6                                                               
*                                                                               
VR69     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR70                                                             
         DC    H'0'                                                             
*                                                                               
***                                                                             
* VALIDATE THE STANDARD COMMENTS                                                
***                                                                             
VR70     LA    R2,BUYSCMTH          STANDARD COMMENT                            
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR71                 NO                                          
         GOTO1 VALISCOM,DMCB,8(R2)  VALIDATE STANDARD COMMENT                   
         BNE   INVLFLD                                                          
VR71     CLI   ACTNUM,ACTADD                                                    
         BE    VR80                                                             
*                                                                               
         MVI   ELCODE,BYRCMCDQ     COMMENT ELEMENT X'15'                        
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR80     LA    R6,ELEM                                                          
         USING BYRCMTD,R6                                                       
*                                                                               
         LA    R2,BUYSCMTH         SCREEN STANDARD COMMENT HEADER               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR82                YES                                          
         LA    R2,BUYOCMTH         SCREEN ORDER COMMENT LINE 1 HEADER           
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR82                YES                                          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               SCREEN ORDER COMMENT LINE 2 HEADER           
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VRX                 YES                                          
*                                                                               
VR82     XC    ELEM,ELEM                                                        
         MVI   BYRCMCD,BYRCMCDQ    COMMENT ELEMENT CODE                         
         MVI   BYRCMLN,BYRCMTLQ    COMMENT ELEMENT LENGTH                       
         MVC   BYRSCMNT,BUYSCMT    STANDARD COMMENT                             
         OC    BYRSCMNT,SPACES     SPACE PADDED                                 
         LA    R2,BUYOCMTH                                                      
         MVC   BYROCMT1(L'BUYOCMT),8(R2)  ORDER COMMENT LINE1                   
         OC    BYROCMT1,SPACES                                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   BYROCMT2(L'BUYOCMT),8(R2)  ORDER COMMENT LINE2                   
         OC    BYROCMT2,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VRX                                                              
         DC    H'0'                                                             
                                                                                
VRX      B     DR                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          VALEMAIL                                   *         
***********************************************************************         
VALEMAIL NTR1                                                                   
*                                                                               
         LA    R2,BUYEMALH                                                      
         ZIC   R1,5(R2)                                                         
         LA    R3,BUYEMAL                                                       
         AR    R1,R3                                                            
*                                                                               
         CLI   0(R3),C'@'           FIRST CHAR CANNOT BE @                      
         BE    EMLERR0                                                          
*                                                                               
ATLOOP   AHI   R3,1                                                             
         CR    R3,R1                ARE WE PAST THE END OF E-MAIL?              
         BNL   EMLERR1              YES                                         
         CLI   0(R3),C'@'           ARE WE POINTING TO AN '@'?                  
         BNE   ATLOOP               NO                                          
*                                                                               
         AHI   R3,1                                                             
DOTLOOP  AHI   R3,1                                                             
         CR    R3,R1                ARE WE PAST THE END OF E-MAIL?              
         BNL   EMLERR2              YES                                         
         CLI   0(R3),C'@'           ARE WE POINTING TO AN '@'?                  
         BE    EMLERR3              YES...ERROR                                 
         CLI   0(R3),C'.'           ARE WE POINTING TO AN '.'?                  
         BNE   DOTLOOP              NO                                          
*                                                                               
         AHI   R3,1                                                             
         CR    R3,R1                ARE WE PAST THE END OF E-MAIL?              
         BNL   EMLERR4              YES..MISSING TOP LEVEL DOMAIN               
DOMAIN   AHI   R3,1                 BUMP UP R2 CHAR CHECKED LATER               
         CLI   0(R3),C'@'           ARE WE POINTING TO AN '@'?                  
         BE    EMLERR3              YES...ERROR                                 
         CR    R3,R1                ARE WE PAST THE END OF E-MAIL?              
         BNL   EMAILOK              YES..EMAIL IS OK                            
         B     DOMAIN               NO                                          
*                                                                               
EMAILOK  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       LA    R4,KEY                                                           
         USING BYRKEY,R4                                                        
*                                                                               
         CLI   PREVFLAG,0          FIRST TIME THRU??                            
         BE    LR05                                                             
*                                                                               
         MVC   KEY,PREVKEY                                                      
         XC    PREVFLAG,PREVFLAG                                                
         B     LR10                                                             
*                                                                               
LR05     MVI   BYRKTYP,BYRKTYPQ    TYPE      '0D'                               
         MVI   BYRKSUB,BYRKSUBQ    SUB-TYPE  '31'                               
         MVC   BYRKAM,BAGYMD                                                    
*                                                                               
LR10     MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(3),SAVEKEY      SAME TYPE, A/M ?                             
         BNE   LRX                                                              
         CLC   KEY+6(7),=7X'00'    BUYERS RECORDS ONLY                          
         BNE   LR60                                                             
*                                                                               
         OC    BUYID,BUYID         FILTER ON BUYER??                            
         BZ    LR40                                                             
         ZIC   R1,BUYIDH+5         YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BYRKBYR(0),BUYID    FILTER ON BUYER                              
         BNE   LR60                                                             
*                                                                               
LR40     GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LBYBUYID,BYRKBYR    MOVE IN BUYER ID                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCD2Q     BUYER DESCRIPTION 2 ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   LR40E                                                            
*                                                                               
         USING BYRDSCD2,R6                                                      
         TM    BYRMFLG1,BYRMFDAC   IS IT DEACTIVATED?                           
         BZ    LR40E                - NOPE                                      
         MVI   LBYDACT,C'*'         - YUP                                       
*                                                                               
LR40E    L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               MUST BE THERE                                
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   LBYNAME,BYRFNAME    BUYER FULL NAME                              
         MVC   LBYOFID,BYROFFID    BUYER OFFICE ID                              
         MVC   LBYTELE,BYRPHONE    BU ER TELEPHONE                              
*                                                                               
         LA    R3,LBYBLIST         LIST SCREEN                                  
         LA    R2,BYRBLIST         REC                                          
         LA    R4,4                                                             
         B     *+12                                                             
*                                                                               
LR41     MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         MVC   0(3,R3),0(R2)       BUY LIST                                     
         CLI   2(R2),X'40'         2 CHAR BUYER?                                
         BNH   *+8                 YES...BUMP SCREEN BY 2, NOT 3                
         AHI   R3,1                                                             
         AHI   R3,2                                                             
         AHI   R2,3                                                             
*                                                                               
         CLI   0(R2),X'40'         DONE?                                        
         BNH   LR55                YES                                          
         BCT   R4,LR41                                                          
*                                                                               
         DROP  R6                                                               
LR55     CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR56                NO                                           
         XC    LBYBLIST,LBYBLIST   CLEAR BUYER LIST                             
         MVC   P,LISTAR            MOVE TO PRINTLINE                            
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT TO REPORT                              
         B     LR60                                                             
*                                                                               
LR56     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
         B     LR20                                                             
         DROP  R4                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                             VALNUM                                  *         
***********************************************************************         
VALNUM   NTR1                                                                   
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=--'                                  
         CLI   4(R1),3                                                          
         BNE   INVLFLD                                                          
         CLI   0(R3),3             AREA CODE                                    
         BNE   INVLFLD                                                          
         CLI   32(R3),3            PREFIX                                       
         BNE   INVLFLD                                                          
         CLI   64(R3),4            SUBFIX                                       
         BNE   INVLFLD                                                          
*                                                                               
         TM    2(R3),X'80'         ALL MUST BE NUMERIC                          
         BZ    INVLFLD                                                          
         TM    34(R3),X'80'                                                     
         BZ    INVLFLD                                                          
         TM    66(R3),X'80'                                                     
         BZ    INVLFLD                                                          
*                                                                               
         B     EXIT                                                             
RELO     DS    A                                                                
***********************************************************************         
* VALISCOM                                                                      
*        THIS ROUTINE VALIDATES IF THE STANDARD COMMENT EXISTS.                 
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED.                                            
*                                                                               
* ON ENTRY:    P1                   A(STANDARD COMMENT)                         
***********************************************************************         
VALISCOM NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING COMRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCOM,0(R2)                                                    
         OC    COMKCOM,SPACES                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'COMKEY),KEYSAVE                                            
         BE    VSCOM50                                                          
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
***      B     NO                  RETURN 'NO' TO CALLER                        
         LTR   RC,RC                                                            
         B     VSCOMX                                                           
*                                                                               
VSCOM50  XC    KEY,KEY             RESTORE KEY BEING USED                       
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
***      B     YES                 RETURN 'YES' TO CALLER                       
         SR    RC,RC                                                            
VSCOMX   B     EXIT                                                             
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+7(L'QMED),QMED                                                
         MVC   H4+11(L'MEDNM),MEDNM                                             
HEDX     B     EXIT                                                             
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H4,1,C'Media:'                                                   
*                                                                               
         SSPEC H6,1,C'Buyer'                                                    
         SSPEC H7,1,C'Id'                                                       
         SSPEC H8,1,C'-----'                                                    
*                                                                               
         SSPEC H7,7,C'Full name'                                                
         SSPEC H8,7,C'-------------------------'                                
*                                                                               
         SSPEC H6,33,C'Off'                                                     
         SSPEC H7,33,C'Id'                                                      
         SSPEC H8,33,C'---'                                                     
*                                                                               
         SSPEC H7,37,C'Telephone'                                               
         SSPEC H8,37,C'------------'                                            
         DC    X'00'                                                            
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERROPT1  MVC   ERRNUM,=AL2(897)     OPTIONS ARE SORT OR INBOX                   
         B     SPERREX                                                          
ERRTODO  MVC   ERRNUM,=AL2(906)     VALID OPTS ARE B,C,S                        
         B     SPERREX                                                          
ERRDUPOP MVC   ERRNUM,=AL2(907)     DUPLICATE OPTION                            
         B     SPERREX                                                          
ERRPLUS  MVC   ERRNUM,=AL2(908)     MUST HAVE + SIGN BETWEEN TO DO OPT          
         B     SPERREX                                                          
ERRBLIST MVC   ERRNUM,=AL2(912)     DUPLICATE ENTRY IN BUYER LIST               
         B     SPERREX                                                          
INVLBYR  MVC   ERRNUM,=AL2(950)     BUYER NOT FOUND                             
         B     SPERREX                                                          
EMLERR0  MVC   ERRNUM,=AL2(958)     E-MAIL MISSING PREFIX                       
         B     SPERREX                                                          
*                                                                               
EMLERR1  MVC   ERRNUM,=AL2(954)     E-MAIL MISSING @ SIGN                       
         B     SPERREX                                                          
*                                                                               
EMLERR2  MVC   ERRNUM,=AL2(955)     E-MAIL MISSING DOMAIN PREFIX                
         B     SPERREX                                                          
*                                                                               
EMLERR3  MVC   ERRNUM,=AL2(956)     E-MAIL CONTAINS MORE THAN 1 @ SIGN          
         B     SPERREX                                                          
*                                                                               
EMLERR4  MVC   ERRNUM,=AL2(957)     E-MAIL MISSING DOMAIN EXTENSION             
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     ERREXIT                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPOMSDSCTS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT (CHANGED DATE)              
       ++INCLUDE SPSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM35D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
**     ++INCLUDE SPOMSE1D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE SCSFM36D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD          ERROR MSGS                                  
         EJECT                                                                  
**     ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
       ++INCLUDE SPSFMWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
         PRINT ON                                                               
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
AIOSAVE  DS    A                                                                
SAVEKEY  DS    XL48                                                             
SAVEKEY2 DS    XL48                                                             
PREVKEY  DS    XL48                SAVE FOR LIST SELECT                         
PREVFLAG DS    XL1                                                              
MEBUYER  DS    CL3                                                              
ERRNUM   DS    CL2                 FOR ERRORS                                   
BUYLIST  DS    CL12                TO CHECK FOR DUPLICATE BUYERS                
TODOFLAG DS    X                   NO DUPLICATE TO DO OPTIONS ALLOWED           
TODOOPTB EQU   X'01'               B OPTION FLAGGED                             
TODOOPTC EQU   X'02'               C OPTION FLAGGED                             
TODOOPTS EQU   X'04'               S OPTION FLAGGED                             
DUPFLAG  DS    X                   FOR DUPLICATE BUYER LISTS                    
         SPACE                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LBYBUYID DS    CL3                                                              
         DS    CL1                                                              
LBYDACT  DS    CL1                                                              
         DS    CL1                                                              
LBYNAME  DS    CL24                                                             
         DS    CL2                                                              
LBYOFID  DS    CL2                                                              
         DS    CL2                                                              
LBYTELE  DS    CL12                                                             
         DS    CL4                                                              
LBYBLIST DS    CL15                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPSFM71   08/25/10'                                      
         END                                                                    
