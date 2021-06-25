*          DATA SET SPADD01    AT LEVEL 067 AS OF 05/01/02                      
*PHASE T21201A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21201 - MAINTENANCE/LIST OF BUYERS                                   
*                                                                               
*  COMMENTS: MAINTAINS BUYERS                                                   
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDE1 (T212E1) -- MAINTENANCE                              
*                  SPADDF1 (T212F1) -- LIST                                     
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
         TITLE 'SPADD01 MAINTENANCE OF BUYERS'                                  
T21201   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21201*,R7,RR=R3                                              
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
         GOTO1 INITIAL,DMCB,0      INITIALIZE PFKEYS                            
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
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
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
         LA    R2,BUYIDH           VALIDATE BUYER ID                            
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
         B     MISSFLD             MISSING INPUT                                
*                                                                               
VK10     OC    BUYID,SPACES        BLANK PADDED                                 
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
         L     R4,AIO                                                           
         USING BYRRECD,R4                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT CODE               
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BE    *+6                                                              
         DC    H'00'               MUST HAVE ELEMENT                            
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   BUYNAME,BYRFNAME                                                 
         OI    BUYNAMEH+6,X'80'    BUYER FULL NAME                              
*                                                                               
         MVC   BUYOFID,BYROFFID                                                 
         OI    BUYOFIDH+6,X'80'    OFFICE ID                                    
*                                                                               
         MVC   BUYTELE,BYRPHONE                                                 
         OI    BUYTELEH+6,X'80'    TELEPHONE                                    
*                                                                               
         MVC   BUYEXT,BYRPHEXT                                                  
         OI    BUYEXTH+6,X'80'     TELEPHONE EXTENSION                          
*                                                                               
         MVC   BUYASST,BYRASBYR                                                 
         OI    BUYASSTH+6,X'80'    ASSISTANT BUYER                              
         DROP  R6                                                               
*                                                                               
         LA    R2,BUYMKG1H         MARKET GROUP                                 
         TWAXC BUYMKG1H,BUYMKG4H                                                
         L     R6,AIO                                                           
         MVI   ELCODE,BYRSMCDQ     SPOTPAK MARKET GROUP ELEMENT CODE            
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   DR20                NO, GO DISPLAY MARKET LIST                   
*                                                                               
         USING BYRSMGD,R6          YES, DISPLAY THE SPOTPAK MKT GRPS            
DR10     DS    0H                                                               
         MVC   8(1,R2),BYRSMKGP                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),BYRSMKGP+1                                             
         L     R1,FULL                                                          
         SLL   R1,4                                                             
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        PWOS -> P                                    
         EDIT  (P3,FULL+1),(4,9(R2)),ALIGN=LEFT                                 
         OI    6(R2),X'80'         DISPLAY MKT GRP                              
*                                                                               
         BAS   RE,NEXTEL           NEXT SPOTPAK MKT GRP                         
         BNE   DR20                NO MORE??                                    
*                                                                               
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,BUYMKG4H         ROOM FOR 4 MARKET GROUPS ONLY                
         CR    R2,RF                                                            
         BNH   DR10                                                             
         DROP  R6                                                               
*                                                                               
DR20     DS    0H                                                               
         TWAXC BUYMLSH,BUYMLSLH,PROT=Y                                          
         L     R6,AIO                                                           
         USING BYRMKAD,R6                                                       
         MVI   ELCODE,BYRMKAEQ     MARKET ASSIGNMENT ELEMENT CODE               
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   DRX                 NO, DONE                                     
*                                                                               
DR30     DS    0H                                                               
         LA    R2,BUYMLSH          MARKET LIST                                  
         LA    R3,BUYMLS           R3 POINTS TO DATA FIELD                      
*                                                                               
DR40     DS    0H                                                               
         EDIT  (B2,BYRMKANO),(4,(R3)),FILL=0                                    
*                                                                               
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   DR50                NO MORE??                                    
*                                                                               
         LA    R3,6(R3)            NEXT FIELD                                   
         LA    RF,BUYMLSLH                                                      
         LA    RF,68(RF)                                                        
         CR    R3,RF               LIST TO LAST ROW YET??                       
         BNL   DR45                                                             
*                                                                               
         LA    RF,68(R2)           SHOULD BE 10 MARKETS PER LINE                
         CR    R3,RF               LIST TO END OF LINE YET??                    
         BL    DR40                                                             
*                                                                               
         OI    6(R2),X'80'         DISPLAY MKT GRP                              
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
         LA    R3,8(R2)                                                         
         B     DR40                                                             
*                                                                               
DR45     DS    0H                                                               
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   DR50                NO MORE??                                    
         MVC   0(6,R3),=C'(MORE)'                                               
*                                                                               
DR50     DS    0H                                                               
         OI    6(R2),X'80'         DISPLAY MKT GRP                              
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
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
         LA    R2,BUYEXTH          OPTIONAL TELEPHONE EXTENSION                 
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
*                                                                               
VR20     LA    R2,BUYASSTH         OPTIONAL                                     
         CLI   5(R2),0                                                          
         BE    VR30                                                             
*                                                                               
         CLC   BYRKBYR,BUYASST     BUYER CANNOT BE ITS ASST. BUYER              
         BE    INVLFLD                                                          
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         MVC   AIOSAVE,AIO         CHECK IF ASSISTANT BUYER EXISTS              
         MVC   AIO,AIO1                                                         
         LA    R4,KEY              A(BUYER RECORD)                              
         USING BYRKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BYRKEY),SAVEKEY2                                           
         MVC   BYRKBYR,BUYASST                                                  
         OC    BYRKBYR,SPACES      BLANK PADDED                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BNE   INVLBYR             DOES NOT EXIST                               
         MVC   AIO,AIOSAVE         RESTORE AIO                                  
         MVC   KEY,SAVEKEY2                                                     
         DROP  R4                                                               
*                                                                               
VR30     DS    0H                                                               
         L     R4,AIO              A(BUYER RECORD)                              
         USING BYRKEY,R4                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VR40                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT CODE               
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR40     LA    R6,ELEM                                                          
         USING BYRDSCD,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   BYRDCDE,BYRDCDQ     BUYER DESCRIPTION ELEMENT CODE               
         MVI   BYRDSLN,BYRDSLQ     BUYER DESCRIPTION ELEMENT LENGTH             
         MVC   BYRFNAME,BUYNAME                                                 
         MVC   BYROFFID,BUYOFID                                                 
         MVC   BYRPHONE,BUYTELE                                                 
         MVC   BYRPHEXT,BUYEXT                                                  
         MVC   BYRASBYR,BUYASST                                                 
         OC    BYRASBYR,SPACES     BLANK PADDED                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR50                                                             
         DC    H'0'                                                             
*                                                                               
VR50     CLI   ACTNUM,ACTADD                                                    
         BE    VR60                                                             
*                                                                               
         MVI   ELCODE,BYRSMCDQ     SPOTPAK MKT GRP ELEMENT CODE                 
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR60     LA    R2,BUYMKG1H         POINT TO FIRST MARKET GROUP                  
*                                                                               
VR70     CLI   5(R2),0             BUMP NEXT IF NO ENTRY                        
         BE    VR80                                                             
*                                                                               
*        MKTGRP MUST BE A LETTER FOLLOWED BY A 1 TO 4 DIGIT NUMBER              
*                                                                               
         CLI   5(R2),2             LENGTH MUST BE AT LEAST 2                    
         BL    INVLFLD                                                          
*                                                                               
         CLI   5(R2),5             LENGTH MUST BE NO MORE THAN 5                
         BH    INVLFLD                                                          
*                                                                               
         CLI   8(R2),C'A'          FIRST CHARACTER MUST BE ALPHABETIC           
         BL    INVLFLD                                                          
*                                                                               
         CLI   8(R2),C'Z'          FIRST CHARACTER MUST BE ALPHABETIC           
         BH    INVLFLD                                                          
*                                                                               
         CLI   8(R2),C'}'          FIRST CHARACTER MUST BE ALPHABETIC           
         BE    INVLFLD                                                          
*                                                                               
         CLI   8(R2),C'\'          FIRST CHARACTER MUST BE ALPHABETIC           
         BE    INVLFLD                                                          
*                                                                               
         ZIC   RF,5(R2)            RF POINTS TO END OF FIELD                    
         LA    RF,7(RF,R2)                                                      
         LA    R1,9(R2)                                                         
*                                                                               
VR75     CLI   0(R1),C'0'          NEXT 1-4 CHARACTERS MUST BE NUMERIC          
         BL    INVLFLD                                                          
         CLI   0(R1),C'9'                                                       
         BH    INVLFLD                                                          
         LA    R1,1(R1)                                                         
         CR    R1,RF               ARE WE AT THE END YET?                       
         BNH   VR75                                                             
*                                                                               
         LA    R6,ELEM                                                          
         USING BYRSMGD,R6                                                       
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   BYRSMCDE,BYRSMCDQ   SPOTPAK MKTGRP ELEMENT CODE                  
         MVI   BYRSMGLN,BYRSMGLQ   SPOTPAK MKTGRP ELEMENT LENGTH                
         MVC   BYRSMKGP(1),8(R2)   MOVE IN LETTER                               
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                MINUS LENGTH OF FIRST LETTER                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  FULL(4),9(0,R2)                                                  
         ICM   R1,15,FULL                                                       
         SRL   R1,4                GET RID OF THE SIGN                          
         STCM  R1,3,BYRSMKGP+1                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR80                                                             
         DC    H'0'                                                             
*                                                                               
VR80     ZIC   R0,0(R2)            BUMP TO NEXT SPOTPAK MKTGRP FIELD            
         AR    R2,R0                                                            
         LA    RF,BUYMKG4H         ALL 4 MKTGRP FIELDS CHECKED??                
         CR    R2,RF                                                            
         BNH   VR70                                                             
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
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
*                                                                               
LR40     GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         MVC   LBYBUY,BYRKBYR      MOVE IN BUYER                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      BUYER DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               MUST BE THERE                                
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   LBYNAME,BYRFNAME    BUYER FULL NAME                              
         MVC   LBYOFID,BYROFFID    BUYER OFFICE ID                              
         MVC   LBYTELE,BYRPHONE    BUYER TELEPHONE                              
         MVC   LBYASST,BYRASBYR     ASSISTANT BUYER                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRSMCDQ     SPOTPAK MARKET GRP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   LR55                                                             
*                                                                               
         USING BYRSMGD,R6                                                       
         MVC   LBYMKG(1),BYRSMKGP  SPOTPAK MKTGRP                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),BYRSMKGP+1                                             
         L     R1,FULL                                                          
         SLL   R1,4                                                             
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        PWOS -> P                                    
         EDIT  (P3,FULL+1),(4,LBYMKG+1),ALIGN=LEFT                              
         DROP  R6                                                               
*                                                                               
LR55     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
         B     LR20                                                             
         DROP  R4                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLBYR  MVI   GERROR1,NOASSBYR    ASSISTANT BUYER NOT FOUND                    
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPADDFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF1D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDE1D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPADDWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
AIOSAVE  DS    A                                                                
SAVEKEY  DS    XL48                                                             
SAVEKEY2 DS    XL48                                                             
PREVKEY  DS    XL48                SAVE FOR LIST SELECT                         
PREVFLAG DS    XL1                                                              
         SPACE                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LBYBUY   DS    CL3                                                              
         DS    CL3                                                              
LBYNAME  DS    CL24                                                             
         DS    CL2                                                              
LBYOFID  DS    CL2                                                              
         DS    CL2                                                              
LBYTELE  DS    CL12                                                             
         DS    CL4                                                              
LBYASST  DS    CL3                                                              
         DS    CL4                                                              
LBYMKG   DS    CL10                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067SPADD01   05/01/02'                                      
         END                                                                    
