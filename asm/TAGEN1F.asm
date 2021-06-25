*          DATA SET TAGEN1F    AT LEVEL 025 AS OF 05/29/15                      
*PHASE T7021FE,*                                                                
         TITLE 'T7021F - CAST VERIFY'                                           
T7021F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021F                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING WORKD,R7            WORKD IS IN TAGEN1AD                         
*                                  TAGEN18 USES TWAHOLE+X'C80'                  
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
                                                                                
         OI    SCLVERLH+1,X'0C'    DON'T SHOW VERS FIELD                        
         OI    SCLVERH+1,X'0C'                                                  
         OI    SCLVERH+1,X'20'                                                  
         NI    SCLVERH+4,X'DF'                                                  
         XC    SCLVER,SCLVER       AND CLEAR PREVIOUS INPUT                     
         OI    SCLVERLH+6,X'80'                                                 
         OI    SCLVERH+6,X'80'                                                  
*                                                                               
         MVI   BYTE,X'40'                                                       
         CLI   SVACT,ACTVER        IF RECORD ACTION CHANGED                     
         BNE   COM10                                                            
         CLI   SVREC,CA            FROM CAST VERIFY                             
         BE    COM20                                                            
*                                                                               
COM10    MVI   CVERSTAT,0          CLEAR STORAGE                                
         MVI   CVERSTA2,0                                                       
         MVI   BYTE,0                                                           
*                                                                               
COM20    GOTO1 INITIAL,DMCB,(BYTE,0) ELSE FORCE NOT TO CLEAR W/S                
*                                                                               
         TM    CVERSTAT,CVERSCOM   IF COM NOT DISPLAYED                         
         BO    COM30                  TEST IF VALKEY                            
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
*                                                                               
COM30    TM    CVERSTAT,CVERSCOV   ELSE TEST IF COM VALID                       
         BNO   COM40                                                            
         OI    TRNSTAT,OKINTPFK    SET PF23 OK TO VERIFY                        
*                                                                               
COM40    B     VER                 GOTO VALREC                                  
*                                                                               
COMX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R2,SCLAGYH                                                       
         CLI   SCRSTAT,0                                                        
         BNE   *+12                                                             
         TM    4(R2),X'20'         WAS IT VALIDATED                             
         BO    VK05                                                             
         NI    SCLCIDH+4,X'DF'     SET NEXT FIELD INVALID                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK05     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SCLCIDH                                                       
         TM    4(R2),X'20'         IF IT WAS VALIDATED                          
         BO    VER                 GO STRAIGHT TO VALREC                        
*                                                                               
         OI    4(R2),X'20'         VALIDATED                                    
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'28',SCLCIDH),SCLCIDNH                    
*                                                                               
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   VK07                                                             
         L     R4,TGELEM           ENSURE COMMERCIAL DOES NOT HAVE              
         CLC   =C'VC',TAFNNAME     VITA COMPLETIONS LOCK                        
         BE    ERUWB                                                            
         CLC   =C'RC',TAFNNAME                                                  
         BE    ERUWB                                                            
         DROP  R4                                                               
                                                                                
VK07     L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      CHECK IF COMMERCIAL ALREADY VERIFIED         
         USING TACOD,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
*                                                                               
         TM    TACOUVST,TACOUVER   IF COMMERCIAL WAS NEVER VERIFIED             
         BZ    VK30                CONTINUE                                     
         TM    TACOUVST,TACOUVMU   IF A MUSICIAN UNVERIFIED COMM'L              
         BNO   VK10                                                             
         TM    TACOUVST,TACOUVNM   & NOT A NON-MUSICIAN                         
         BO    VK30                                                             
         OI    CVERSTA2,CVERSOMS   SHOW ONLY MUSICIANS                          
         B     VK30                                                             
*                                                                               
VK10     TM    TACOUVST,TACOUVNM   IF UNVERIFIED BY NON MUSICIAN                
         BNO   VK20                                                             
         OI    CVERSTA2,CVERSNMS   ONLY SHOW NON-MUSICIANS                      
*                                                                               
VK20     TM    TACOUVST,TACOUVCO   IF UNVERIFIED BY COMML                       
         BNO   VK30                                                             
         OI    CVERSTA2,CVERSOCO   SHOW COMM'L                                  
*                                                                               
VK30     OC    TACOVDTE,TACOVDTE   IS CAST VERIFIED                             
         BZ    VK100                                                            
         OI    CVERSTAT,CVERSVER   CAST VERIFIED                                
*                                                                               
VK100    LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
*                                                                               
VKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        THIS ROUTINE LOADS THE CAST SCREEN/PGM - TO VIEW THE                   
*        CAST.  AFTER RETURNING, IT WILL LOAD THE COMMERCIAL                    
*        SCREEN/PGM AND WILL RE-VALIDATE THE COMMERCIAL                         
*        IT WILL THEN SET THE CAST VERIFIED DATE AND RE-WRITE                   
*        THE COMMERCIAL.                                                        
*                                                                               
VER      TM    CVERSTAT,CVERSERR   IF ERROR IN VALIDATION                       
         BO    VER80                  SHOW COMMERCIAL                           
         TM    CVERSTAT,CVERSVER   IF CAST VERIFIED ALREADY                     
         BO    BEENVER                GIVE MESSAGE                              
         TM    CVERSTAT,CVERSCOV   IF COMMERCIAL REC VALID                      
         BO    VER80                  DON'T RE-VALIDATE                         
         TM    CVERSTAT,CVERSDON   IF FINISHED SHOWING CAST LIST                
         BO    VER10                  SHOW LAST EXTENSION SCREENS               
         TM    CVERSTA2,CVERSOMS   IF ANY CAST MEMEBER CAUSED COMM'L            
         BO    VER05                                                            
         TM    CVERSTA2,CVERSNMS   TO BE UNVERIFIED SHOW CAST                   
         BO    VER05                                                            
         TM    CVERSTA2,CVERSOCO   BUT IF ONLY COMMERCIAL UNVERIFIED            
         BO    VER80               COMML, ONLY SHOW THE COMM'L                  
*                                                                               
VER05    TM    CVERSTA2,CVERSSEL   IF SHOWING EXTENSION SCREENS                 
         BO    VER10                  CONTINUE                                  
         TM    CVERSTAT,CVERSVK    IF BEEN THROUGH CAST VALKEY                  
         BO    VER10                  GO STRAIGHT TO VALREC                     
         NI    SCLAGYH+4,X'DF'     SET AGY NOT VALID FOR CAST PHASE             
         B     VER20                   ON 1ST TIME THROUGH                      
*                                                                               
VER10    CLI   PFAID,15            IF DONE WITH CAST LIST - EXIT                
         BNE   VER15                                                            
         OI    CVERSTAT,CVERSDON                                                
         NI    CVERSTA2,X'FF'-CVERSSEL                                          
         B     VER60                                                            
*                                                                               
VER15    BAS   RE,CASTEXT          SHOW EXTENSION SCREENS                       
         TM    CVERSTAT,CVERSDON   IF FINISHED SHOWING EXT SCREENS              
         BO    VER80                  SHOW COMMERCIAL                           
*                                                                               
VER20    BAS   RE,LOADCAST         LOAD CAST PHASE (RETURNS ADDR IN R3)         
         MVI   MODE,VALKEY         GOTO1 CAST - MODE VALKEY                     
         GOTO1 (R3),DMCB,(RC)                                                   
         OI    CVERSTAT,CVERSVK    SET - BEEN THROUGH CAST VALKEY               
         BAS   RE,LOADCAST         LOAD CAST PHASE (RETURNS ADDR IN R3)         
*                                                                               
         MVI   MODE,VALREC         GOTO1 CAST - MODE VALREC                     
         GOTO1 (R3),DMCB,(RC)                                                   
         BNE   VER60                                                            
         OI    CVERSTA2,CVERSSEL   SET FLAG - SHOW EXT SCREENS                  
         B     MIDCAST                                                          
*                                                                               
VER60    TM    CVERSTAT,CVERSNW4   IF A W4 RECORD WAS NOT FOUND                 
         BO    CASTMISS            DO NOT ALLOW VERIFICATION                    
         OI    CVERSTAT,CVERSDON                                                
         TM    CVERSTA2,CVERSNCA   IF NO CAST TO DISPLAY                        
         BO    VER80               CONTINUE WITH COMMERCIAL                     
         CLI   PFAID,15            IF PF15 - CONTINUE WITH COMMERCIAL           
         BNE   ENDCAST                                                          
*                                                                               
VER80    MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)  GET REC FOR DISPLAY              
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         TM    CVERSTAT,CVERSCOV   IF COMMERCIAL REC VALID                      
         BO    VER95               DON'T RE-VALIDATE                            
         MVI   OVERLAY,SCR18       LOAD COMMERCIAL SCREEN                       
         MVI   TWASCR,SCR18                                                     
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   OVERLAY,X'18'       LOAD PROGRAM                                 
         GOTO1 LOADSOPH,DMCB,0                                                  
         MVI   MODE,DISPKEY        GOTO1 COMML - MODE DISP KEY                  
         GOTO1 (R3),DMCB,(RC)                                                   
*                                                                               
         MVI   MODE,DISPREC        GOTO1 COMML - MODE DISP RECORD               
         GOTO1 (R3),DMCB,(RC)                                                   
         OI    CVERSTAT,CVERSCOM   COMMERCIAL DISPLAYED                         
         TM    CVERSTAT,CVERSCOV   IF COMMERCIAL REC VALID                      
         BO    VER90                  DON'T RE-VALIDATE                         
         BAS   RE,REVAL            REVALIDATE CERTAIN FIELDS                    
*                                                                               
VER90    CLI   PFAID,22            HIT PF22 FOR PAGE 2                          
         BNE   COMDIS2                                                          
VER95    MVI   OVERLAY,X'F8'       LOAD COMMERCIAL PAGE 2 SCREEN                
         MVI   TWASCR,X'F8'                                                     
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
         MVI   OVERLAY,X'18'       LOAD PROGRAM                                 
         GOTO1 LOADSOPH,DMCB,0                                                  
         MVI   MODE,DISPKEY        GOTO1 COMML - MODE DISP KEY                  
         GOTO1 (R3),DMCB,(RC)                                                   
*                                                                               
         MVI   MODE,DISPREC        GOTO1 COMML - MODE DISP RECORD               
         GOTO1 (R3),DMCB,(RC)                                                   
         OI    CVERSTAT,CVERSCOM   COMMERCIAL DISPLAYED                         
         TM    CVERSTAT,CVERSCOV   IF COMMERCIAL REC VALID                      
         BO    VER100                 DON'T RE-VALIDATE                         
         TM    CVERSTAT,CVERSERR   IF THERE SHOULD BE AN AFM CODE               
         BNO   *+12                GIVE MISSING ERROR                           
         LA    R2,SC2AFMH                                                       
         B     MISSERR                                                          
         OI    CVERSTAT,CVERSCOV   COMMERCIAL VALIDATED                         
         B     COMDIS                                                           
*                                                                               
VER100   CLI   PFAID,23            HIT PF23 TO VERIFY                           
         BNE   COMDIS                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',0)  GET REC FOR UPDATE               
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
         L     R4,AIO                                                           
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMM'L DETAIL ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
*                                                                               
         MVI   TACOUVST,0          CLEAR UNVERIFIED STATUS BYTE                 
         MVC   TACOVDTE,TGTODAY1   TODAY'S DATE                                 
         MVC   TACOVSTU,TWAORIG    USER ID                                      
         MVC   TACOVST,TGCTSTAF    STAFF ID                                     
         OI    TACOUVST,TACOUVER   COMMERCIAL WAS VERIFIED                      
                                                                                
         TIME  DEC                                                              
         STCM  R0,14,TACOVTIM      CURRENT TIME                                 
         GOTO1 PUTREC                                                           
*                                                                               
         BAS   RE,LOADCSCR         LOAD CAST LIST SCREEN                        
         MVC   SCLAGY,TGAGY                                                     
         OI    SCLAGYH+6,X'80'     TRANSMIT FIELD                               
         MVC   SCLCID,TGCID                                                     
         OI    SCLCIDH+6,X'80'                                                  
         GOTO1 CHAROUT,DMCB,(X'80',TASNELQ),(1,SCLCIDNH)                        
         MVI   CVERSTAT,0          CLEAR CAST VERIFICATION STATUS               
*                                                                               
VERX     B     CVERCOM                                                          
         EJECT                                                                  
*                                                                               
*        SHOW ALL CAST EXTENSION SCREENS THAT HAVE INFO THAT'S                  
*        NOT ON THE LIST SCREEN                                                 
*                                                                               
CASTEXT  NTR1                                                                   
         MVC   SVACTVER,ACTNUM     SAVE THE PREVIOUS ACTION                     
         LA    R4,NUMSCR           SET LOOP COUNTER                             
         LA    R2,VERTAB                                                        
*                                                                               
CX10     OC    0(4,R2),0(R2)       END OF TABLE                                 
         BNZ   CX20                                                             
         LA    R2,4(R2)                                                         
         BCT   R4,CX10                                                          
         B     CXX                                                              
*                                                                               
CX20     XC    KEY,KEY                                                          
         USING TLDRD,R3            SET DISK ADDRESS OF RECORD                   
         LA    R3,KEY                                                           
         MVC   TLDRDA,0(R2)                                                     
         GOTO1 GETREC              & GET CAST RECORD                            
*                                                                               
         MVI   OVERLAY,SCR1A       LOAD CAST EXTENSION SCREEN                   
         CLI   RECNUM,SO                                                        
         BNE   *+8                                                              
         MVI   OVERLAY,SCRA3                                                    
         MVC   TWASCR,OVERLAY                                                   
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
*                                                                               
         BAS   RE,LOADCAST         LOAD CAST PHASE (RETURNS ADDR IN R3)         
*                                                                               
         MVI   MODE,DISPKEY        GOTO1 CAST - MODE DISPKEY                    
         GOTO1 (R3),DMCB,(RC)                                                   
         MVI   MODE,DISPREC        GOTO1 CAST - MODE DISPREC                    
         GOTO1 (R3),DMCB,(RC)                                                   
         XC    0(4,R2),0(R2)       CLEAR D/A OF WHAT WAS SHOWN                  
         MVI   SVACT,ACTVER        RESET ACTION                                 
         B     CASDISP                                                          
*                                                                               
CXX      BAS   RE,LOADCSCR         LOAD CAST LIST SCREEN                        
*                                                                               
         BAS   RE,LOADCAST         LOAD CAST PHASE (RETURNS ADDR IN R3)         
*                                                                               
         MVI   MODE,VALKEY         GOTO1 CAST - MODE VALKEY                     
         GOTO1 (R3),DMCB,(RC)                                                   
*                                                                               
         NI    CVERSTA2,X'FF'-CVERSSEL                                          
         MVC   SVACT,SVACTVER      RESET ACTION                                 
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        RE- VALIDATE CERTAIN PARTS OF COMMERCIAL RECORD                        
*                                                                               
REVAL    NTR1                                                                   
*                                                                               
         NI    CVERSTAT,X'FF'-CVERSERR                                          
         TM    CVERSTAT,CVERSMUS   IF THERE ARE MUSICIANS                       
         BNO   RV20                MUST HAVE MUSIC DATE                         
         L     R4,AIO              GET COMMERCIAL STUDIO ELEMENT                
         USING TACSD,R4                                                         
         MVI   ELCODE,TACSELQ                                                   
         MVI   WORK,TACSTYPM       GET MUSIC ELEMENT                            
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BE    RV10                                                             
         OI    CVERSTAT,CVERSERR                                                
         LA    R2,SCOMDATH                                                      
         B     MISSERR                                                          
         DROP  R4                                                               
*                                                                               
RV10     L     R4,AIO              ELSE                                         
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMM'L DETAIL ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
         OC    TACOAFM,TACOAFM                                                  
         BNZ   RV20                                                             
         OI    CVERSTAT,CVERSERR                                                
         B     XIT                                                              
*                                                                               
RV20     L     R4,AIO              CHECK IF MEDIA = TV                          
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      THEN MUST HAVE 1ST FIXED CYCLE               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
         MVC   BYTE,TACOSTA2       SAVE 2ND STATUS BYTE FOR LIFT CHECK          
         CLI   TACOTYPE,CTYMUS     IF COMM'L TYPE IS MUSIC                      
         BE    RV30                FIRST FIXED CYCLE IS NOT REQUIRED            
         CLI   TACOMED,TACOMEDT                                                 
         BE    RV25                                                             
         CLI   TACOMED,TACOMEDI                                                 
         BE    RV25                                                             
         CLI   TACOMED,TACOMEDN                                                 
         BNE   RV30                                                             
RV25     OC    TACOFCYC,TACOFCYC                                                
         BNZ   RV30                                                             
         LA    R2,SCOFFCH          SET CURSOR                                   
         OI    CVERSTAT,CVERSERR                                                
         B     MISSERR                                                          
*                                                                               
RV30     L     R4,AIO              CHECK IF THERE IS A LIFT ELEMENT             
         USING TALFD,R4                                                         
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RV40                                                             
         TM    BYTE,TACOSLFT       THEN THERE MUST BE CAST MEMBERS              
         BO    RV40                                                             
         OI    CVERSTAT,CVERSERR   ON THE LIFT                                  
         B     LIFTERR                                                          
*                                                                               
RV40     DS    0H                                                               
*                                                                               
RVX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS LOADING APPROPRIATE CAST PROGRAM                
         SPACE 1                                                                
LOADCAST NTR1                                                                   
         MVI   OVERLAY,X'1A'       LOAD CAST PHASE                              
         CLI   RECNUM,SO                                                        
         BNE   *+8                                                              
         MVI   OVERLAY,X'A3'       MAY NEED SOAP CAST                           
         GOTO1 LOADSOPH,DMCB,0                                                  
         XIT1  REGS=(R3)                                                        
         SPACE 3                                                                
*              ROUTINE CONTROLS LOADING APPROPRIATE CAST LIST SCREEN            
         SPACE 1                                                                
LOADCSCR NTR1                                                                   
         MVI   OVERLAY,SCR0A       LOAD CAST LIST SCREEN                        
         CLI   RECNUM,SO                                                        
         BNE   *+8                                                              
         MVI   OVERLAY,SCR03       MAY NEED SOAP CAST                           
         MVC   TWASCR,OVERLAY                                                   
         L     R3,EFHTAG                                                        
         GOTO1 LOADSOPH,DMCB,1                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
COMDIS   MVI   MYMSGNO1,28         COMMERCIAL PAGE 2 DISPLAYED                  
         B     INFXIT                                                           
*                                                                               
COMDIS2  MVI   MYMSGNO1,112        COMM'L PAGE 1 DISPLAYED                      
         B     INFXIT                                                           
*                                                                               
CASDISP  MVI   MYMSGNO1,63         CAST MEMBER DISPLAYED                        
         B     INFXIT                                                           
*                                                                               
MIDCAST  MVI   MYMSGNO1,27         MIDDLE OF CAST LIST                          
         B     INFXIT                                                           
*                                                                               
ENDCAST  MVI   MYMSGNO1,26         END OF CAST LIST                             
         B     INFXIT                                                           
*                                                                               
CVERCOM  MVI   MYMSGNO1,25         CAST VERIFICATION COMPLETE                   
         B     INFXIT                                                           
*                                                                               
BEENVER  MVI   ERROR,ERBNVER       ALREADY BEEN VERIFIED                        
         LA    R2,SCLCIDH                                                       
         B     ERRXIT                                                           
*                                                                               
CASTMISS MVI   ERROR,ERVW4MIS      W4 RECORD MISSING FOR CAST MEMBER            
         LA    R2,SCLCIDH                                                       
         B     ERRXIT                                                           
*                                                                               
LIFTERR  MVI   ERROR,ERVLIFT       NO CAST MEMBERS ON LIFT                      
         LA    R2,SCOLCIDH                                                      
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
ERUWB    MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE VERIFIED FROM                
         J     ERREND                                                           
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA) RECORD / ACTION INVALID FOR P+              
         J     ERREND                                                           
                                                                                
ERREND   MVI   BLOCK,0              VITA COMPLETION                             
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
                                                                                
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 3                                                                
*                                                                               
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR0AD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR18D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRF8D                                                       
         EJECT                                                                  
       ++INCLUDE TAGEN1AD                                                       
         EJECT                                                                  
*                                                                               
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025TAGEN1F   05/29/15'                                      
         END                                                                    
