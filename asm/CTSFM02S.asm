*          DATA SET CTSFM02S   AT LEVEL 049 AS OF 05/01/02                      
*PHASE TA0A02A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A02 - AUTO HELP RECORD MAINTENANCE/LIST                  *         
*                                                                     *         
*  COMMENTS: MAINTAINS AUTO HELP RECORDS.                             *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMF2 (TA0AF2) -- MAINTENANCE                    *         
*                  CTSFME2 (TA0AE2) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED AUTO HELP RECORDS                                 *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SECOND BASE                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A02 HELP RECORD MAINTENANCE/LIST'                            
TA0A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A02*,R8,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         XC    ACURSOR,ACURSOR                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         MVI   MYFLAGS,0           CLEAR FLAGS                                  
*                                                                               
         CLI   SFMLANGH+5,0        LANGUAGE NAME GIVEN?                         
         BNE   *+18                YES                                          
         MVC   SFMLANG,=C'ENG'     DEFAULT TO ENGLISH                           
         MVI   SFMLANGH+5,3        FUDGE INPUT LENGTH                           
         OI    SFMLANGH+6,X'80'    XMIT                                         
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    VKX                                                              
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FAALANG          A(LANGUAGE TABLE)                            
         DROP  R1                                                               
*                                                                               
         USING LANGTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLI   SFMLANGH+5,3        3-CHAR LANGUAGE CODE?                        
         BNE   INVLNG              MUST BE EXACTLY 3 CHARACTERS                 
*                                                                               
VK10     CLC   SFMLANG,LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BE    VK15                                                             
         CLC   SFMLANG,LANGSHRN    MATCH ON NATIVE SHORT LANG NAME?             
         BE    VK15                                                             
         BXLE  R5,R6,VK10          TRY NEXT TABLE ENTRY                         
         B     INVLNG              LANGUAGE NAME NOT IN TABLE                   
*                                                                               
VK15     MVC   LANGNUM,LANGCODE    SAVE LANGUAGE CODE                           
         XI    LANGNUM,X'FF'       FLIP BITS OF LANGUAGE CODE                   
*&&US*&& CLI   LANGNUM,X'FE'       IN U.S., LANGUAGE EUK NOT ALLOWED            
*&&UK*&& CLI   LANGNUM,X'FD'       IN U.K., LANGUAGE EUS NOT ALLOWED            
         BE    INVLNG                                                           
         DROP  R5                                                               
*                                                                               
         LA    R2,SFMSYSH          SYSTEM NAME                                  
         GOTO1 ANY                 MOVES NAME INTO WORK                         
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FASYSLST         A(SYSTEM LIST)                               
         DROP  R1                                                               
*                                                                               
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R3,5(R2)            INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
VK20     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SYSLNAME    MATCH ON SYSTEM NAME?                        
         BE    VK25                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SYSLSHRT    MATCH ON SHORT SYSTEM NAME?                  
         BE    VK25                                                             
         BXLE  R5,R6,VK20          TRY NEXT TABLE ENTRY                         
         B     INVSYSNM            SYSTEM NAME NOT IN TABLE                     
*                                                                               
VK25     MVC   SYSNUM,SYSLNUM      SAVE SYSTEM NUMBER                           
         DROP  R5                                                               
*                                                                               
         L     R1,SYSPARMS         A(PARAMS)                                    
         L     R1,0(R1)            A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         L     R5,VSELIST          A(SYSTEM EXECUTIVE LIST)                     
         DROP  R1                                                               
*                                                                               
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,SYSNUM      FIND ENTRY FOR THIS SYSTEM                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
*                                                                               
         L     R5,SEPGMS           A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
         MVI   PROGNUM,0           ASSUME PROGRAM 'ALL'                         
         LA    R2,SFMPROGH         PROGRAM NAME                                 
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK50                                                             
         CLI   ACTNUM,ACTREP       OPTIONAL FOR REPORT                          
         BE    VK50                                                             
         B     MISSERR                                                          
*                                                                               
VK30     CLI   5(R2),3             'ALL' IS 3 LONG                              
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)       'ALL' SCREENS?                               
         BE    VK50                                                             
*                                                                               
         XC    PROFNAME,PROFNAME                                                
         CLI   8(R2),C'='          '=XXX' IN PROGRAM FIELD?                     
         BNE   VK32                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   VK30A                                                            
         ZIC   RF,5(R2)            GET # OF CHARACTERS                          
         BCTR  RF,0                SUBTRACT 1 FOR =                             
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PROFNAME(0),9(R2)      START AT FILTER                           
         B     VK31                                                             
*                                                                               
VK30A    CLI   9(R2),X'C0'         ALPHA NUMERIC CHARACTER                      
         BNH   INVPRGNM                                                         
         CLI   5(R2),3             AT LEAST 2 ALPHAS IN PROGRAM NAME            
         BL    INVPRGNM                                                         
*                                                                               
         MVC   PROFNAME,9(R2)      MOVE IN PROG NAME                            
VK31     OI    MYFLAGS,PRGEQUAL    PROGRAM NAME IN PROGRAM FIELD                
*                                                                               
         LA    R2,SFMSCRNH         SCREEN                                       
         CLI   5(R2),0             MUST BE NULL IF PROGRAM NAME ENTERED         
         BNE   NULLFLD                                                          
*                                                                               
         LA    R2,SFMPAGEH         PAGE                                         
         CLI   5(R2),0             MUST BE NULL IF PROGRAM NAME ENTERED         
         BNE   NULLFLD                                                          
         B     VK60                                                             
*                                                                               
VK32     DS    0H                                                               
         CLI   8(R2),C'#'          PROGRAM NUMBER GIVEN?                        
         BNE   VK33                                                             
         CLI   5(R2),3             FORMAT IS #NN                                
         BNE   INVPRGNM                                                         
         GOTO1 HEXIN,DMCB,9(R2),PROGNUM,2                                       
         CLC   =F'1',DMCB+12                                                    
         BE    VK50                                                             
         B     INVPRGNM                                                         
*                                                                               
VK33     GOTO1 ANY                 LEFT-JUSTIFY NAME INTO WORK                  
*                                                                               
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         MVI   BYTE,C'N'           CLEAR LANG FLAG                              
*                                                                               
VK33A    CLI   PGMCTRY,0           ARE WE ON DEFAULTS                           
         BNE   *+16                                                             
         CLI   BYTE,C'Y'           ANY FOR THIS CTRY?                           
         BE    INVPRGNM            THEN NOT FOUND                               
         B     VK33B                                                            
*                                                                               
         MVC   HALF,LANGNUM                                                     
         XI    HALF,X'FF'                                                       
         CLC   PGMCTRY,HALF        TEST SAME LANG/(CTRY)                        
         BNE   *+12                                                             
         MVI   BYTE,C'Y'           FLAG WE HAVE SOME                            
         B     VK33B                                                            
         CLI   BYTE,C'Y'           ANY FOR THIS CTRY?                           
         BE    INVPRGNM            THEN NOT FOUND                               
         B     VK41                                                             
*                                                                               
VK33B    CLI   SYSNUM,1            SERVICE SYSTEM?                              
         BNE   VK35                NO -- DO EXECUTED COMPARE                    
         CLC   PGMNAME,WORK        YES -- ENTIRE NAME MUST MATCH                
         BE    VK45                                                             
         BXLE  R5,R6,VK33A         TRY NEXT TABLE ENTRY                         
         B     INVPRGNM            SERVICE REQUEST NAME NOT IN TABLE            
*                                                                               
VK35     ZIC   R3,5(R2)            INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
VK40     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),PGMNAME     MATCH ON PROGRAM NAME?                       
         BE    VK45                                                             
VK41     BXLE  R5,R6,VK33A         TRY NEXT TABLE ENTRY                         
         B     INVPRGNM            PROGRAM NAME NOT IN TABLE                    
*                                                                               
VK45     MVC   PROGNUM,PGMNUM      SAVE PROGRAM NUMBER                          
         DROP  R5                                                               
*                                                                               
VK50     CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                                                              
*                                                                               
         MVI   SCRNNUM,0           ASSUME SCREEN 'ALL'                          
         MVI   ALLSCRNS,C'Y'                                                    
         LA    R2,SFMSCRNH         SCREEN NAME                                  
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK60                                                             
         B     MISSERR                                                          
*                                                                               
         CLC   =C'ALL',8(R2)       'ALL' SCREENS?                               
         BE    VK60                                                             
*                                                                               
         MVI   ALLSCRNS,C'N'                                                    
         GOTO1 HEXIN,DMCB,SFMSCRN,BYTE,2                                        
         CLI   DMCB+15,1                                                        
         BNE   INVSCRN             SCREEN NAME IS ONE-BYTE HEX                  
         MVC   SCRNNUM,BYTE                                                     
*                                                                               
         CLI   PROGNUM,0           'ALL' PROGRAMS?                              
         BE    NOALLERR            YES -- ERROR                                 
*                                                                               
VK60     MVI   FIELDNUM,0          ASSUME FIELD 'ALL'                           
         MVI   ALLFLDS,C'Y'                                                     
         LA    R2,SFMFLDH          FIELD NUMBER                                 
         CLI   5(R2),0             NUMBER GIVEN?                                
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK70                                                             
*                                                                               
         TM    MYFLAGS,PRGEQUAL    ENTERED PROGRAM NAME '=XXX'?                 
         BO    VK62                                                             
*                                                                               
         CLI   SCRNNUM,0           FIELD REQUIRES SCREEN                        
         BNE   *+12                                                             
         CLI   ALLSCRNS,C'Y'       OR 'ALL' SCREENS                             
         BNE   NEEDSCRN                                                         
*                                                                               
VK62     CLC   =C'ALL',8(R2)       'ALL' FIELDS?                                
         BNE   *+16                                                             
         CLI   SCRNNUM,0           'ALL' SCREENS?                               
         BE    VK70                                                             
         B     NOTSUPP             SCREEN-WIDE HELP NOT YET SUPPORTED           
         MVI   ALLFLDS,C'N'                                                     
         TM    4(R2),X'08'         DATA NUMERIC?                                
         BZ    INVFLDN             NO                                           
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMFLD(0)                                                    
         CVB   R1,DUB                                                           
         CH    R1,=H'1'            NUMBER BETWEEN 1 AND 255?                    
         BL    INVFLDN                                                          
         CH    R1,=H'255'                                                       
         BH    INVFLDN                                                          
         STC   R1,FIELDNUM                                                      
*                                                                               
VK70     DS    0H                                                               
         TM    MYFLAGS,PRGEQUAL    ENTERED PROGRAM NAME '=XXX'?                 
         BO    VK72                                                             
*                                                                               
         MVI   PAGENUM,1           ASSUME PAGE 1                                
         LA    R2,SFMPAGEH         PAGE NUMBER FIELD                            
         CLI   5(R2),0             NUMBER GIVEN?                                
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK80                                                             
         CLI   FIELDNUM,0          PAGE REQUIRES FIELD                          
         BNE   *+12                                                             
         CLI   ALLFLDS,C'Y'        OR 'ALL' FIELDS                              
         BNE   NEEDFLDN                                                         
*                                                                               
         TM    4(R2),X'08'         DATA NUMERIC?                                
         BZ    INVPAGEN            NO                                           
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMPAGE(0)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'1'            NUMBER IN RANGE?                             
         BE    VK75                                                             
         BL    INVPAGEN                                                         
         CH    R1,=H'255'                                                       
         BH    INVPAGEN                                                         
         STC   R1,PAGENUM                                                       
*                                                                               
VK72     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK75                                                             
         XC    KEY,KEY             BUILD KEY FOR PREVIOUS PAGE                  
         LA    R4,KEY                                                           
         USING HV1KEY,R4                                                        
         MVI   HV1TYPE,HV1TYPEQ    AUTO HELP RECORD                             
         MVC   HV1SYS,SYSNUM                                                    
         MVC   HV1LANG,LANGNUM                                                  
*                                                                               
         TM    MYFLAGS,PRGEQUAL    PROGRAM NAME ENTERED '=XXX'                  
         BZ    VK73                                                             
         MVC   HV1PROF(3),PROFNAME    PROGRAM NAME                              
         MVC   HV1PRFN,FIELDNUM    FIELD NUMBER                                 
         B     VK74                                                             
*                                                                               
VK73     MVC   HV1PROG,PROGNUM                                                  
         MVC   HV1SCRN,SCRNNUM                                                  
         MVC   HV1FIELD,FIELDNUM                                                
         ZIC   R1,PAGENUM                                                       
         BCTR  R1,0                                                             
         STC   R1,HV1PAGE                                                       
*                                                                               
VK74     GOTO1 HIGH                                                             
         CLC   KEY(HV1KLENQ),KEYSAVE                                            
         BE    VK80                PREVIOUS PAGE IS THERE                       
         TM    MYFLAGS,PRGEQUAL    PROGRAM NAME ENTERED '=XXX'                  
         BZ    NOPREVPG                                                         
*                                                                               
VK75     CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   VK80                                                             
         XC    KEY,KEY             BUILD KEY FOR NEXT PAGE                      
         LA    R4,KEY                                                           
         USING HV1KEY,R4                                                        
         MVI   HV1TYPE,HV1TYPEQ    AUTO HELP RECORD                             
         MVC   HV1SYS,SYSNUM                                                    
         MVC   HV1LANG,LANGNUM                                                  
*                                                                               
         TM    MYFLAGS,PRGEQUAL    PROGRAM NAME ENTERED '=XXX'                  
         BZ    VK76                                                             
         MVC   HV1PROF(3),PROFNAME    PROGRAM NAME                              
         MVC   HV1PRFN,FIELDNUM    FIELD NUMBER                                 
         B     VK77                                                             
*                                                                               
VK76     MVC   HV1PROG,PROGNUM                                                  
         MVC   HV1SCRN,SCRNNUM                                                  
         MVC   HV1FIELD,FIELDNUM                                                
         ZIC   R1,PAGENUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,HV1PAGE                                                       
*                                                                               
VK77     GOTO1 HIGH                                                             
         CLC   KEY(HV1KLENQ),KEYSAVE                                            
         BE    YSNEXTPG            NEXT PAGE EXISTS - CANNOT DELETE             
*                                                                               
VK80     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         MVI   HV1TYPE,HV1TYPEQ    AUTO HELP RECORD                             
         MVC   HV1SYS,SYSNUM                                                    
         MVC   HV1LANG,LANGNUM                                                  
*                                                                               
         TM    MYFLAGS,PRGEQUAL    PROGRAM NAME ENTERED '=XXX'                  
         BZ    VK81                                                             
         MVC   HV1PROF(3),PROFNAME    PROGRAM NAME                              
         MVC   HV1PRFN,FIELDNUM    FIELD NUMBER                                 
         B     VKX                                                              
*                                                                               
VK81     MVC   HV1PROG,PROGNUM                                                  
         MVC   HV1SCRN,SCRNNUM                                                  
         MVC   HV1FIELD,FIELDNUM                                                
         MVC   HV1PAGE,PAGENUM                                                  
*                                                                               
VKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       CLI   PFKEY,3             ERASE LINE?                                  
         BE    VR5                                                              
         CLI   PFKEY,4             ADD LINE?                                    
         BE    VR5                                                              
         CLI   PFKEY,5             TOGGLE HIGHLIGHTING?                         
         BNE   VR100                                                            
*                                                                               
VR5      L     R5,ATIOB            A(TIOB)                                      
         USING TIOBD,R5                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R5                                                               
*                                                                               
         LA    R2,SFMFILLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR10     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR100               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR20                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR100               YES                                          
         B     VR10                                                             
*                                                                               
VR20     LA    RF,SFMTXTLH         A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VR50                NO                                           
*                                                                               
         LA    R0,SFMFILLH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR100               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR30     CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR40                YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R2),1(R3)       MOVE ATTRIBUTE BYTE                          
         LR    R2,R3                                                            
         B     VR30                                                             
*                                                                               
VR40     XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R2),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR100                                                            
*                                                                               
VR50     CLI   PFKEY,4             ADD LINE?                                    
         BNE   VR80                NO                                           
*                                                                               
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR100               YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,SFMTXTLH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR70                YES                                          
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R3),1(R2)       MOVE ATTRIBUTE BYTE                          
         LR    R3,R2                                                            
         B     VR60                                                             
*                                                                               
VR70     XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R3),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURSOR          KEEP CURSOR IN PLACE                         
         B     VR100                                                            
*                                                                               
VR80     TM    1(R2),X'08'         IS LINE CURRENTLY HIGH INTENSITY?            
         BZ    *+12                                                             
         NI    1(R2),X'FF'-X'0C'   YES -- FORCE NORMAL INTENSITY                
         B     *+8                                                              
         OI    1(R2),X'08'         NO -- FORCE HIGH INTENSITY                   
         EJECT                                                                  
VR100    L     R4,AIO              A(HELP RECORD)                               
         USING HV1KEY,R4                                                        
*                                                                               
         MVI   ELCODE,HV1HEDEQ     HEADING ELEMENT                              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMHEADH         HEADING FIELD                                
         CLI   5(R2),0             ANY HEADING?                                 
         BE    VR110                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING HV1HEDEL,R6                                                      
         MVI   HV1HEDEL,HV1HEDEQ   ELEMENT CODE                                 
         MVI   HV1HEDLN,HV1HEDLQ   ELEMENT LENGTH                               
         MVC   HV1HEDTL,5(R2)      LENGTH OF HEADING                            
         MVC   HV1HEDTX,SFMHEAD    ACTUAL HEADING                               
         OC    HV1HEDTX,BLANKS                                                  
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR110    MVI   ELCODE,HV1STAEQ     STATUS ELEMENT                               
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING HV1STAEL,R6                                                      
         MVI   HV1STAEL,HV1STAEQ   ELEMENT CODE                                 
         MVI   HV1STALN,HV1STALQ   ELEMENT LENGTH                               
         OI    HV1STAST,HV1STAPQ   TURN ON PROTECT BIT                          
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SFMTXTH          FIRST TEXT FIELD                             
         MVI   TXTFOUND,C'N'       NO TEXT FOUND YET                            
         MVI   SEQNUM,0                                                         
         MVI   ELCODE,HV1TXTEQ     HELP TEXT ELEMENT CODE                       
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R6,ELEM                                                          
         USING HV1TXTEL,R6                                                      
*                                                                               
VR120    LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   HV1TXTEL,HV1TXTEQ   HELP TEXT LINE ELEMENT CODE                  
         MVC   HV1TXTSQ,SEQNUM     SEQUENCE NUMBER                              
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR130                                                            
         MVI   TXTFOUND,C'Y'       SOME TEXT WAS FOUND                          
         BCTR  R1,0                                                             
         LA    R3,8(R2,R1)         LAST CHARACTER OF TEXT                       
         CLI   0(R3),X'4F'         HIGH INTENSITY CHARACTER?                    
         BNE   *+8                                                              
         OI    HV1TXTOP,X'80'      TURN ON HIGH INTENSITY FLAG BIT              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HV1TXTTX(0),8(R2)   HELP TEXT LINE                               
         LA    R3,HV1TXTOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,1(R3,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR150                                                            
*                                                                               
VR130    ST    R2,MYWORK           HANG ON TO CURRENT TWA POINTER               
         LA    RF,SFMTXTLH                                                      
*                                                                               
VR140    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         CR    R2,RF               END OF SCREEN?                               
         BNH   *+12                NO                                           
         L     R2,MYWORK                                                        
         B     VRX                                                              
*                                                                               
         CLI   5(R2),0             ANY INPUT THIS FIELD?                        
         BE    VR140               TRY NEXT FIELD                               
         L     R2,MYWORK                                                        
*                                                                               
         MVI   HV1TXTTX,C' '       MUST SAVE A BLANK LINE                       
         LA    R1,HV1TXTOV                                                      
         LA    R1,1(R1)            TOTAL LENGTH OF ELEMENT                      
*                                                                               
VR150    STC   R1,HV1TXTLN                                                      
*                                                                               
         TM    1(R2),X'08'         HIGH INTENSITY?                              
         BZ    *+8                                                              
         OI    HV1TXTOP,X'80'                                                   
         MVC   HV1TXTVB,=X'FFFF'   VALIDITY BITS                                
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         B     VR120                                                            
*                                                                               
VRX      CLI   TXTFOUND,C'Y'       MAKE SURE SOME TEXT WAS FOUND                
         BNE   NOTXTERR                                                         
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R4,AIO                                                           
*                                                                               
         LA    R6,HV1FSTEL         A(FIRST ELEMENT)                             
         USING HV1HEDEL,R6                                                      
         LA    R2,SFMHEADH         HEADING FIELD                                
         MVC   SFMHEAD,BLANKS                                                   
         MVI   ELCODE,HV1HEDEQ     HEADING ELEMENT CODE                         
         BAS   RE,FIRSTEL          HEADING ELEMENT IS THERE?                    
         BNE   *+10                                                             
         MVC   SFMHEAD,HV1HEDTX    PUT HEADING IN FIELD                         
         OI    SFMHEADH+6,X'80'                                                 
*                                                                               
         LA    R6,HV1FSTEL         A(FIRST ELEMENT)                             
         USING HV1TXTEL,R6                                                      
         LA    R2,SFMTXTH          FIRST HELP TEXT FIELD                        
         MVI   ELCODE,HV1TXTEQ     HELP TEXT ELEMENT CODE                       
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BE    *+6                                                              
         DC    H'00'               MUST BE SOME TEXT                            
*                                                                               
DR10     MVC   8(L'SFMTXT,R2),BLANKS                                            
         ZIC   R1,HV1TXTLN         LENGTH OF ELEMENT                            
         LA    R3,HV1TXTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),HV1TXTTX    HELP LINE OF TEXT                            
         NI    1(R2),X'FF'-X'0C'   ASSUME NORMAL INTENSITY                      
         TM    HV1TXTOP,X'80'                                                   
         BZ    *+8                                                              
         OI    1(R2),X'08'         NO, IT'S HIGH INTENSITY                      
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
*                                                                               
         BAS   RE,NEXTEL           NEXT LINE OF HELP TEXT                       
         BE    DR10                                                             
*                                                                               
DR20     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            LENGTH OF DATA + 1 (FOR EX)                  
         TM    1(R2),X'02'         TEXT EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLANKS      BLANK OUT REMAINING FIELDS                   
         NI    1(R2),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BNH   DR20                NO                                           
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING HV1KEY,R4                                                        
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FAALANG          A(LANGUAGE TABLE)                            
         DROP  R1                                                               
*                                                                               
         USING LANGTABD,R5                                                      
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
DK5      MVC   BYTE,LANGCODE                                                    
         XI    BYTE,X'FF'          FLIP BITS OF LANGUAGE                        
         CLC   HV1LANG,BYTE        MATCH ON LANGUAGE CODE?                      
         BE    *+10                                                             
         BXLE  R5,R6,DK5           TRY NEXT TABLE ENTRY                         
         DC    H'0'                BAD LANGUAGE CODE IN RECORD                  
*                                                                               
         MVC   SFMLANG,BLANKS                                                   
         MVC   SFMLANG(L'LANGSHR),LANGSHR                                       
         OI    SFMLANGH+6,X'80'    TRANSMIT SHORT LANGUAGE NAME                 
         DROP  R5                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FASYSLST         A(SYSTEM LIST)                               
         DROP  R1                                                               
*                                                                               
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   HV1SYS,SYSLNUM      MATCH ON SYSTEM NUMBER?                      
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT TABLE ENTRY                         
         DC    H'0'                BAD SYSTEM NUMBER IN RECORD                  
*                                                                               
         MVC   SFMSYS,BLANKS                                                    
         MVC   SFMSYS(L'SYSLSHRT),SYSLSHRT                                      
         OI    SFMSYSH+6,X'80'     TRANSMIT SHORT SYSTEM NAME                   
         DROP  R5                                                               
*                                                                               
         L     R1,SYSPARMS         A(PARAMS)                                    
         L     R1,0(R1)            A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         L     R5,VSELIST          A(SYSTEM EXECUTIVE LIST)                     
         DROP  R1                                                               
*                                                                               
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   HV1SYS,SEOVSYS      FIND ENTRY FOR THIS SYSTEM                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
*                                                                               
         L     R5,SEPGMS           A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
         OI    SFMPROGH+6,X'80'    ALWAYS XMIT PROGRAM NAME                     
         CLI   HV1PROF,X'C0'       ALPHA CHARACTER                              
         BL    DK7                                                              
         MVI   SFMPROG,C'='                                                     
         MVC   SFMPROG+1(3),HV1PROF          YES                                
         B     DK50                                                             
*                                                                               
DK7      CLI   HV1PROG,0           'ALL' PROGRAMS?                              
         BNE   DK10                                                             
         XC    SFMPROG,SFMPROG                                                  
         MVC   SFMPROG(3),=C'ALL'                                               
         B     DK20                                                             
*                                                                               
         USING PGMLSTD,R5                                                       
DK10     LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   HV1PROG,PGMNUM      MATCH ON PROGRAM NUMBER?                     
         BE    DK15                                                             
         BXLE  R5,R6,*-10          TRY NEXT TABLE ENTRY                         
         MVI   SFMPROG,C'#'                                                     
         GOTO1 HEXOUT,DMCB,HV1PROG,SFMPROG+1,1,=C'TOG'                          
         CLI   DMCB+19,2                                                        
         BE    DK20                PROGRAM NUMBER IS ONE-BYTE HEX               
         DC    H'0'                                                             
DK15     MVC   SFMPROG,PGMNAME                                                  
         DROP  R5                                                               
*                                                                               
DK20     CLI   HV1SCRN,0           SCREEN 'ALL'?                                
         BNE   *+14                                                             
         MVC   SFMSCRN,=C'ALL'                                                  
         B     DK30                                                             
*                                                                               
         XC    SFMSCRN,SFMSCRN                                                  
         GOTO1 HEXOUT,DMCB,HV1SCRN,SFMSCRN,1,=C'TOG'                            
         CLI   DMCB+19,2                                                        
         BE    *+6                 SCREEN NAME IS ONE-BYTE HEX                  
         DC    H'00'                                                            
DK30     OI    SFMSCRNH+6,X'80'                                                 
*                                                                               
         CLI   HV1FIELD,0                                                       
         BNE   *+14                                                             
         MVC   SFMFLD,=C'ALL'                                                   
         B     DK40                                                             
*                                                                               
         EDIT  (B1,HV1FIELD),(3,SFMFLD),ALIGN=LEFT                              
DK40     OI    SFMFLDH+6,X'80'     FIELD                                        
*                                                                               
         EDIT  (B1,HV1PAGE),(3,SFMPAGE),ALIGN=LEFT                              
         OI    SFMPAGEH+6,X'80'    PAGE                                         
         B     EXIT                                                             
*                                                                               
DK50     DS    0H                                                               
         EDIT  (B1,HV1PRFN),(3,SFMFLD),ALIGN=LEFT                               
         OI    SFMFLDH+6,X'80'     FIELD                                        
*                                                                               
DKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       LA    R4,KEY                                                           
         USING HV1KEY,R4                                                        
*                                                                               
         OC    KEY(HV1KLENQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   HV1TYPE,HV1TYPEQ    AUTO HELP RECORD KEY                         
         MVC   HV1SYS,SYSNUM                                                    
         MVC   HV1LANG,LANGNUM                                                  
*                                                                               
         TM    MYFLAGS,PRGEQUAL    PROGRAM NAME ENTERED '=XXX'                  
         BZ    LR5                                                              
         MVC   HV1PROF(3),PROFNAME    PROGRAM NAME                              
         MVC   HV1PRFN,FIELDNUM    FIELD NUMBER                                 
         MVC   SAVEKEY,KEY                                                      
         B     LR10                                                             
*                                                                               
LR5      MVC   HV1PROG,PROGNUM                                                  
         MVC   HV1SCRN,SCRNNUM                                                  
         MVC   HV1FIELD,FIELDNUM                                                
         MVC   HV1PAGE,PAGENUM                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(23),SAVEKEY     SAME SYSTEM?                                 
         BNE   EXIT                                                             
*                                                                               
         CLI   PROGNUM,0           'ALL' PROGRAMS?                              
         BE    *+14                                                             
         CLC   HV1PROG,PROGNUM     IF SO, TEST KEY MATCH                        
         BNE   EXIT                                                             
*                                                                               
         CLI   SCRNNUM,0           SCREEN ENTERED?                              
         BE    *+14                                                             
         CLC   HV1SCRN,SCRNNUM     IF SO, TEST KEY MATCH                        
         BNE   EXIT                                                             
*                                                                               
         CLI   FIELDNUM,0          FIELD ENTERED?                               
         BE    LR20A                                                            
*                                                                               
         CLI   HV1PROF,X'C0'       =XXX PROGRAM NAME?                           
         BL    *+18                                                             
         CLC   HV1PRFN,FIELDNUM    YES - TEST KEY MATCH                         
         BNE   LR60                                                             
         B     LR20A                                                            
*                                                                               
         CLC   HV1FIELD,FIELDNUM   IF SO, TEST KEY MATCH                        
         BNE   LR60                                                             
*                                                                               
LR20A    CLI   SFMPAGEH+5,0        PAGE NUMBER ENTERED?                         
         BE    *+14                                                             
         CLC   HV1PAGE,PAGENUM     IF SO, TEST KEY MATCH                        
         BNE   LR60                                                             
*                                                                               
         CLC   HV1LANG,LANGNUM     MATCH ON LANGUAGE?                           
         BNE   LR60                NO                                           
*                                                                               
         MVC   LISTAR,BLANKS       FILL IN LIST LINE                            
*                                                                               
         CLI   HV1PROF,X'C0'       ALPHA CHARACTER HERE?                        
         BL    LR21                NO                                           
         MVC   LSTPROG(3),HV1PROG                                               
*                                                                               
         CLI   HV1PRFN,0           JUMP TO FIELD NUMBER                         
         BNE   *+14                                                             
         MVC   LSTFIELD,=C'ALL'                                                 
         B     LR41                                                             
         EDIT  (B1,HV1PRFN),(3,LSTFIELD)                                        
         B     LR41                                                             
*                                                                               
LR21     CLI   HV1PROG,0           'ALL' PROGRAMS?                              
         BNE   *+14                                                             
         MVC   LSTPROG(3),=C'ALL'                                               
         B     LR25                                                             
*                                                                               
         L     R1,SYSPARMS         A(PARAMS)                                    
         L     R1,0(R1)            A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         L     R5,VSELIST          A(SYSTEM EXECUTIVE LIST)                     
         DROP  R1                                                               
*                                                                               
         USING SELISTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   HV1SYS,SEOVSYS      FIND ENTRY FOR THIS SYSTEM                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
*                                                                               
         L     R5,SEPGMS           A(PROGRAM NAME LIST)                         
         DROP  R5                                                               
*                                                                               
         USING PGMLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
         CLC   HV1PROG,PGMNUM      MATCH ON PROGRAM NUMBER?                     
         BE    LR22                                                             
         BXLE  R5,R6,*-10          TRY NEXT TABLE ENTRY                         
         MVI   LSTPROG,C'#'                                                     
         GOTO1 HEXOUT,DMCB,HV1PROG,LSTPROG+1,1,=C'TOG'                          
         CLI   DMCB+19,2                                                        
         BE    LR25                PROGRAM NUMBER IS ONE-BYTE HEX               
         DC    H'0'                                                             
LR22     MVC   LSTPROG,PGMNAME     DISPLAY PROGRAM NAME                         
         DROP  R5                                                               
*                                                                               
LR25     CLI   HV1SCRN,0           'ALL' SCREENS?                               
         BNE   *+14                                                             
         MVC   LSTSCRN,=C'ALL'                                                  
         B     LR30                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,HV1SCRN,LSTSCRN,1,=C'TOG'                            
         CLI   DMCB+19,2                                                        
         BE    *+6                 SCREEN NAME IS ONE-BYTE HEX                  
         DC    H'00'                                                            
*                                                                               
LR30     CLI   HV1FIELD,0                                                       
         BNE   *+14                                                             
         MVC   LSTFIELD,=C'ALL'                                                 
         B     LR40                                                             
         EDIT  (B1,HV1FIELD),(3,LSTFIELD)                                       
*                                                                               
LR40     EDIT  (B1,HV1PAGE),(3,LSTPAGE)                                         
LR41     GOTO1 GETREC              READ IN ELEMENTS                             
         L     R4,AIO              A(HELP RECORD)                               
*                                                                               
         LA    R6,HV1FSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,HV1HEDEQ     HEADING ELEMENT                              
*                                                                               
         USING HV1HEDEL,R6                                                      
         BAS   RE,FIRSTEL          LOOK FOR HEADING ELEMENT                     
         BNE   *+10                                                             
         MVC   LSTHEAD,HV1HEDTX    PUT HEADING IN LIST LINE                     
*                                                                               
         LA    R6,HV1FSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,HV1TXTEQ     TEXT LINE ELEMENT                            
*                                                                               
         USING HV1TXTEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 LOOK FOR FIRST TEXT LINE ELEMENT             
         DC    H'00'               MUST BE THERE                                
*                                                                               
         MVC   MYWORK,BLANKS                                                    
         ZIC   R3,HV1TXTLN         TOTAL LENGTH OF ELEMENT                      
         LA    R1,HV1TXTOV         LENGTH OF OVERHEAD                           
         SR    R3,R1               LENGTH OF TEXT                               
         C     R3,=F'1'            TEXT LENGTH 1?                               
         BNE   *+12                NO                                           
         CLI   HV1TXTTX,C' '       BLANK LINE?                                  
         BE    LR55                YES - LEAVE THIS FIELD ALONE                 
         LA    R1,HV1TXTTX         A(TEXT)                                      
*                                                                               
LR45     CLI   0(R1),C' '          IGNORE LEADING SPACES                        
         BNE   LR50                                                             
         BCTR  R3,0                SUBTRACT 1 FROM MOVE LENGTH                  
         LA    R1,1(R1)                                                         
         B     LR45                LOOK AT NEXT CHARACTER                       
*                                                                               
LR50     BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK(0),0(R1)     TEXT IS LEFT-JUSTIFIED                       
         MVC   LSTTEXT,MYWORK      AND PADDED WITH SPACES                       
*                                                                               
LR55     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                 NEXT HELP RECORD                             
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR20                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
INVLNG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVLANGM),INVLANGM                                     
         LA    R2,SFMLANGH                                                      
         GOTO1 ERREX2                                                           
INVLANGM DC    C'* ERROR * INVALID LANGUAGE NAME *'                             
*                                                                               
INVSYSNM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVSYSMS),INVSYSMS                                     
         LA    R2,SFMSYSH                                                       
         GOTO1 ERREX2                                                           
INVSYSMS DC    C'* ERROR * INVALID SYSTEM NAME *'                               
*                                                                               
INVPRGNM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPRGMS),INVPRGMS                                     
         LA    R2,SFMPROGH                                                      
         GOTO1 ERREX2                                                           
INVPRGMS DC    C'* ERROR * INVALID PROGRAM NAME *'                              
*                                                                               
INVSCRN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVSCRNM),INVSCRNM                                     
         LA    R2,SFMSCRNH                                                      
         GOTO1 ERREX2                                                           
INVSCRNM DC    C'* ERROR * INVALID SCREEN NAME *'                               
*                                                                               
NEEDSCRN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NEEDSCRM),NEEDSCRM                                     
         LA    R2,SFMSCRNH                                                      
         GOTO1 ERREX2                                                           
NEEDSCRM DC    C'* ERROR * FIELD NUMBER REQUIRES SCREEN *'                      
*                                                                               
INVFLDN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFLDNM),INVFLDNM                                     
         LA    R2,SFMFLDH                                                       
         GOTO1 ERREX2                                                           
INVFLDNM DC    C'* ERROR * INVALID FIELD NUMBER *'                              
*                                                                               
NEEDFLDN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NEEDFLDM),NEEDFLDM                                     
         LA    R2,SFMFLDH                                                       
         GOTO1 ERREX2                                                           
NEEDFLDM DC    C'* ERROR * PAGE NUMBER REQUIRES FIELD *'                        
*                                                                               
INVPAGEN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPAGEM),INVPAGEM                                     
         LA    R2,SFMPAGEH                                                      
         GOTO1 ERREX2                                                           
INVPAGEM DC    C'* ERROR * INVALID PAGE NUMBER *'                               
*                                                                               
NOTXTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTXTMS),NOTXTMS                                       
         LA    R2,SFMTXTH                                                       
         GOTO1 ERREX2                                                           
NOTXTMS  DC    C'* ERROR * AT LEAST ONE LINE OF TEXT REQUIRED *'                
*                                                                               
NOALLERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOALLMS),NOALLMS                                       
         LA    R2,SFMPROGH                                                      
         GOTO1 ERREX2                                                           
NOALLMS  DC    C'* ERROR * ''ALL'' PROGRAMS REQUIRES ''ALL'' SCREENS *'         
*                                                                               
NOPREVPG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPREVPM),NOPREVPM                                     
         LA    R2,SFMPAGEH                                                      
         GOTO1 ERREX2                                                           
NOPREVPM DC    C'* ERROR * PREVIOUS PAGE NUMBER DOES NOT EXIST *'               
*                                                                               
YSNEXTPG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'YSNEXTPM),YSNEXTPM                                     
         LA    R2,SFMPAGEH                                                      
         GOTO1 ERREX2                                                           
YSNEXTPM DC    C'* ERROR * SUBSEQUENT PAGE(S) STILL EXIST *'                    
*                                                                               
NOTSUPP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTSUPPH),NOTSUPPH                                     
         LA    R2,SFMFLDH                                                       
         GOTO1 ERREX2                                                           
NOTSUPPH DC    C'* ERROR * SCREEN-WIDE HELP NOT YET SUPPORTED *'                
*                                                                               
NULLFLD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NULLFLDH),NULLFLDH                                     
         GOTO1 ERREX2                                                           
NULLFLDH DC    C'* ERROR * MUST BE NULL IF PROGRAM NAME ENTERED *'              
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
BLANKS   DS    CL132' '                                                         
*                                                                               
MYFLAGS  DS    XL1                 FLAGS                                        
PRGEQUAL EQU   X'01'               =XXX IN PROGRAM FIELD                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENHV1                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMF2D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
ACURSOR  DS    A                   FORCE CURSOR HERE                            
MYWORK   DS    XL96                                                             
SAVEKEY  DS    XL25                AUTO HELP RECORD KEY                         
SYSNUM   DS    XL1                 SYSTEM NUMBER                                
PROGNUM  DS    XL1                 PROGRAM NUMBER                               
SCRNNUM  DS    XL1                 SCREEN NUMBER                                
FIELDNUM DS    XL1                 FIELD NUMBER                                 
PAGENUM  DS    XL1                 PAGE NUMBER                                  
LANGNUM  DS    XL1                 LANGUAGE CODE                                
SEQNUM   DS    XL1                 TEXT LINE NUMBER                             
TXTFOUND DS    CL1                 'Y' IF TEXT LINE WAS FOUND                   
ALLSCRNS DS    CL1                 'Y' IF SCREEN IS 'ALL'                       
ALLFLDS  DS    CL1                 'Y' IF FIELD IS 'ALL'                        
*                                                                               
PROFNAME DS    CL3                 PROGRAM NAME                                 
PRFLDNUM DS    XL1                 FIELD NUMBER                                 
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPROG  DS    CL3                                                              
         DS    CL3                                                              
LSTSCRN  DS    CL3                                                              
         DS    CL3                                                              
LSTFIELD DS    CL3                                                              
         DS    CL1                                                              
LSTPAGE  DS    CL3                                                              
         DS    CL2                                                              
LSTHEAD  DS    CL26                                                             
         DS    CL2                                                              
LSTTEXT  DS    CL25                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049CTSFM02S  05/01/02'                                      
         END                                                                    
