*          DATA SET SPTRA1B    AT LEVEL 034 AS OF 10/24/11                      
*PHASE T2161BA                                                                  
*                                                                               
*  TITLE: T2161B - NETWORK TRAFFIC FEED DESCRIPTION MAINTENANCE       *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL MAINTAIN AND LIST RECORDS DESCRIBING   *         
*            THE FEED FOR VARIOUS NETWORK SUBDIVISIONS.               *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: LIST  SCREEN SPTRAAB (T216AB)                              *         
*          MAINT SCREEN SPTRABB (T216BB)                              *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: UPDATED FEED DESCRIPTION RECORDS                          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R5 - WORK REG                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
* LEV 23 JUN18/86 ADD ELEMENT NUMBER TO KEEP DESCRIPTIONS IN ORDER    *         
* LEV 24    MAR12/91 ALLOW CLIENT BLANK TO FUNCTION AS ALL            *         
* LEV 25    APR22/94 ALLOW UPTO 4 DESCRIPTION LINES                   *         
* LEV 26    OCT17/01 ADD OFFICE SECURITY                              *         
* LEV 27 SMUR JAN25/02 ADD FILTER FOR MAP CODE/MAP YEAR               *         
* LEV 29 SMUR MAY14/02 SEE THAT MYKEY HAS THE RIGHT KEY WHEN LIST     *         
* LEV 30 SMUR JUN27/02 CLIENT STRING SECURITY                         *         
* LEV 31 SMUR NOV19/03 BRAND LEVEL SECURITY                           *         
* LEV 32 SMUR JUL26/04 SOX                                            *         
* LEV 33 SMUR JUN21/11 BYPASS RECORDS WITH CLIENTS NOT ON FILE        *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2161B FEED DESCRIPTION RECORD MAINTENANCE'                     
T2161B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2161B*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*MN                                                                             
         CLI   MODE,XRECADD                                                     
         BE    XRADD                                                            
         CLI   MODE,XRECDEL                                                     
         BE    XRDEL                                                            
         CLI   MODE,XRECREST                                                    
         BE    XRREST                                                           
*MN                                                                             
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BNE   *+8                                                              
         BRAS  RE,LR                                                            
*                                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         GOTO1 VSOXERR                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
                                                                                
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VK02     DS    0H                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    NETWORK,NETWORK     NETWORK                                      
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BE    MISSERR             REQUIRED                                     
         BAS   RE,VNET                                                          
*                                                                               
         XC    BCLT,BCLT           CLIENT                                       
         LA    R2,TRACLTH                                                       
         CLI   5(R2),0             TEST CLIENT ENTERED                          
         BE    VK10                                                             
*                                                                               
         CLC   =C'ALL',TRACLT      TEST CLIENT 'ALL'                            
         BNE   VK14                                                             
         CLI   ACTNUM,ACTLIST      'ALL' VALID ONLY FOR LIST ACTION             
         BNE   BADALL                                                           
*                                                                               
VK10     TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK20                 NO                                          
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   ERRX1                                                            
                                                                                
         MVI   ERROR,0                                                          
         B     VK20                                                             
*                                                                               
VK14     DS    0H                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   ERROPT,0                                                         
                                                                                
         CLI   ERROR,0                                                          
         BE    VK20                                                             
                                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    ERRX1                                                            
                                                                                
         CLI   ERROR,SECLOCK       ONLY VALID ERROR SEC-LOCKOUT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BO    ERRX1                                                            
*                                                                               
VK20     XC    FEED,FEED           FEED                                         
         LA    R2,TRAFEEDH                                                      
         CLI   5(R2),0             TEST FEED ENTERED                            
         BE    VK30                                                             
         GOTO1 ANY                                                              
         MVC   FEED,WORK                                                        
         B     VK40                                                             
*                                                                               
VK30     CLI   ACTNUM,ACTLIST      OPTIONAL ONLY FOR LIST ACTION                
         BNE   MISSERR                                                          
*                                                                               
VK40     XC    KEY,KEY             BUILD FEED DESCRIPTION RECORD KEY            
         LA    R4,KEY                                                           
         USING FEEDKEY,R4                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,FEED                                                     
*                                                                               
         MVI   SVACTNUM,0                                                       
*                                                                               
         CLI   ACTNUM,ACTADD         AND ACTION IS ADD                          
         BE    VK50                                                             
         CLI   ACTNUM,ACTREST        OR RESTORE                                 
         BE    VK50                                                             
         CLI   ACTNUM,ACTDEL         OR DELETE                                  
         BNE   EXIT                                                             
*                                                                               
VK50     MVC   SVACTNUM,ACTNUM                                                  
*                                                                               
         CLI   ACTNUM,ACTREST                                                   
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(13),KEYSAVE     SEE IF RECORD ALREADY EXISTS                 
         BNE   VK60                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   MODE,VALREC                                                      
         B     VR                                                               
*                                                                               
VK60     DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    RECNFERR            RECORD NOT FOUND                             
         CLI   ACTNUM,ACTREST                                                   
         BE    RECNFERR            RECORD NOT FOUND                             
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
                                                                                
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
                                                                                
         GOTO1 VSOXERR                                                          
                                                                                
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         USING FEEDKEY,R4                                                       
         MVC   BAGYMD,FEEDKAM                                                   
         MVC   NETWORK,FEEDKNET                                                 
         MVC   BCLT,FEEDKCLT                                                    
         MVC   FEED,FEEDKFD                                                     
         DROP  R4                                                               
*                                                                               
         NI    SVFLAG,X'FF'-(DELRECSW+DELNSSW)                                  
*                                                                               
         CLI   TRAFLTRH+5,0        ANY FILTER ENTERED                           
         BE    VR02                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    VR20                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BE    VR20                                                             
         B     FILTRERR            THEN ERROR                                   
*                                                                               
VR02     CLI   ACTNUM,ACTDEL                                                    
         BE    VR04                                                             
         CLI   ACTNUM,ACTREST                                                   
         BE    VR04                                                             
*                                                                               
         LA    R2,TRADSC1H                                                      
         CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    NODSCERR            REQUIRED                                     
*                                                                               
         LA    R5,TRADSC2H                                                      
         SR    R5,R2               BUMP FACTOR                                  
*                                                                               
VR04     LA    R3,1                                                             
*                                                                               
         CLI   ACTNUM,ACTREST      RESTORE?                                     
         BNE   VR06                                                             
*                                                                               
         TM    KEY+13,X'80'        DELETED RECORD                               
         BO    VR04C                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    NDELERR             RECORD NOT DELETED                           
*                                                                               
VR04C    L     R6,AIO                                                           
         MVI   ELCODE,X'21'        ANY DELETED ELEMS                            
         BRAS  RE,GETEL                                                         
         BNE   RECNFERR            RECORD NOT FOUND                             
*                                                                               
         MVI   0(R6),X'20'         JUST CHANGE ELEMENT CODE                     
         BRAS  RE,NEXTEL                                                        
         BE    *-8                                                              
         B     VR110                                                            
*                                                                               
VR06     CLI   ACTNUM,ACTADD       ADD ?                                        
         BNE   VR08                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    RECEXERR            RECORD EXISTS                                
*                                                                               
VR08     DS    0H                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VR08F                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RECNFERR                                                         
         B     *+8                                                              
VR08C    BRAS  RE,NEXTEL                                                        
         BE    *+12                                                             
         OI    SVFLAG,DELNSSW      DELETE NON-SECTIONAL FEED                    
         B     VR58                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BE    VR08F                                                            
         MVI   0(R6),X'21'         DELETED ELEM ID                              
         B     VR08C                                                            
*                                                                               
VR08F    MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING FEEDELEM,R6                                                      
         XC    ELEM,ELEM                                                        
         MVC   FEEDELID,ELCODE     ELEMENT IDENTIFIER                           
         MVI   FEEDELLN,63         ELEMENT LENGTH                               
         STC   R3,FEEDELNO         ELEMENT NUMBER                               
*                                                                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   FEEDELDS,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR10     LA    R3,1(R3)                                                         
*                                                                               
         LA    R6,ELEM                                                          
         USING FEEDELEM,R6                                                      
         XC    ELEM,ELEM                                                        
         MVC   FEEDELID,ELCODE     ELEMENT IDENTIFIER                           
         MVI   FEEDELLN,63         ELEMENT LENGTH                               
         STC   R3,FEEDELNO         ELEMENT NUMBER                               
*                                                                               
         AR    R2,R5                                                            
         CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BNE   VR15                                                             
*                                                                               
         CLI   ACTNUM,ACTADD       ADD?                                         
         BNE   DR                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     RECORD ALREADY EXISTS ???                    
         BNE   DR                                                               
*                                                                               
         MVI   ACTNUM,ACTCHA       YES, FORCE ACTION TO CHANGE                  
         OI    TRADSC1H+6,X'81'    FIELD MODIFIED FOR NEXT INPUT                
         B     VR110               JUST ADD ELEMS                               
*                                                                               
VR15     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   FEEDELDS,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R1,TRADSCLH                                                      
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BL    VR10                 NO                                          
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BNE   *+12                                                             
         OI    SVFLAG,DELNSSW      DELETE NON-SECTIONAL FEED                    
         B     VR58                                                             
*                                                                               
         CLI   ACTNUM,ACTADD       ADD?                                         
         BNE   DR                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     RECORD ALREADY EXISTS ???                    
         BNE   DR                                                               
*                                                                               
         MVI   ACTNUM,ACTCHA       YES, FORCE ACTION TO CHANGE                  
         OI    TRADSC1H+6,X'81'    FIELD MODIFIED FOR NEXT INPUT                
         B     VR110               JUST ADD ELEMS                               
*                                                                               
* SECTIONAL MAPS FEED (ELEMENT) RECORD                                          
*                                                                               
VR20     DS    0H                                                               
*                                                                               
         LA    R2,TRADSC1H         DESC 1                                       
         LA    R5,TRADSC2H         DESC 2                                       
         SR    R5,R2               BUMP FACTOR                                  
         LA    R1,TRADSCLH         LAST DESC                                    
*                                                                               
* PROTECT DESCRIPTION LINE 2-4                                                  
*                                                                               
VR23     AR    R2,R5               BUMP TO THE NEXT DESC FIELD                  
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BH    VR26                 YES, DONE                                   
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VR23                                                             
*                                                                               
VR26     LA    R3,1                                                             
         MVI   ELCODE,X'40'        SECTIONAL MAP ELEMENT                        
*                                                                               
         LA    R6,ELEM             BUILD NEW ELEMENT INFO IN ELEM               
         USING FEEDSMEL,R6                                                      
         XC    ELEM,ELEM                                                        
         MVC   FEEDSMID,ELCODE     ELEMENT IDENTIFIER                           
         MVI   FEEDSMLN,80         ELEMENT LENGTH                               
         STC   R3,FEEDSMNO         ELEMENT NUMBER                               
*                                                                               
         CLI   SVACTNUM,ACTDEL                                                  
         BE    VR30                                                             
         CLI   SVACTNUM,ACTREST                                                 
         BE    VR30                                                             
*                                                                               
         LA    R2,TRADSC1H                                                      
         GOTO1 ANY                                                              
         MVC   FEEDSMDS,WORK       MOVE FEED DESCRIPTION                        
*                                                                               
VR30     BAS   RE,BMYEAR           GET BINARY MAP YEAR MAP CODE                 
*                                                                               
         MVC   FEEDSMYR,FTRMYR     JUST MOVE IN MAP YEAR                        
*                                                                               
         MVC   FEEDSMCD,FTRMCD     AND MAP CODE                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'KEY),KEY     SAVE KEY                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SMAPRECD,R4                                                      
                                                                                
         MVC   SMKEY(2),=X'0D3D'                                                
         MVC   SMKAGY,BAGYMD       AGENCY/MEDIA                                 
         MVC   SMKYEAR,FTRMYR      YEAR                                         
         MVC   SMKCODE,FTRMCD      MAP CODE                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SMAPERR             NO SECTIONAL MAP ERROR                       
*                                                                               
         MVC   KEY,WORK            RESTORE ORIGINAL KEY                         
         MVC   KEYSAVE(13),KEY                                                  
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL            GET FEED DESCRIPTION                         
         B     *+8                                                              
VR40     BRAS  RE,NEXTEL                                                        
         BE    VR42                                                             
*                                                                               
VR41     CLI   SVACTNUM,ACTREST                                                 
         BE    RECNFERR            RECORD NOT FOUND                             
         CLI   SVACTNUM,ACTDEL                                                  
         BE    RECNFERR                                                         
         CLI   ACTNUM,ACTCHA                                                    
         BE    RECNFERR                                                         
         B     VR47                                                             
*                                                                               
VR42     CLC   0(FEEDSFLG-FEEDSMID,R6),ELEM  SAME MAP YEAR/MAP CODE             
         BNE   VR40                NO, GET NEXT ELEMENT                         
*                                                                               
         CLI   SVACTNUM,ACTADD     IS ACTION ADD                                
         BNE   VR42C                                                            
         TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BZ    RECEXERR                                                         
         MVI   ERROR,DELEXIST      YES, DELETED RECORD EXISTS                   
         B     ERX                                                              
*                                                                               
VR42C    CLI   SVACTNUM,ACTREST    IS ACTION RESTORE                            
         BNE   VR44                                                             
         TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BZ    NDELERR             RECORD NOT DELETED ERROR                     
         B     VR80                                                             
*                                                                               
VR44     CLI   ACTNUM,ACTCHA       IS ACTION CHANGE                             
         BNE   *+16                                                             
         TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BO    RECNFERR                                                         
         B     VR45                                                             
*                                                                               
         CLI   ACTNUM,ACTSEL       IS ACTION CHANGE SELECT                      
         BE    VR45                                                             
*                                                                               
         CLI   SVACTNUM,ACTDEL     IS ACTION DELETE                             
         BE    *+6                                                              
         DC    H'0'                NO, WHAT IS THE ACTION???                    
*                                                                               
         TM    FEEDSFLG,FEEDSDEL   IS THIS FEED ALREADY DELETED                 
         BO    DELERR              RECORD IS ALREADY DELETED                    
         B     VR50                                                             
*                                                                               
VR45     XC    DMCB(24),DMCB                                                    
         GOTO1 VRECUP,DMCB,(C'S',AIO),(R6)  DELETE THIS ELEMENT                 
         B     VR50                                                             
*                                                                               
VR47     MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES RECORD ALREADY EXIST                    
         BE    VR49                                                             
*TEMP    TM    KEY+13,X'80'        DELETE BIT ON?                               
******   BO    VR49                TREAT IT AS RECORD DOES NOT EXIST            
*                                                                               
         CLI   SVACTNUM,ACTDEL     IS ACTION DELETE                             
         BE    RECNFERR                                                         
         CLI   SVACTNUM,ACTREST    IS ACTION RESTORE                            
         BE    RECNFERR                                                         
         B     VR50                                                             
*                                                                               
VR49     MVI   ACTNUM,ACTCHA       YES, FORCE ACTION TO CHANGE                  
         OI    TRADSC1H+6,X'81'    FIELD MODIFIED FOR NEXT INPUT                
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
*                                                                               
VR50     DS    0H                                                               
         CLI   SVACTNUM,ACTDEL     IS ACTION DELETE                             
         BNE   VR80                                                             
         OI    FEEDSFLG,FEEDSDEL   TURN ON DELETE BIT                           
                                                                                
* NOW SEE IF ENTIRE RECORD SHOULD BE DELETED                                    
* (IF ALL ELEMS HAVE DELETE BIT ON)                                             
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    VR70                ONE GOOD ELEMENT FOUND                       
                                                                                
VR58     L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VR60     BRAS  RE,NEXTEL                                                        
         BNE   VR65                                                             
         TM    FEEDSFLG,FEEDSDEL   DELETED ???                                  
         BZ    VR70                NO, DONE                                     
         B     VR60                                                             
                                                                                
VR65     OI    SVFLAG,DELRECSW     DELETE ENTIRE RECORD                         
                                                                                
VR70     LA    R6,ELEM                                                          
         B     VR100                                                            
*                                                                               
VR80     CLI   SVACTNUM,ACTREST      IS ACTION RESTORE                          
         BNE   VR100                                                            
         NI    FEEDSFLG,X'FF'-FEEDSDEL TURN OFF DELETE BIT                      
*                                                                               
VR100    TM    SVFLAG,DELNSSW      DELETE NON-SECTIONAL FEED                    
         BO    VR105                YES                                         
*                                                                               
         CLI   SVACTNUM,ACTDEL     IS ACTION DELETE                             
         BE    VR105                YES                                         
         CLI   SVACTNUM,ACTREST                                                 
         BE    VR115                                                            
*                                                                               
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
*                                                                               
VR105    TM    SVFLAG,DELRECSW     DELETE ENTIRE RECORD                         
         BO    VR130               YES                                          
*                                                                               
         CLC   KEY(13),KEYSAVE     DOES RECORD ALREADY EXIST                    
         BNE   EXIT                NO, RETURN TO GENCON                         
         TM    KEY+13,X'80'                                                     
         BO    EXIT                                                             
*                                                                               
* UPDATE F1 ELEMENT                                                             
*                                                                               
VR110    DS   0H                                                                
         CLI   SVACTNUM,ACTREST                                                 
         BNE   VR115                                                            
         TM    KEY+13,X'80'        DELETED RECORD                               
         BO    VR130                                                            
*                                                                               
VR115    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         GOTO1 REMELEM                                                          
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,ACTVCHDT) RECORD CHANGED DATE               
                                                                                
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         OC    ACTVADDT,ACTVADDT   DATE ADDED THE RECORD ?                      
         BNZ   *+10                 YES, DO NOT OVERWRITE IT                    
         MVC   ACTVADDT,ACTVCHDT   TODAY'S DATE (DATE ADDED)                    
                                                                                
         OC    ACTVADID,ACTVADID   ANY ID THAT ADDED THE RECORD                 
         BNZ   *+10                 YES,DO NOT OVERWRITE                        
         MVC   ACTVADID,TWAORIG    USER ID THAT ADDED RECORD                    
         MVC   ACTVCHID,TWAORIG    USER ID THAT CHANGED REC                     
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR130    MVI   IOOPT,C'Y'                                                       
*                                                                               
         GOTO1 DATAMGR,(R1),=C'PUTREC',=C'SPTFIL',KEY+14,AIO,DMWORK             
*                                                                               
         TM    SVFLAG,DELRECSW     DELETE ENTIRE RECORD                         
         BO    VR133               GIVE CONTROL TO GENCON                       
*                                                                               
         CLI   SVACTNUM,ACTREST                                                 
         BNE   VR135                                                            
         TM    KEY+13,X'80'                                                     
         BZ    VR135                                                            
         MVC   ACTNUM,SVACTNUM     RESTORE ORIGINAL ACTION                      
VR133    MVI   IOOPT,0                                                          
         B     EXIT                                                             
*                                                                               
VR135    CLI   ACTNUM,ACTREST      ACTION RESTORE                               
         BNE   *+12                                                             
         MVI   GERROR+1,8          RECORD RESTORED                              
         B     VR150                                                            
         CLI   SVACTNUM,ACTREST                                                 
         BNE   *+12                                                             
         MVI   GERROR+1,8          RECORD RESTORED                              
         B     VR150                                                            
*                                                                               
         CLI   SVACTNUM,ACTADD     ACTION ADD                                   
         BNE   EXIT                                                             
         CLC   SVACTNUM,ACTNUM     IS IT STILL ADD                              
         BE    EXIT                 YES, DONE                                   
*                                                                               
         MVI   GERROR+1,6          NEW RECORD HAS BEEN ADDED                    
*                                                                               
VR150    OI    GENSTAT2,USMYOK     USE MY OK MESSAGE                            
         MVI   GMSGTYPE,C'I'       INFO MESSAGE                                 
*                                                                               
         GOTO1 VTRAERR                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR05                                                             
*                                                                               
         XC    TRAFLTR,TRAFLTR     CLEAR FILTER FIELD                           
         OI    TRAFLTRH+6,X'80'    TRANSMIT                                     
*                                                                               
         BAS   RE,GTBLENT          GET TABLE ENTRY FOR THIS                     
         CLI   ELCODE,X'20'                                                     
         BE    DR07                                                             
         B     DR60                                                             
*                                                                               
DR05     LA    R2,TRAFLTRH                                                      
         CLI   5(R2),0             ANY FILTER                                   
         BNE   DR30                 YES                                         
*                                                                               
         LA    R2,TRADSC1H         DESC 1                                       
         LA    R5,TRADSC2H         DESC 2                                       
         SR    R5,R2               BUMP FACTOR                                  
         LA    R1,TRADSCLH         LAST DESC                                    
*                                                                               
* UNPROTECT DESCRIPTION LINE 2-4                                                
*                                                                               
DR06     AR    R2,R5               BUMP TO THE NEXT DESC FIELD                  
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BH    DR07                 YES, DONE                                   
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         B     DR06                                                             
*                                                                               
DR07     MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            GET FEED DESCRIPTION                         
         BNE   RECNFERR                                                         
*                                                                               
         USING FEEDELEM,R6                                                      
*                                                                               
         LA    R2,TRADSC1H                                                      
         LA    R5,TRADSC2H                                                      
         SR    R5,R2                                                            
*                                                                               
DR10     MVC   8(60,R2),FEEDELDS                                                
*                                                                               
         CLI   FEEDELLN,63                                                      
         BE    *+10                                                             
         MVC   8(60,R2),FEEDELDS-1                                              
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FEED DESCRIPTION                    
*                                                                               
         BRAS  RE,NEXTEL           GET FEED DESCRIPTION                         
         BNE   DR20                                                             
*                                                                               
         AR    R2,R5               BUMP TO THE NEXT DESC FIELD                  
         LA    R1,TRADSCLH                                                      
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BNH   DR10                 NO                                          
*                                                                               
         DC    H'0'                MORE THAN 4 DESC ELEM                        
*                                                                               
DR20     LA    R1,TRADSCLH                                                      
         CR    R2,R1                                                            
         BE    DR25                                                             
         AR    R2,R5                                                            
         CLC   8(60,R2),SPACES                                                  
         BNH   DR20                                                             
         XC    8(60,R2),8(R2)                                                   
         OI    6(R2),X'80'         TRANSMIT FEED DESCRIPTION                    
         B     DR20                                                             
*                                                                               
DR25     NI    SVFLAG,X'FF'-(MYKEYSW+DISPSW)                                    
         CLI   SVMORE,0            MORE ELEMS WILL FOLLOW                       
         BE    DRX                                                              
         OI    SVFLAG,MYKEYSW      YES, USE MY SAVE KEY                         
         OI    SVFLAG,DISPSW       AND TURN ON DISPLAY SW                       
         B     DRX                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
* FOR ALL OTHER ACTIONS OTHER THAN SELECT                                       
*                                                                               
*                                                                               
DR30     DS    0H                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    DR33                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   FILTRERR            THEN ERROR                                   
*                                                                               
DR33     DS    0H                                                               
         LA    R2,TRADSC1H         DESC 1                                       
         LA    R5,TRADSC2H         DESC 2                                       
         SR    R5,R2               BUMP FACTOR                                  
         LA    R1,TRADSCLH         LAST DESC                                    
*                                                                               
* PROTECT DESCRIPTION LINE 2-4                                                  
*                                                                               
DR35     AR    R2,R5               BUMP TO THE NEXT DESC FIELD                  
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BH    DR38                 YES, DONE                                   
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         B     DR35                                                             
*                                                                               
DR38     BAS   RE,BMYEAR           GET BINARY MAP YEAR                          
         MVI   ELCODE,X'40'        SECTIONAL ELEMENT                            
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR40     BRAS  RE,NEXTEL                                                        
         BE    DR45                                                             
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD                                   
         BE    DRX                                                              
         CLI   SVACTNUM,ACTADD     OR SAVED ACTION ADD                          
         BE    DRX                                                              
         B     RECNFERR            RECORD NOT FOUND                             
*                                                                               
DR45     CLC   FEEDSMYR(9),FTRMYR  COMPARE MAP YEAR/CODE                        
         BNE   DR40                                                             
         TM    FEEDSFLG,FEEDSDEL   IF DELETED                                   
         BO    RECNFERR            RECORD NOT FOUND                             
         B     DR100                                                            
*                                                                               
* FOR ACTION SELECT                                                             
*                                                                               
DR60     ZIC   R5,SVELNUM          ELEMENT NUMBER                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DR70     BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R5,DR70                                                          
*                                                                               
         BAS   RE,PMYEAR           GET PRINTABLE MAP YEAR                       
         MVC   TRAFLTR(2),=C'S='                                                
         MVC   TRAFLTR+2(2),MYEAR                                               
         MVI   TRAFLTR+4,C'/'                                                   
         MVC   TRAFLTR+5(8),FEEDSMCD                                            
         OI    TRAFLTRH+6,X'80'    TRANSMIT                                     
*                                                                               
DR100    MVC   TRADSC1,FEEDSMDS    DESCRIPTION                                  
         OI    TRADSC1H+6,X'80'    TRANSMIT                                     
*                                                                               
         CLI   ELCODE,X'40'        IS THIS SECTIONAL                            
         BNE   DRX                 NO, DONE                                     
*                                                                               
         NI    SVFLAG,X'FF'-(MYKEYSW+DISPSW)                                    
         CLI   SVMORE,0            MORE SECTIONALS FOR NEXT SCREEN?             
         BE    *+8                                                              
         OI    SVFLAG,MYKEYSW      YES, USE MY KEY FOR LIST                     
         OI    SVFLAG,DISPSW       AND TURN ON DISPLAY SW                       
*                                                                               
         LA    R2,TRADSC1H         DESC 1                                       
         LA    R5,TRADSC2H         DESC 2                                       
         SR    R5,R2               BUMP FACTOR                                  
         LA    R1,TRADSCLH         LAST DESC                                    
*                                                                               
* PROTECT DESCRIPTION LINE 2-4                                                  
*                                                                               
DR120    AR    R2,R5               BUMP TO THE NEXT DESC FIELD                  
         CR    R2,R1               ARE WE PAST LAST DISCRIPTION LINE            
         BH    DRX                  YES, DONE                                   
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         B     DR120                                                            
*                                                                               
DRX      B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING FEEDKEY,R4                                                       
*                                                                               
         MVC   TRANET,FEEDKNET                                                  
         OI    TRANETH+6,X'80'     NETWORK                                      
         GOTO1 CLUNPK,DMCB,FEEDKCLT,TRACLT                                      
         OI    TRACLTH+6,X'80'     CLIENT                                       
         MVC   TRAFEED,FEEDKFD                                                  
         OI    TRAFEEDH+6,X'80'    FEED                                         
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*MN                                                                             
*---------------------------------------------------------*                     
* ADD PASSIVE KEY                                                               
*---------------------------------------------------------*                     
XRADD    DS    0H                                                               
         MVC   SVDA,KEY+0                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FEEDKPAS,R4                                                      
         MVC   FDPSKID(2),=X'0AAB'                                              
         MVC   FDPSKAM,BAGYMD                                                   
         MVC   FDPSKCLT,BCLT                                                    
         MVC   FDPSKNET,NETWORK                                                 
         MVC   FDPSKFD,FEED                                                     
         MVC   KEY+14(4),SVDA                                                   
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------*                     
* DELETE PASSIVE KEY                                                            
*---------------------------------------------------------*                     
XRDEL    DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R4,KEYSAVE                                                       
         USING FEEDKPAS,R4                                                      
         MVC   FDPSKID(2),=X'0AAB'                                              
         MVC   FDPSKAM,BAGYMD                                                   
         MVC   FDPSKCLT,BCLT                                                    
         MVC   FDPSKNET,NETWORK                                                 
         MVC   FDPSKFD,FEED                                                     
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XRDXIT                                                           
                                                                                
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
                                                                                
         DC    H'0'                                                             
XRDXIT   B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------*                     
* RESTORE PASSIVE KEY                                                           
*---------------------------------------------------------*                     
XRREST   DS    0H                                                               
         MVC   SVDA,KEY+14                                                      
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R4,KEYSAVE                                                       
         USING FEEDKPAS,R4                                                      
         MVC   FDPSKID(2),=X'0AAB'                                              
         MVC   FDPSKAM,BAGYMD                                                   
         MVC   FDPSKCLT,BCLT                                                    
         MVC   FDPSKNET,NETWORK                                                 
         MVC   FDPSKFD,FEED                                                     
                                                                                
         OI    DMINBTS,X'08'     READ FOR DELETES                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEYSAVE,KEY,0         
         NI    DMINBTS,X'F7'     TURN OFF READ FOR DELETES                      
         CLC   KEY(13),KEYSAVE   IF NOT FOUND I NEED TO ADD IT                  
         BNE   XRR050                                                           
                                                                                
         NI    KEY+13,X'7F'      UNDELETE PASSIVE POINTER                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    XRRXIT                                                           
         DC    H'0'                                                             
                                                                                
XRR050   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FEEDKPAS,R4                                                      
         MVC   FDPSKID(2),=X'0AAB'                                              
         MVC   FDPSKAM,BAGYMD                                                   
         MVC   FDPSKCLT,BCLT                                                    
         MVC   FDPSKNET,NETWORK                                                 
         MVC   FDPSKFD,FEED                                                     
         MVC   KEY+14(4),SVDA                                                   
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
XRRXIT   B     EXIT                                                             
         EJECT                                                                  
*MN                                                                             
*                                                                               
* GET THIS TABLE ENTRY                                                          
*                                                                               
GTBLENT  NTR1                                                                   
         LA    RF,TABLELST         POINT TO TABLE                               
         USING TBLENTD,RF                                                       
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         ZIC   R3,SELLISTN         LINE # SELECTED                              
*                                                                               
         SR    R2,R2                                                            
         LA    RE,L'TBLENT         LENGTH OF EACH TABLE ENTRY                   
         MR    R2,RE               LINE# * ENTRY LENGTH                         
         LA    RF,0(R3,RF)         BUMP IN TABLE                                
*                                                                               
         OC    0(L'TBLENT,RF),0(RF) SHOULD NOT BE EMPTY                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MYKEY,TBLKEY                                                     
         MVC   SVMORE,TBLFLAG                                                   
         MVC   SVDISK,TBLDISK      DISK ADDRESS                                 
         CLC   TBLLNUM,SELLISTN                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELCODE,TBLELCD      ELEMENT CODE                                 
         MVC   SVELNUM,TBLELNUM    ELEMENT NUMBER                               
         MVC   SVELCOD,TBLELCD                                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CONVER MAP YEAR TO BINARY                                                     
*                                                                               
BMYEAR   NTR1                                                                   
*                                                                               
         MVI   FTRMYR,0            INIT MAP YEAR                                
         XC    FTRMCD,FTRMCD       INIT MAP CODE                                
*                                                                               
         GOTO1 SCANNER,DMCB,(X'0B',TRAFLTRH),WORK                               
         CLI   4(R1),0                                                          
         BE    MAPERROR            MAP ERROR                                    
*                                                                               
         CLI   WORK+12,C'S'        FILTER: S=YY/CCCCCCCC?                       
         BNE   MAPERROR             NO ERROR                                    
         CLI   WORK+24,C'/'                                                     
         BNE   MAPERROR                                                         
*                                                                               
         XC    FULL,FULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+22      YEAR                                         
         MVC   DUB+2(4),=C'0101'   JAN01                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
*                                                                               
         MVC   FTRMYR,FULL         SAVE FILTER MAP YEAR                         
         MVC   FTRMCD,WORK+25      SAVE FILTER MAP CODE                         
         OC    FTRMCD,SPACES                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
                                                                                
*                                                                               
* GET PRINTABLE MAP CODE                                                        
*                                                                               
PMYEAR   NTR1                                                                   
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),FEEDSMYR     MAP YEAR                                     
         MVC   DUB+1(2),=X'0101'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(X'20',MYEAR)                                
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
*                                                                               
         B     EXIT                                                             
                                                                                
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         GOTO1 ERREX2                                                           
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
ERRX1    GOTO1 ERREX                                                            
*                                                                               
RECEXERR MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
ERX      B     ERRX1                                                            
*                                                                               
DELERR   MVI   ERROR,RECISDEL      RECORD IS ALREADY DELETED                    
         B     ERRX1                                                            
*                                                                               
NDELERR  MVI   ERROR,RECNTDEL      RECORD NOT DELETED                           
         B     ERRX1                                                            
*                                                                               
FILTRERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FILTRMS),FILTRMS                                       
         GOTO1 ERREX2                                                           
FILTRMS  DC    C'* ERROR * DDS FUTURE USE'                                      
*                                                                               
SMAPERR  LA    R2,TRAFLTRH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SMAPMS),SMAPMS                                         
         GOTO1 ERREX2                                                           
SMAPMS   DC    C'* ERROR * SMAP RECORD NOT FOUND'                               
*                                                                               
NODSCERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODSCMS),NODSCMS                                       
         GOTO1 ERREX2                                                           
NODSCMS  DC    C'* ERROR * FEED DESCRIPTION REQUIRED *'                         
*                                                                               
RECNFERR MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERRX1                                                            
*                                                                               
MAPERROR LA    R2,TRAFLTRH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAPERMS),MAPERMS                                       
         GOTO1 ERREX2                                                           
MAPERMS  DC    C'* ERROR * INVALID FILTER: S=YY/CCCCCCCC'                       
*                                                                               
BADALL   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADALLMS),BADALLMS                                     
         GOTO1 ERREX2                                                           
BADALLMS DC    C'* ERROR * ''ALL'' VALID ONLY FOR LIST ACTION *'                
*                                                                               
         DROP  RB,RC,R4                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ONLINE LIST ROUTINE                                                           
*                                                                               
LR       NMOD1 0,***LR***                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING FEEDKEY,R4                                                       
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LR00                                                             
*                                                                               
         TM    SVFLAG,MYKEYSW      USE MY KEY FOR LIST                          
         BZ    LR00                                                             
         NI    SVFLAG,X'FF'-MYKEYSW                                             
*                                                                               
         CLC   =X'0A2B',MYKEY      IS THIS THE KEY                              
         BNE   LR00                                                             
*                                                                               
         MVC   KEY,MYKEY           RESTORE MY KEY                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         ZIC   R5,SVELNUM          ELEMENT NUMBER                               
         TM    SVFLAG,DISPSW       RETURNING FROM DISPLAY                       
         BZ    LRA                                                              
         NI    SVFLAG,X'FF'-DISPSW                                              
         CLI   LISTNUM,0           NEW SCREEN                                   
         BE    LRA                                                              
         LA    R5,1(R5)            SHOW NEXT ELEMENT                            
LRA      STC   R5,ELNUM                                                         
*                                                                               
         CLI   SVELCOD,X'20'                                                    
         BNE   *+8                                                              
         MVI   SVELCOD,X'40'       NOW SHOW SECTIONAL FEED                      
*                                                                               
         MVC   ELCODE,SVELCOD                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LRNXT    BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R5,LRNXT                                                         
         B     LR68                                                             
*                                                                               
LR00     CLI   TRAFLTRH+5,0        WAS FILTER ENTERED                           
         BE    LR02                 NO                                          
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    LR02                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   FILTRER1            THEN ERROR                                   
*                                                                               
LR02     OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVI   SVELCOD,0                                                        
         MVI   SVELNUM,0                                                        
         XC    MYKEY,MYKEY                                                      
         MVI   SVFLAG,0                                                         
         MVI   SVMORE,0                                                         
*                                                                               
         MVC   FEEDKID,=X'0A2B'    FEED DESCRIPTION RECORD KEY                  
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,FEED                                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
LR30     CLC   KEY(7),SAVEKEY      TEST SAME TYPE/AGENCY/MEDIA/NTWK             
         BNE   LRX                                                              
*                                                                               
         OC    SAVEKEY+7(2),SAVEKEY+7     TEST CLIENT ENTERED                   
         BZ    LR34                                                             
*                                                                               
         CLC   FEEDKCLT,SAVEKEY+7  IF SO, TEST KEY MATCH                        
         BNE   LRX                                                              
*                                                                               
LR34     DS    0H                                                               
         OC    SAVEKEY+9(4),SAVEKEY+9     TEST FEED ENTERED                     
         BZ    LR40                                                             
         CLC   FEEDKFD,SAVEKEY+9   IF SO, TEST KEY MATCH                        
         BNE   LR20                                                             
*                                                                               
LR40     DS   0H                                                                
         OC    FEEDKCLT,FEEDKCLT   NULL CLIENT?                                 
         BZ    LR50                 YES, NO SECURITY CHECK                      
                                                                                
         MVC   SVKEY,KEY                                                        
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         GOTO1 CLUNPK,DMCB,FEEDKCLT,FLD                                         
         LA    R2,FLDH                                                          
                                                                                
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0                                                          
                                                                                
         GOTO1 VALICLT                                                          
                                                                                
         MVI   ERROPT,0                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         CLI   ERROR,0                                                          
         BE    LR46                                                             
                                                                                
*NOP     CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
*        BE    *+6                                                              
*******  DC    H'0'                                                             
                                                                                
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    LR44                NO, GET NEXT CLIENT                          
                                                                                
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    LR46                OFFICES MATCH, OK TO LIST                    
                                                                                
LR44     MVI   FEEDKCLT+3,X'FF'    FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         B     LR30                                                             
*                                                                               
LR46     DS   0H                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR50     DS   0H                                                                
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
*                                                                               
         MVI   ELNUM,0             INIT ELEMENT COUNTER                         
         MVI   SVMORE,0            INIT MORE ELEMS TO FOLLOW FLAG               
*                                                                               
         CLI   TRAFLTRH+5,0        ANY FILTER                                   
         BE    *+12                                                             
         BAS   RE,BMYEAR1          GET BINARY MAP YEAR                          
         B     LR60                AND BYPASS X'20' ELEMS                       
*                                                                               
         MVI   ELCODE,X'20'        FEED DESCRIPTION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    LR70                                                             
*                                                                               
LR60     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        SECTIONAL FEED DESCRIPTION                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LR65     BRAS  RE,NEXTEL                                                        
         BNE   LR20                                                             
*                                                                               
         ZIC   R1,ELNUM                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ELNUM                                                         
*                                                                               
         CLI   TRAFLTRH+5,0        WAS FILTER ENTERED                           
         BE    LR70                 NO                                          
*                                                                               
         CLC   FEEDSMYR,FTRMYR     DOES MAP YEAR MATCH                          
         BNE   LR65                                                             
                                                                                
*                                                                               
         CLC   FEEDSMCD,FTRMCD     DOES MAP CODE MATCH                          
         BNE   LR65                                                             
*                                                                               
LR68     TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BO    LR65                YES, GET NEXT ELEMENT                        
*                                                                               
LR70     DS    0H                                                               
         MVI   SVMORE,0                                                         
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LR120                                                            
*                                                                               
         CLI   ELCODE,X'40'                                                     
         BE    LR73                                                             
*                                                                               
* SEE IF THERE ARE SECTIONAL ELEMENTS THAT FOLLOW THIS ONE                      
*                                                                               
         MVI   ELCODE,X'40'                                                     
         LR    R5,R6                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   SVMORE,X'FF'                                                     
         MVI   ELCODE,X'20'        RESTORE ELCODE                               
         B     LR85                                                             
*                                                                               
LR73     LR    R5,R6               SAVE R6 POINTER                              
*                                                                               
LR75     BRAS  RE,NEXTEL           DO A LOOK AHEAD FOR MORE ELEMS               
         BNE   LR85                NO MORE ELEMS                                
*                                                                               
         CLI   TRAFLTRH+5,0        ANY FILTER ENTERED                           
         BE    LR80                 NO                                          
*                                                                               
         CLC   FEEDSMYR,FTRMYR     DOES MAP YEAR MATCH                          
         BNE   LR75                                                             
                                                                                
*                                                                               
         CLC   FEEDSMCD,FTRMCD     DOES MAP CODE MATCH                          
         BNE   LR75                                                             
*                                                                               
LR80     MVI   SVMORE,X'FF'        SET FLAG MORE ELEMS TO FOLLOW                
*                                                                               
LR85     LR    R6,R5               RESTORE R6                                   
         BAS   RE,TBLADD           ADD ENTRY TO THE TABLE                       
*                                                                               
LR100    CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   LR120                                                            
         BAS   RE,LRL                                                           
         CLI   ELCODE,X'40'        ARE WE DOING SECTIONALS                      
         BE    LR65                                                             
         B     LR60                                                             
*                                                                               
LR120    CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,LRR                                                           
         CLI   ELCODE,X'40'        ARE WE DOING SECTIONALS                      
         BE    LR20                                                             
         B     LR60                                                             
                                                                                
LRX      XIT1                                                                   
         EJECT                                                                  
* ONLINE LIST *                                                                 
                                                                                
         USING FEEDELEM,R6                                                      
                                                                                
LRL      NTR1                                                                   
         CLI   ELCODE,X'40'        SECTIONAL MAP DESC ELEMENT                   
         BNE   LRL10                                                            
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BO    LRLX                 YES                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
LRL10    OI    GLSTSTAT,RETEXTRA   EXTRA CALL AFTER LIST-MON                    
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         MVC   LSTNET,FEEDKNET                                                  
         GOTO1 CLUNPK,DMCB,FEEDKCLT,LSTCLT                                      
         MVC   LSTFEED,FEEDKFD                                                  
                                                                                
         CLI   ELCODE,X'40'        SECTIONAL MAP DESC ELEMENT                   
         BE    LRL20                                                            
*                                                                               
         USING FEEDELEM,R6                                                      
*                                                                               
         MVC   LSTDESC,FEEDELDS                                                 
*                                                                               
         CLI   FEEDELLN,63                                                      
         BE    *+10                                                             
         MVC   LSTDESC,FEEDELDS-1                                               
         B     LRL50                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
LRL20    DS    0H                                                               
         BAS   RE,PMYEAR1                                                       
*                                                                               
         MVC   LFILTER(2),MYEAR                                                 
         MVI   LFILTER+2,C'/'                                                   
         MVC   LFILTER+3(8),FEEDSMCD                                            
         MVC   LSTDESC,FEEDSMDS                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
LRL50    GOTO1 LISTMON             SEND LINE TO SCREEN                          
         BE    LRLX                NO END OF SCREEN                             
*                                                                               
         NI    SVFLAG,X'FF'-MYKEYSW                                             
         CLI   SVMORE,0            MORE SECTIONALS FOR NEXT SCREEN?             
         BE    LRLX                                                             
*                                                                               
         MVC   MYKEY,KEY           YES, THIS IS THE RECORD WE WANT              
         OI    SVFLAG,MYKEYSW      USE MY KEY FOR LIST                          
*                                                                               
LRLX     XIT1                                                                   
*                                                                               
                                                                                
* OFFLINE LIST *                                                                
*                                                                               
                                                                                
LRR      NTR1                                                                   
*                                                                               
         CLI   ELCODE,X'40'        SECTIONAL MAP DESC ELEMENT                   
         BNE   LRR05                                                            
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL   IS IT DELETED                                
         BO    LRRX                 YES                                         
*                                                                               
LRR05    MVC   PNET,FEEDKNET                                                    
         OC    FEEDKCLT,FEEDKCLT                                                
         BZ    LRR10                                                            
         GOTO1 CLUNPK,DMCB,FEEDKCLT,PCLT                                        
*                                                                               
LRR10    MVC   PFEED,FEEDKFD                                                    
*                                                                               
         CLI   ELCODE,X'40'        SECTIONAL                                    
         BE    LRR30                YES                                         
*                                                                               
         USING FEEDELEM,R6                                                      
*                                                                               
         LA    R3,PFDESC                                                        
*                                                                               
LRR15    MVC   0(60,R3),FEEDELDS                                                
         CLI   FEEDELLN,63                                                      
         BE    *+10                                                             
         MVC   0(60,R3),FEEDELDS-1                                              
*                                                                               
         BRAS  RE,NEXTEL           SEE IF LINE 2 OF FEED DESCRIPTION            
         BNE   LRR20               NO                                           
*                                                                               
         LA    R3,132(R3)                                                       
         B     LRR15                                                            
*                                                                               
LRR20    GOTO1 SPOOL,DMCB,(R8)     SEND LINE TO PRINT                           
         B     LRRX                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
LRR30    LA    R3,PFILTER                                                       
         LA    R5,PFDESC                                                        
         LA    R1,4                4 PRINT LINES                                
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
LRR40    BAS   RE,PMYEAR1                                                       
*                                                                               
         MVC   0(2,R3),MYEAR       MAP YEAR                                     
         MVI   2(R3),C'/'                                                       
         MVC   3(8,R3),FEEDSMCD    MAPCODE                                      
         MVC   0(L'FEEDSMDS,R5),FEEDSMDS  FEED DESCRIPTION                      
*                                                                               
         LA    R3,132(R3)                                                       
         LA    R5,132(R5)                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1               ANY MORE PRINT LINES                         
         BNZ   LRR50                YES                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND LINE TO PRINT                           
*                                                                               
         LA    R3,PFILTER                                                       
         LA    R5,PFDESC                                                        
         LA    R1,4                4 PRINT LINES                                
*                                                                               
LRR50    BRAS  RE,NEXTEL           ANY MODE SECTIONALS                          
         BE    LRR40                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND LINE TO PRINT                           
*                                                                               
LRRX     XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* ADD THIS ENTRY TO TABLE                                                       
*                                                                               
TBLADD   NTR1                                                                   
         LA    RF,TABLELST         POINT TO TABLE                               
         USING TBLENTD,RF                                                       
*                                                                               
         ZIC   R3,LISTNUM          LINE # ON THE SCREEN                         
         LTR   R3,R3               NEW SCREEN                                   
         BNZ   *+16                                                             
         XC    TABLELST(200),TABLELST   YES, CLEAR TABLE                        
         XC    TABLELST+200(136),TABLELST+200                                   
*                                                                               
         SR    R2,R2                                                            
         LA    RE,L'TBLENT         LENGTH OF EACH TABLE ENTRY                   
         MR    R2,RE               LINE# * ENTRY LENGTH                         
         LA    RF,0(R3,RF)         BUMP IN TABLE                                
*                                                                               
         OC    0(L'TBLENT,RF),0(RF) SHOULD BE EMPTY                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TBLKEY,KEY          SAVE KEY IN TABLE                            
         MVC   TBLDISK,KEY+14           DISK ADDRESS                            
         MVC   TBLLNUM,LISTNUM     LINE NUMBER ON THE SCREEN                    
         MVC   TBLELCD,ELCODE      ELEMENT CODE                                 
         MVC   TBLELNUM,ELNUM      ELEMENT NUMBER (ZERO=NOT SECTIONAL)          
         MVC   TBLFLAG,SVMORE      MORE ELEMS TO FOLLOW                         
*                                                                               
         MVC   SVELNUM,ELNUM       SAVE ELEMENT NUMBER                          
         MVC   SVELCOD,ELCODE      SAVE ELCODE                                  
         MVC   MYKEY,KEY           SAVE THIS KEY                                
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CONVER MAP YEAR TO BINARY                                                     
*                                                                               
BMYEAR1  NTR1                                                                   
*                                                                               
         MVI   FTRMYR,0            INIT MAP YEAR                                
         XC    FTRMCD,FTRMCD       INIT MAP CODE                                
*                                                                               
         GOTO1 SCANNER,DMCB,(X'0B',TRAFLTRH),WORK                               
         CLI   4(R1),0                                                          
         BE    MPERROR             MAP ERROR                                    
*                                                                               
         CLI   WORK+12,C'S'        FILTER: S=YY/CCCCCCCC?                       
         BNE   MPERROR             NO ERROR                                     
         CLI   WORK+24,C'/'                                                     
         BNE   MPERROR                                                          
*                                                                               
         XC    FULL,FULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+22      YEAR                                         
         MVC   DUB+2(4),=C'0101'   JAN01                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
*                                                                               
         MVC   FTRMYR,FULL         SAVE FILTER MAP YEAR                         
         MVC   FTRMCD,WORK+25      SAVE FILTER MAP CODE                         
         OC    FTRMCD,SPACES                                                    
*                                                                               
         B     LRRX                                                             
*                                                                               
                                                                                
*                                                                               
* GET PRINTABLE MAP CODE                                                        
*                                                                               
PMYEAR1  NTR1                                                                   
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),FEEDSMYR     MAP YEAR                                     
         MVC   DUB+1(2),=X'0101'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(X'20',MYEAR)                                
*                                                                               
         B     LRRX                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FILTRER1 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FILTRMS1),FILTRMS1                                     
         GOTO1 ERREX2                                                           
FILTRMS1 DC    C'* ERROR * DDS FUTURE USE'                                      
*                                                                               
MPERROR  LA    R2,TRAFLTRH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MPERMS),MPERMS                                         
         GOTO1 ERREX2                                                           
MPERMS   DC    C'* ERROR * INVALID FILTER: S=YY/CCCCCCCC'                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,36,C'NETWORK FEED LIST'                                       
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,8,C'NETWORK'                                                  
         SSPEC H9,8,C'-------'                                                  
         SSPEC H8,22,C'CLIENT'                                                  
         SSPEC H9,22,C'------'                                                  
         SSPEC H8,37,C'FEED'                                                    
         SSPEC H9,37,C'----'                                                    
         SSPEC H8,58,C'FEED DESCRIPTION'                                        
         SSPEC H9,58,C'----------------'                                        
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE DDACTIVD                                                       
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSMAP                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRABBD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
*                                                                               
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
NETWORK  DS    CL4                                                              
FEED     DS    CL4                                                              
SAVEKEY  DS    XL13                                                             
SVMORE   DS    XL1                 FF=MORE ELEMS TO FOLLOW                      
*                                                                               
SVFLAG   DS    XL1                                                              
MYKEYSW  EQU   X'80'               USE MY KEY FOR LIST                          
DISPSW   EQU   X'40'               DISPLAY SWITCH                               
DELRECSW EQU   X'20'               DELETE ENTIRE RECORD                         
DELNSSW  EQU   X'10'               DELETE NON-SECTIONAL FEED                    
*                                                                               
MYKEY    DS    XL13                                                             
ELNUM    DS    XL1                 ELEMENT COUNTER                              
SVELCOD  DS    XL1                 SAVE ELCODE                                  
SVELNUM  DS    XL1                 SAVE ELEMENT NUMBER                          
SVDISK   DS    XL4                                                              
SVACTNUM DS    XL1                 SAVE ACTION                                  
*                                                                               
* KEEP MAP YEAR AND MAP CODE TOGETHER AND IN ORDER                              
*                                                                               
FTRMYR   DS    XL1                 FILTER MAP YEAR                              
FTRMCD   DS    XL8                 FILTER MAP CODE                              
*                                                                               
MYEAR    DS    XL6                 MAP YEAR EBCDIC                              
TABLELST DS    CL(16*L'TBLENT)  HOLDS UPTO 16 LIST ENTRIES                      
         DS    0H                                                               
*MN                                                                             
SVDA     DS    F                                                                
*MN                                                                             
                                                                                
TBLENTD  DSECT                                                                  
*                                                                               
TBLENT   DS    0CL(TBLNEXT-TBLKEY)                                              
TBLKEY   DS    CL13                KEY                                          
TBLDISK  DS    CL4                 DISK ADDRESS                                 
TBLLNUM  DS    XL1                 RELATIVE SELECT LINE #                       
TBLELCD  DS    XL1                 ELEMENT CODE                                 
TBLELNUM DS    XL1                 ELEMENT NUMBER (ZERO=NOT SECTIONAL)          
TBLFLAG  DS    XL1                 FF=MORE ELEMENTS TO FOLLOW                   
TBLNEXT  EQU   *                                                                
                                                                                
                                                                                
* OFFLINE REPORT LINE                                                           
                                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL9                                                              
PNET     DS    CL4                                                              
         DS    CL10                                                             
PCLT     DS    CL3                                                              
         DS    CL10                                                             
PFEED    DS    CL4                                                              
         DS    CL3                                                              
PFILTER  DS    CL11                                                             
         DS    CL3                                                              
PFDESC   DS    CL60                                                             
                                                                                
* ONLINE LIST LINE                                                              
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTNET   DS    CL4                                                              
         DS    CL1                                                              
LSTCLT   DS    CL3                                                              
         DS    CL1                                                              
LSTFEED  DS    CL4                                                              
         DS    CL3                                                              
LFILTER  DS    CL11                                                             
         DS    CL2                                                              
LSTDESC  DS    CL44                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPTRA1B   10/24/11'                                      
         END                                                                    
