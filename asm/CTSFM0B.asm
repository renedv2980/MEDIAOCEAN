*          DATA SET CTSFM0B    AT LEVEL 010 AS OF 05/01/02                      
*PHASE TA0A0BA                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A0B - MAINTENANCE/LIST OF PHASE RECORDS                  *         
*                                                                     *         
*  COMMENTS: MAINTAINS PHASE RECORDS.                                 *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMFB (TA0AFB) -- MAINTENANCE                    *         
*                  CTSFMEB (TA0AEB) -- LIST                           *         
*                  CTSFMDB (TA0ADB) -- REPORT                         *         
*                  CTSFMCB (TA0ACB) -- COPY                           *         
*                                                                     *         
*  OUTPUTS: UPDATED PHASE RECORDS                                     *         
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
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A0B MAINTENANCE OF PHASE RECORDS'                            
TA0A0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A0B*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         XC    DMCB,DMCB           CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A03' GET ADDR OF DAYVAL VIA CALLOV             
         GOTO1 CALLOV,DMCB         RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         BNE   *+6                 NO, IT'S IN 1ST PARAMETER                    
         DC    H'0'                YES, DIE HORRIBLY                            
         MVC   VDAYVAL,DMCB        SAVE THAT ADDRESS FOR LATER                  
         ST    R3,RELO                                                          
*                                                                               
         L     R1,SYSPARMS         A(PARAMS)                                    
         L     R1,0(R1)            A(SYSFACS)                                   
         USING SYSFACD,R1                                                       
         MVC   ASELIST,VSELIST     A(SYSTEM EXECUTIVE LIST)                     
         DROP  R1                                                               
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   USEHDHK,C'Y'                                                     
         MVI   RECFOUND,C'N'                                                    
         MVI   ACTELOPT,C'Y'       WE WANT AN ACTIVITY ELEMENT                  
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET RETURN BLOCK FOR FACTSD                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R3,FAALANG          ADDRESS OF LANGUAGE TABLE                    
         ST    R3,AFAALANG                                                      
         ZIC   R3,FASYSID          GET FACPACK SYSTEM ID                        
         STC   R3,FACPCKID                                                      
         DROP  R1                  NO NEED FOR THE DSECT RIGHT NOW              
*                                                                               
         OI    CONSERVH+1,X'01'                                                 
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE THE RECORD?                           
         BE    DELREC                                                           
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   EXIT                                                             
         GOTO1 =A(PR),DMCB,(RC),RR=RELO                                         
*                                                                               
EXIT     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
*                                                                               
* VALIDATE THE PHASE                                                            
*                                                                               
VK       CLI   ACTNUM,ACTREP       REPORT ACTION?                               
         BE    VKWEEK              START CHECKING THE WEEKS USER WANTS          
*                                                                               
         CLI   ACTNUM,ACTCOPY      COPY ACTION?                                 
         BNE   VKPHSE10            NO                                           
         LA    R2,SFCPHS1H                                                      
         CLI   5(R2),4             MUST HAVE 'TSPP', ONLY OVERLAYS ARE          
         BL    INVLFLD                 COPIED                                   
         CLI   8(R2),C'T'          CAN ONLY COPY ON-LINE PHASES                 
         BNE   INVLFLD                                                          
         B     VKPHSE30                                                         
*                                                                               
VKPHSE10 CLI   ACTNUM,ACTLIST      UNDER LIST MODE?                             
         BNE   VKPHSE20                                                         
         LA    R2,SFLPHSEH                                                      
         CLI   5(R2),1             ONE CHARACTER OR BLANK?                      
         BNH   VKLANG              YES, PHASE CAN BE ON OR OFF LINE             
         B     VKPHSE30            NO, VALIDATE IF ON-LINE                      
*                                                                               
VKPHSE20 LA    R2,SFMPHSEH         IT'S ONE OF THE MAINTENANCE ACTIONS          
         CLI   5(R2),0             NEED SOMETHING FOR THE PHASE NAME            
         BE    MISSFLD                                                          
         CLI   8(R2),C'T'          ON-LINE PHASE?                               
         BNE   VKLANG              NO, NOTHING TO VALIDATE                      
         CLI   5(R2),L'SFMPHSE     YES, MUST BE FULL TSPPOO FORMAT              
         BNE   INVLFLD                                                          
*                                                                               
VKPHSE30 CLI   8(R2),C'T'          ON-LINE PHASE?                               
         BNE   VKLANG              NO, NOTHING TO VALIDATE                      
*                                                                               
         LA    R5,9(R2)            YES, SKIP THE BEGINNING T                    
         ZIC   R4,5(R2)            LOAD LENGTH                                  
         BCTR  R4,0                NUMBER OF HEX NUMBERS TO VALIDATE            
         GOTO1 HEXIN,DMCB,(R5),DUB,(R4)                                         
         CLI   DMCB+15,0           INVALID HEX STRING? (PARA 4, BYTE 4)         
         BE    INVLFLD             YES                                          
         B     VKLANG              VALIDATE THE LANGUAGE                        
         EJECT                                                                  
*                                                                               
* VALIDATE THE ALL RECORDS FIELD                                                
*                                                                               
VKWEEK   LA    R2,SFRWEEKH          POINT TO THE ALL RECORDS FIELD              
         CLI   5(R2),0              WAS THERE ANY INPUT                         
         BNE   VKWEEK1              YES                                         
         MVI   SFRWEEK,C'Y'         DEFAULT TO YES, ONLY 2 WEEKS                
         B     VKCORE                                                           
VKWEEK1  CLI   SFRWEEK,C'N'         USER WANTS ALL                              
         BE    VKCORE                                                           
         CLI   SFRWEEK,C'Y'         USER WANTS LAST 2 WEEKS                     
         BNE   INVLFLD              INVALID INPUT                               
*                                                                               
* VALIDATE THE CORE RESIDENT FIELD                                              
*                                                                               
VKCORE   LA    R2,SFRCOREH          POINT TO THE CORE RESIDENT FIELD            
         CLI   5(R2),0              WAS THERE ANY INPUT                         
         BNE   VKCORE1              YES                                         
         MVI   CORERES,C'N'         DEFAULT TO NO                               
         B     VKLANG                                                           
VKCORE1  CLI   SFRCORE,C'Y'         USER WANTS ONLY CORE RESIDENT               
         BNE   *+12                                                             
         MVI   CORERES,C'Y'                                                     
         B     VKLANG                                                           
         CLI   SFRCORE,C'N'         USER WANTS ALL                              
         BNE   INVLFLD              INVALID INPUT                               
         MVI   CORERES,C'N'                                                     
*                                                                               
* VALIDATE THE LANGUAGE                                                         
*                                                                               
VKLANG   LA    R2,SFMLANGH         POINT TO THE LANGUAGE FIELD                  
         CLI   ACTNUM,ACTCOPY      CODE FOR COPY ACTION                         
         BNE   *+12                                                             
         LA    R2,SFCLNG1H                                                      
         B     VKLANGDF                                                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+12                NO, REGULAR OR REPORT                        
         LA    R2,SFLLANGH                                                      
         B     VKLANGDF                                                         
         CLI   ACTNUM,ACTREP       REPORT?                                      
         BNE   VKLANGDF            NO, CHECK FOR DEFAULT                        
         LA    R2,SFRLANGH                                                      
VKLANGDF CLI   5(R2),0             NO LANGUAGE ENTERED?                         
         BNE   VKLANG1             THERE IS ONE ENTERED ALREADY                 
         MVC   8(3,R2),=C'ENG'     DEFAULT IS ENGLISH                           
         MVI   5(R2),L'SFMLANG     DEFAULT INPUT LENGTH                         
         OI    6(R2),X'80'         DISPLAY THE DEFAULT                          
*VKLANG1  CLI   5(R2),3             3 CHARACTER LANGUAGE CODE?                  
*         BNE   INVLFLD                                                         
VKLANG1  CLC   =C'ALL',8(R2)       ALL LANGUAGES?                               
         BNE   VKLANG2                                                          
         CLI   ACTNUM,ACTLIST      ALL LANGUAGES IN LIST MODE?                  
         BE    VKLNGALL            YES                                          
         CLI   ACTNUM,ACTREP       ALL LANGUAGES IN REPORT MODE?                
         BNE   INVLFLD             YES                                          
VKLNGALL MVI   LANGNUM,0           NULL FOR WILDCARD                            
         NI    FILTRFLG,X'BF'      TURN OFF THE FILTER FLAG                     
         CLI   ACTNUM,ACTREP       REPORT MODE?                                 
         BE    XIT                 DONE WITH THE REPORT MODE                    
         B     VKLVEL                                                           
VKLANG2  L     R3,AFAALANG         ADDRESS OF LANGUAGE TABLE                    
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF THE TABLE                             
         LA    R3,6(R3)            FIRST ENTRY OF LANGUAGE TABLE                
         ZIC   R1,5(R2)            LENGTH OF LANGUAGE                           
         BCTR  R1,0                -1 FOR CLC                                   
*                                                                               
* CHECKING THROUGH THE LANGUAGE TABLE                                           
*                                                                               
VKLANGLP EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BNE   *+14                                                             
         MVC   8(3,R2),LANGSHR     COPY SHORT LANGUAGE NAME TO SCREEN           
         B     VKLANG3             GOOD LANGUAGE                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LANGSHRN    MATCH ON NATIVE LANGUAGE NAME?               
         BNE   *+14                                                             
         MVC   8(3,R2),LANGSHRN    COPY NATIVE LANGUAGE NAME TO SCREEN          
         B     VKLANG3                                                          
         BXLE  R3,R4,VKLANGLP      WHILE R6<=R5, R6:=R6+R4                      
         B     INVLFLD             INVALID LANGUAGE                             
VKLANG3  MVC   LANGNUM,LANGCODE    GOOD LANGUAGE CODE                           
         OI    6(R2),X'80'         TRANSMIT IN CASE NOT 3 LETTERS               
         XI    LANGNUM,X'FF'       FLIP THE BITS OF LANG CODE                   
*&&US*&& CLI   LANGNUM,X'FE'       IN U.S., LANG EUK NOT ALLOWED                
*&&UK*&& CLI   LANGNUM,X'FD'       IN U.K., LANG EUS NOT ALLOWED                
         BE    INVLFLD                                                          
         XI    LANGNUM,X'FF'       FLIP THE BITS OF LANG CODE AGAIN             
         OI    FILTRFLG,X'40'      TURN ON THE FILTER FLAG                      
         CLI   ACTNUM,ACTREP       REPORT MODE?                                 
         BE    XIT                 DONE WITH THE REPORT MODE                    
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE THE LEVEL                                                            
*                                                                               
VKLVEL   LA    R2,SFMLVELH         POINT TO THE LEVEL FIELD                     
         CLI   ACTNUM,ACTCOPY      CODE FOR COPY ACTION                         
         BNE   *+8                                                              
         LA    R2,SFCLVL1H                                                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,SFLLVELH                                                      
         CLI   5(R2),0             DO WE HAVE TO TEST THE LEVEL?                
         BNE   VKLVEL1             NO WE DON'T, NO LEVEL INPUTTED               
         MVI   LVELNUM,0                                                        
         NI    FILTRFLG,X'DF'      TURN OFF THE FILTER FLAG                     
         CLI   ACTNUM,ACTCOPY      COPY ACTION?                                 
         BE    XIT                 DONE WITH KEY VALIDATION                     
         B     VKRDTE                                                           
*                                                                               
VKLVEL1  CLI   8(R2),C'A'          VALID LEVELS BTWN A AND C                    
         BL    INVLFLD                                                          
         CLI   8(R2),C'C'                                                       
         BNH   VKLVEL2             BETWEEN A AND C                              
         CLI   8(R2),C'P'          PRODUCTION LEVEL?                            
         BNE   INVLFLD             NO, INVALID                                  
         CLI   ACTNUM,ACTLIST      LIST MODE?                                   
         BNE   INVLFLD             'P' ONLY VALID IN LIST MODE                  
         MVI   LVELNUM,0                                                        
         B     VKLVEL3             FILTER ON PRODUCTION LEVELS                  
VKLVEL2  MVC   LVELNUM,8(R2)                                                    
VKLVEL3  OI    FILTRFLG,X'20'      TURN ON THE FILTER FLAG                      
         CLI   ACTNUM,ACTCOPY      COPY ACTION?                                 
         BE    XIT                 DONE WITH KEY VALIDATION                     
         EJECT                                                                  
*                                                                               
* VALIDATE THE RELOAD DATE FOR THE LIST MODE                                    
*                                                                               
VKRDTE   CLI   ACTNUM,ACTLIST      LIST MODE                                    
         BNE   VKBKEY              NO, CONTINUE BUILDING THE KEY                
         CLI   SFLRDTEH+5,0        ALL RELOAD DATES?                            
         BNE   VKRDTE1             NO                                           
         NI    FILTRFLG,X'EF'      TURN OFF THE FILTER FLAG                     
         B     VKCDTE                                                           
*                                                                               
VKRDTE1  LA    R2,SFLRDTEH         PREPARE FIRST PARAM TO PERVAL                
         BAS   RE,VALIADTE                                                      
*&&DO                                                                           
         LA    R3,SFLRDTE          PREPARE FIRST PARAM TO PERVAL                
         ICM   R3,8,SFLRDTEH+5                                                  
         O     R3,=X'40000000'     MM/DD  NOT  MM/YY                            
         LA    R4,PERVALBK         OUTPUT BLOCK FOR PERVAL                      
*        ICM   R4,8,LANGNUM        ONLY IF WE WANT DATE IN LANG FORMAT          
         O     R4,=X'40000000'     SINGLE DATE ONLY                             
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
         TM    DMCB+4,X'03'        INVALID?                                     
         BZ    VKRDTE2             NO                                           
         LA    R2,SFLRDTEH                                                      
         B     INVLFLD             TELL USER                                    
*&&                                                                             
VKRDTE2  LA    R6,PERVALBK         CONVERT DATE TO JULIAN FOR STORAGE           
         USING PERVALD,R6                                                       
         GOTO1 DATCON,DMCB,(2,PVALCSTA),(19,RLDATE) SAVE FOR LATER              
         MVC   SFLRDTE(8),PVALCPER  RECOPY NEW VALID DATE                       
         OI    SFLRDTEH+6,X'80'                                                 
         OI    FILTRFLG,X'10'      TURN ON THE FILTER FLAG                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE THE CHANGE DATE FOR THE LIST MODE                                    
*                                                                               
VKCDTE   CLI   SFLCDTEH+5,0        ALL RELOAD DATES?                            
         BNE   VKCDTE1             NO                                           
         NI    FILTRFLG,X'FF'-X'08'   TURN OFF THE FILTER FLAG                  
         B     XIT                 DONE VALKEY IN LIST MODE                     
*                                                                               
VKCDTE1  LA    R3,SFLCDTE          PREPARE FIRST PARAM TO PERVAL                
         ICM   R3,8,SFLCDTEH+5                                                  
         O     R3,=X'40000000'     MM/DD  NOT  MM/YY                            
         LA    R4,PERVALBK         OUTPUT BLOCK FOR PERVAL                      
*        ICM   R4,8,LANGNUM        ONLY IF WE WANT DATE IN LANG FORMAT          
         O     R4,=X'40000000'     SINGLE DATE ONLY                             
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
*                                                                               
         TM    DMCB+4,X'03'        INVALID?                                     
         BZ    VKCDTE2             NO                                           
         LA    R2,SFLCDTEH                                                      
         B     INVLFLD             TELL USER                                    
*                                                                               
VKCDTE2  LA    R6,PERVALBK         CONVERT DATE TO JULIAN FOR STORAGE           
         USING PERVALD,R6                                                       
         MVC   CHDATE,PVALBSTA                                                  
         MVC   SFLCDTE(8),PVALCPER  RECOPY NEW VALID DATE                       
         OI    SFLCDTEH+6,X'80'                                                 
         OI    FILTRFLG,X'08'      TURN ON THE FILTER FLAG                      
         B     XIT                 DONE VALKEY IN LIST MODE                     
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* BUILD THE KEY FOR GENCON                                                      
*                                                                               
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING CTPHRECD,R4         OVERLAY KEY WITH OUR TEMPLATE                
         MVI   CTPHID,CTPHIDQ      LOAD UP THE IDENTIFIERS                      
         MVI   CTPHSUBI,CTPHSUBQ                                                
         CLI   SFMPHSE,C'T'        ON-LINE PHASE?                               
         BE    VKBKEY10                                                         
         MVC   CTPHNAME,SFMPHSE    NO, JUST A OFF-LINE PHASE NAME               
         OC    CTPHNAME,SPACES                                                  
         B     VKBKEY20                                                         
*                                                                               
VKBKEY10 MVC   DUB(L'SFMPHSE),SFMPHSE                                           
         MVI   DUB,C'0'                                                         
         GOTO1 HEXIN,DMCB,DUB,HEXPHASE,L'SFMPHSE                                
         MVC   CTPHHEXN,HEXPHASE   YES, MAKE HEX EQUIV. FOR THE KEY             
*                                                                               
VKBKEY20 MVC   CTPHLANG,LANGNUM    COPY THE LANG CODE                           
         MVC   CTPHLVL,LVELNUM              LEVEL                               
*                                                                               
         CLI   ACTNUM,ACTADD       ADD A RECORD?                                
         BNE   XIT                 NO, DONE                                     
*                                                                               
         CLI   SFMPHSE,C'T'        ON-LINE PHASE?                               
         BNE   VKPRDLVL            NO                                           
*                                                                               
         CLI   CTPHHEXN+2,0        CHECK IF INITIAL PROGRAM                     
         BE    VKPRDLVL            DON'T NEED TO CHECK IF IN FILE               
*                                                                               
         MVI   CTPHHEXN+2,0        CONTROLLER MUST BE IN BEFORE OVERLAY         
         MVI   CTPHLVL,0           DEFAULT LEVEL, PRODUCTION LEVEL              
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTPHPKEY),KEYSAVE                                          
         BNE   INVLADD             CANNOT ADD, CONTROLLER IS NOT IN YET         
         MVC   CTPHHEXN,HEXPHASE   RECOPY THE PHASE NAME AND LEVEL              
         MVC   CTPHLVL,SFMLVEL                                                  
*                                                                               
VKPRDLVL CLI   SFMLVELH+5,0        PRODUCTION LEVEL?                            
         BE    VKNODES             YES                                          
*                                                                               
         MVI   CTPHLVL,0           DEFAULT LEVEL, PRODUCTION LEVEL              
         GOTO1 HIGH                SEARCH IF PRODUCTION LEVEL EXISTS            
         CLC   KEY(L'CTPHPKEY),KEYSAVE    PRODUCTION LEVEL EXIST?               
         BNE   INVLTLVL            NO, CAN'T ADD THIS LEVEL                     
         MVC   CTPHLVL,SFMLVEL                                                  
*                                                                               
VKNODES  CLI   SFMSTNDH+5,0        ANY INPUT?                                   
         BNE   VKPORS              LEAVE NODES ALONE                            
         CLI   SFMENNDH+5,0        ANY INPUT?                                   
         BNE   VKPORS              LEAVE NODES ALONE                            
         OI    SFMSTNDH+6,X'80'    SHOW THE DEFAULT NODES                       
         OI    SFMENNDH+6,X'80'                                                 
*                                                                               
         CLC   =C'T00',SFMPHSE     IF PHASE STARTS WITH T00                     
         BE    VKBK10              THEN CORE-RESIDENT T00 ARE 00'S              
*                                                                               
         CLI   SFMPHSE,C'T'        IF OFF-LINE                                  
         BNE   VKBK10              THEN NODES ARE 00 (MOST LIKELY)              
*                                                                               
         CLI   CTPHHEXN+2,X'C0'    IS IT A SCREEN?                              
         BL    VKNODESP            NO                                           
VKBK10   MVI   SFMSTND,C'0'        DEFAULTS FOR SCREEN                          
         MVI   SFMENND,C'0'                                                     
         B     VKPORS                                                           
*                                                                               
VKNODESP MVI   SFMSTND,C'1'        DEFAULTS FOR PROGRAM                         
         MVI   SFMENND,C'0'                                                     
         CLI   CTPHHEXN+2,0        IS IT THE CONTROLLER?                        
         BNE   VKPORS                                                           
         MVI   SFMSTND,C'0'        DEFAULTS FOR CONTROLLER                      
         MVI   SFMENND,C'1'                                                     
*                                                                               
VKPORS   CLI   SFMPORSH+5,0        ANY INPUT?                                   
         BNE   VKCRRE              YES, LEAVE IT ALONE                          
*                                                                               
         CLI   SFMPHSE,C'T'        OFF-LINE?                                    
         BNE   VKPORS1             YES, PROGRAM                                 
*                                                                               
         CLC   =C'T00',SFMPHSE     IF PHASE STARTS WITH T00                     
         BE    VKPORS1             THEN IT'S A PROGRAM                          
*                                                                               
         CLI   CTPHHEXN+2,X'C0'    IS IT A SCREEN?                              
         BL    VKPORS1             NO                                           
         MVI   SFMPORS,C'S'        SCREEN                                       
         B     VKPORSX             TRANSMIT IT                                  
VKPORS1  MVI   SFMPORS,C'P'        PROGRAM                                      
VKPORSX  OI    SFMPORSH+6,X'80'    XMIT                                         
*                                                                               
VKCRRE   CLI   SFMCRREH+5,0        ANY INPUT?                                   
         BNE   VKDUMM              YES, LEAVE IT ALONE                          
         MVI   SFMCRRE,C'N'        DEFAULT IS NO AS PER MEL                     
         OI    SFMCRREH+6,X'80'    XMIT                                         
*                                                                               
VKDUMM   CLI   SFMDUMMH+5,0        ANY INPUT?                                   
         BNE   VKOFFL              YES, LEAVE IT ALONE                          
         MVI   SFMDUMM,C'N'        DEFAULT IS NO                                
         OI    SFMDUMMH+6,X'80'    XMIT                                         
*                                                                               
VKOFFL   CLI   SFMDUMMH+5,0        ANY INPUT?                                   
         BNE   XIT                 YES, AND NO DEFAULT RELOAD DATE              
         MVI   SFMOFFL,C'N'        DEFAULT IS NO                                
         CLI   SFMPHSE,C'T'                                                     
         BE    *+8                                                              
         MVI   SFMOFFL,C'Y'        UNLESS IT IS AN OFF-LINE PHASE               
         OI    SFMOFFLH+6,X'80'    XMIT                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHECKS TO MAKE SURE THE WORD  'DELETE'  IS IN THE RELOAD DATE IF              
* WE'RE DELETING A PRODUCTION PHASE.                                            
***********************************************************************         
DELREC   DS    0H                                                               
         CLI   SFMLVELH+5,0        PRODUCTION LEVEL?                            
         BNE   XIT                 NO, WE CAN DELETE AT WILL                    
*                                                                               
         CLC   =C'DELETE',SFMRDTE     YES, NEED WORD 'DELETE' FIRST             
         BE    XIT                 DELETE THIS PRODUCTION PHASE                 
*                                                                               
         CLI   ACTNUM,ACTSEL       DON'T HAVE WORD, CAN'T DELETE                
         BNE   CANTDELT                                                         
*                                                                               
         CLI   MODE,RECDEL         SECOND TIME HERE?                            
         BE    DELREC20            YES                                          
*                                                                               
         LA    R3,LISTDIR          PUT DELETE SELECT CODE BACK IN LIST          
         ZIC   R0,LISTNUM                                                       
DELREC10 CLI   0(R3),DELSELQ                                                    
         BE    DELREC15                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,DELREC10                                                      
         DC    H'0'                                                             
*                                                                               
DELREC15 ZIC   R1,LISTNUM          SAVE DISP INTO THE LIST OF SELECTION         
         SR    R1,R0                                                            
         STC   R1,DISPLIST                                                      
         B     CANTDELT                                                         
*                                                                               
DELREC20 LA    R3,LISTDIR          PUT DELETE SELECT CODE BACK IN LIST          
         ZIC   R0,DISPLIST                                                      
         LTR   R0,R0                                                            
         BZ    *+12                                                             
DELREC25 LA    R3,6(R3)                                                         
         BCT   R0,DELREC25                                                      
*                                                                               
         MVI   0(R3),DELSELQ                                                    
         B     CANTDELT                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                     *         
***********************************************************************         
DK       DS    0H                                                               
         LA    R2,KEY              POINT TO THE KEY TO DISPLAY                  
         USING CTPHRECD,R2                                                      
         MVI   PREVFLAG,1          PREVIOUS KEY EXAMINED                        
         MVC   PREVKEY,KEY         COPY THE KEY USED                            
         OI    SFMPHSEH+6,X'80'    DISPLAY THE PHASE NAME                       
*                                                                               
         CLI   CTPHNAME,0          IS IT AN OFF-LINE PHASE?                     
         BE    DKONLINE                                                         
         MVC   SFMPHSE,CTPHNAME    YES, JUST DISPLAY THE NAME                   
         B     DK10                                                             
*                                                                               
DKONLINE GOTO1 HEXOUT,DMCB,CTPHHEXN,SFMPHSE,L'CTPHHEXN                          
         MVI   SFMPHSE,C'T'                                                     
*                                                                               
DK10     OI    SFMLVELH+6,X'80'    DISPLAY THE LEVEL OF THE PHASE               
         MVI   SFMLVEL,C' '                                                     
         CLI   CTPHLVL,0           ANY LEVEL?                                   
         BE    DKLANG              NONE, DISPLAY THE LANGUAGE                   
         MVC   SFMLVEL,CTPHLVL                                                  
*                                                                               
DKLANG   L     R3,AFAALANG         ADDRESS OF LANGUAGE TABLE                    
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF TABLE                                 
         LA    R3,6(R3)            FIRST ENTRY OF LANGUAGE TABLE                
DKLANGLP CLC   CTPHLANG,LANGCODE   SAME LANGUAGE?                               
         BE    *+10                YES                                          
         BXLE  R3,R4,DKLANGLP      NO, TRY THE NEXT ENTRY                       
         DC    H'0'                BAD CODE                                     
         MVC   SFMLANG,SPACES                                                   
         MVC   SFMLANG(L'LANGSHR),LANGSHR                                       
         OI    SFMLANGH+6,X'80'                                                 
         DROP  R2,R3                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                  *         
***********************************************************************         
DR       XC    DATADISP,DATADISP   ZERO OUT THE DISPLACEMENT                    
         MVI   DATADISP+1,CTPHOVEQ   LOAD UP PROPER DISPLACEMENT                
*                                                                               
* DISPLAY THE SYSTEM ELEMENT                                                    
*                                                                               
DRSYSD   L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHSCEQ     SYSTEM ELEMENT CODE X'05'                    
         BAS   RE,GETEL            IS IT THE SYSTEM ELEMENT?                    
         BE    *+6                 YES                                          
         DC    H'0'                IMPOSSIBLE, MUST HAVE A HEX EQUIV.           
         USING CTPHSYSD,R6         SYSTEM ELEMENT TEMPLATE                      
         GOTO1 HEXOUT,DMCB,CTPHSNDE,WORK,1                                      
         MVC   SFMSTND,WORK                                                     
         OI    SFMSTNDH+6,X'80'    DISPLAY THE START NODE                       
         MVC   SFMENND,WORK+1                                                   
         OI    SFMENNDH+6,X'80'    DISPLAY THE END NODE                         
*                                                                               
DRFLGCR  OI    SFMCRREH+6,X'80'    DISPLAY CORE-RESIDENT(Y/N)                   
         TM    CTPHSFL1,CTPHSCRQ   IS THE CORE-RESIDENT BIT ON                  
         BZ    DRFLGCR1            NO, IT IS ZERO                               
         MVI   SFMCRRE,C'Y'        YES, IT IS                                   
         B     DRFLGDU             GO FOR NEXT FLAG                             
DRFLGCR1 MVI   SFMCRRE,C'N'                                                     
*                                                                               
DRFLGDU  OI    SFMDUMMH+6,X'80'    DISPLAY CORE=DUMMY(Y/N)                      
         TM    CTPHSFL1,CTPHSDMQ   IS CORE=DUMMY BIT ON?                        
         BZ    DRFLGDU1            NO, IT IS ZERO                               
         MVI   SFMDUMM,C'Y'        YES, IT IS                                   
         B     DRFLGOF             GO FOR NEXT FLAG                             
DRFLGDU1 MVI   SFMDUMM,C'N'                                                     
*                                                                               
DRFLGOF  OI    SFMOFFLH+6,X'80'    DISPLAY OFF-LINE ONLY(Y/N)                   
         TM    CTPHSFL1,CTPHSOFQ   IS OFFLINE BIT ON?                           
         BZ    DRFLGOF1            NO, IT IS ZERO                               
         MVI   SFMOFFL,C'Y'        YES, IT IS                                   
         B     DRFLGPO             GO FOR NEXT FLAG                             
DRFLGOF1 MVI   SFMOFFL,C'N'                                                     
*                                                                               
DRFLGPO  OI    SFMPORSH+6,X'80'    DISPLAY PROGRAM/SCREEN(P/S)                  
         TM    CTPHSFL1,CTPHSSCQ   IS SCREEN BIT ON?                            
         BZ    DRFLGPO1            NO, IT IS ZERO                               
         MVI   SFMPORS,C'S'        YES, IT IS                                   
         TM    CTPHSFL1,CTPHSCRQ+CTPHSCSQ TEST SPECIAL CORERES SCREEN           
         BNO   DRSPRE                                                           
         MVI   SFMPORS,C'R'        SHOW SPECIAL CORE RESIDENT SCREEN            
         B     DRSPRE              CONTINUE WITH NEXT SECTION                   
DRFLGPO1 MVI   SFMPORS,C'P'                                                     
*                                                                               
DRSPRE   EDIT  (4,CTPHSSPR),(10,SFMSPRE),0,ALIGN=LEFT                           
         OI    SFMSPREH+6,X'80'    DISPLAY THE SPARE BYTES                      
*                                                                               
DRRDTE   OC    CTPHSRDT,CTPHSRDT   NO RELOAD DATE?                              
         BNZ   DRRDTE0             NONE                                         
         XC    SFMRDTE,SFMRDTE     CLEAR THE FIELD                              
         B     DRRDTE1             TRANSMIT IT                                  
DRRDTE0  GOTO1 DATCON,DMCB,(8,CTPHSRDT),(11,SFMRDTE)                            
DRRDTE1  OI    SFMRDTEH+6,X'80'    DISPLAY THE RELOAD DATE                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY THE RELOAD LIST ELEMENT                                               
*                                                                               
DRLSTD   L     R6,AIO              POINT TO THE RECORD                          
         MVC   SFMRLS1,SPACES      BLANK OUT RELOAD LIST LINE 1                 
         OI    SFMRLS1H+6,X'80'                                                 
         MVC   SFMRLS2,SPACES      BLANK OUT RELOAD LIST LINE 2                 
         OI    SFMRLS2H+6,X'80'                                                 
*                                                                               
         MVI   ELCODE,CTPHLCEQ     RELOAD LIST ELEMENT CODE X'15'               
         BAS   RE,GETEL            IS IT THE RELOAD LIST ELEMENT?               
         BNE   DRDSCD              NO, GO ON FOR THE DESCRIPTION                
         USING CTPHLSTD,R6         RELOAD LIST ELEMENT TEMPLATE                 
         CLI   CTPHLFLG,CTPHLALQ   ALL ITEMS?                                   
         BNE   DRLSTFEW            NO, ONLY A FEW                               
         MVC   SFMRLS1(3),=C'ALL'  SHOW THAT IT IS ALL ITEMS                    
         B     DRDSCD              DONE WITH THIS SECTION                       
DRLSTFEW ZIC   R5,CTPHLLEN         FIND NUMBER OF ITEMS TO LOAD                 
         LA    R4,CTPHLOVQ                                                      
         SR    R5,R4               # OF ITEMS TO LOAD                           
         BNP   DRDSCD              IGNORE BAD ELEMENTS                          
         LA    R3,CTPHLLST         POINT TO ACTUAL LIST                         
         LA    R4,SFMRLS1          POINT WHERE TO OUTPUT                        
         LA    R2,SFMRLS1X                                                      
DRLSTLP  GOTO1 HEXOUT,DMCB,(R3),(R4),1 HEX INTO EBCDIC                          
         LA    R3,1(R3)            MOVE OVER TO NEXT ITEM                       
         LA    R4,2(R4)            MOVE POINTER SO WE CAN PUT A COMMA           
         CR    R4,R2               END-OF-LINE?                                 
         BNE   DRLSTLP1            GET NEXT ITEM                                
         LA    R1,SFMRLS2X         R1 DOESN'T GET AFFECTED BY GOTO1             
         CR    R2,R1               DID WE GO TO THE SECOND LINE?                
         BE    DRDSCD              DID IT ALREADY                               
         LR    R2,R1               GO TO SECOND LINE                            
         LA    R4,SFMRLS2                                                       
DRLSTLP1 BCTR  R5,0                ONE LESS TO GO                               
         LTR   R5,R5               OUT OF ITEMS?                                
         BZ    DRDSCD                                                           
         LA    R1,SFMRLS2                                                       
         CR    R4,R1                                                            
         BE    DRLSTLP             DON'T PUT COMMA, STARTING NEW LINE           
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)            READY FOR NEXT ITEM                          
         B     DRLSTLP                                                          
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY THE DESCRIPTION ELEMENT                                               
*                                                                               
DRDSCD   L     R6,AIO              POINT TO THE RECORD                          
         MVC   SFMDESC,SPACES      BLANK OUT THE DESCRIPTION                    
         OI    SFMDESCH+6,X'80'                                                 
         MVI   ELCODE,CTPHDCEQ     DESCRIPTION ELEMENT CODE X'25'               
         BAS   RE,GETEL            IS IT THE DESCRIPTION ELEMENT?               
         BNE   DRVRCD              NO, SHOULD BE THOUGH                         
         USING CTPHDSCD,R6         DESCRIPTION ELEMENT TEMPLATE                 
         ZIC   R3,CTPHDLEN         LOAD UP THE LENGTH                           
         SH    R3,=H'3'            OVERHEAD OF 2 AND 1 FOR MVC                  
         EX    R3,*+8              MODIFY LENGTH OF MVC, MVC, THEN B            
         B     DRVRCD              DONE WITH THIS SECTION                       
         MVC   SFMDESC(0),CTPHDDSC   COPY DESCRIPTION THEN TRANSMIT             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY THE VERSION CONTROL ELEMENT                                           
*                                                                               
DRVRCD   MVC   SFMVRC1,SPACES      BLANK OUT THE VERSION CONTROL LINE           
         MVC   SFMNDTE,SPACES          AND THE NOTIFY DATE                      
         OI    SFMVRC1H+6,X'80'                                                 
         OI    SFMNDTEH+6,X'80'                                                 
*                                                                               
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHVCEQ     DESCRIPTION ELEMENT CODE C'V'                
         BAS   RE,GETEL            IS IT THE DESCRIPTION ELEMENT?               
         BNE   DRCOMD              NO, SHOULD BE THOUGH                         
         USING CTPHVRSD,R6         VERSION CONTROL TEMPLATE                     
         ZIC   R4,CTPHVLEN         LOAD UP THE LENGTH                           
         SH    R4,=Y(CTPHVOVQ)                                                  
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,SFMVRC1                                                       
         LA    R6,CTPHVFAC         FIRST ENTRY IN VERSION CONTROL ELEM          
         USING CTPHVFAC,R6                                                      
*                                                                               
DRV00    CLI   CTPHVFAC,0          IS FACPACK SYSTEM SET?                       
         BE    DRV20                                                            
*                                                                               
         LA    R5,FACIDTAB         YES                                          
*                                                                               
DRV10    CLC   CTPHVFAC,4(R5)      FIND WHICH SYSTEM                            
         BE    DRV15                                                            
         LA    R5,L'FACIDTAB(R5)                                                
         CLI   0(R5),X'FF'                                                      
         BNE   DRV10                                                            
         DC    H'0'                                                             
*                                                                               
DRV15    MVC   0(4,R3),0(R5)                                                    
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         B     DRV40               PUT OUT THE VERSION LETTER WITH '='          
*                                                                               
DRV20    CLI   CTPHVSEN,0          IS IT AN APPLICATION SYSTEM NUMBER?          
         BE    DRV30                                                            
*                                                                               
         L     R5,ASELIST          YES, PUT OUT THE APPLICATION NAME            
         USING SELISTD,R5                                                       
         LH    R0,0(R5)                                                         
         L     R1,2(R5)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
DRV20LP  CLC   SESYS,CTPHVSEN                                                   
         BE    DRV25                                                            
         BXLE  R5,R0,DRV20LP                                                    
         DC    H'0'                DIE IF NO SUCH APPLICATION SYSTEM #          
*                                                                               
DRV25    MVC   0(L'SENAME,R3),SENAME                                            
         LA    R3,4(R3)                                                         
DRV25LP  CLI   0(R3),C' '          REDUCE SPACING                               
         BE    DRV40                                                            
         LA    R3,1(R3)                                                         
         B     DRV25LP                                                          
         DROP  R5                                                               
*                                                                               
DRV30    OC    CTPHVAGY,CTPHVAGY                                                
         BNZ   *+6                                                              
         DC    H'0'                DIE, NO OTHER TYPE POSSIBLE                  
         MVC   0(L'CTPHVAGY,R3),CTPHVAGY                                        
         LA    R3,2(R3)                                                         
*                                                                               
DRV40    MVI   0(R3),C'='                                                       
         MVC   1(1,R3),CTPHVVRS                                                 
         MVI   2(R3),C','                                                       
         LA    R3,3(R3)                                                         
         LA    R6,CTPHVNXT                                                      
         SH    R4,=Y(CTPHVNXT-CTPHVFAC)                                         
         BP    DRV00                                                            
*                                                                               
         BCTR  R3,R0               REMOVE LAST COMMA                            
         MVI   0(R3),C' '                                                       
*                                                                               
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHVCEQ     DESCRIPTION ELEMENT CODE C'V'                
         BAS   RE,GETEL            IS IT THE DESCRIPTION ELEMENT?               
         BE    *+6                 NO, SHOULD BE THOUGH                         
         DC    H'0'                                                             
         USING CTPHVRSD,R6         VERSION CONTROL TEMPLATE                     
         GOTO1 DATCON,DMCB,(8,CTPHVDTE),(11,SFMNDTE)                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY THE COMMENT ELEMENT                                                   
*                                                                               
DRCOMD   L     R6,AIO              POINT TO THE RECORD                          
         MVC   SFMCMT1,SPACES      BLANK OUT COMMENT LINE 1                     
         OI    SFMCMT1H+6,X'80'                                                 
         MVC   SFMCMT2,SPACES      BLANK OUT COMMENT LINE 2                     
         OI    SFMCMT2H+6,X'80'                                                 
         MVC   SFMCMT3,SPACES      BLANK OUT COMMENT LINE 3                     
         OI    SFMCMT3H+6,X'80'                                                 
         MVI   ELCODE,CTPHCCEQ     COMMENT ELEMENT CODE X'35'                   
         BAS   RE,GETEL            IS IT THE COMMENT ELEMENT?                   
DRCOM1   BNE   DRX                 NO                                           
         USING CTPHCOMD,R6         COMMENT ELEMENT TEMPLATE                     
         ZIC   R3,CTPHCLEN         CALCULATE COMMENT LENGTH                     
         SH    R3,=H'4'            OVERHEAD OF 3, 1 FOR MVC                     
         CLI   CTPHCLNM,1          FIRST LINE?                                  
         BNE   DRCOM1A             NO, SEE IF SECOND LINE                       
         EX    R3,*+8              ADJUST MVC, MVC, THEN B                      
         B     DRCOMNXT                                                         
         MVC   SFMCMT1(0),CTPHCTXT  COPY THE COMMENT                            
DRCOM1A  CLI   CTPHCLNM,2          SECOND LINE                                  
         BNE   DRCOM1B             NO, HAS TO BE THE THIRD                      
         EX    R3,*+8              ADJUST MVC, MVC, THEN B                      
         B     DRCOMNXT                                                         
         MVC   SFMCMT2(0),CTPHCTXT  COPY THE COMMENT                            
DRCOM1B  EX    R3,*+8              ADJUST MVC, MVC, THEN B                      
         B     DRCOMNXT                                                         
         MVC   SFMCMT3(0),CTPHCTXT  COPY THE COMMENT                            
DRCOMNXT BAS   RE,NEXTEL           IS THERE ANOTHER COMMENT ELEMENT?            
         B     DRCOM1              CHECK                                        
*                                                                               
DRX      CLC   THISLSEL,LP@DELS                                                 
         BNE   XIT                                                              
         B     DELREC                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                       *         
***********************************************************************         
VR       CLI   ACTNUM,ACTCOPY      COPY ACTION CODE?                            
         BNE   VR10                NO                                           
         GOTO1 =A(COPYFROM),DMCB,(RC),RR=RELO                                   
         B     XIT                                                              
*                                                                               
VR10     CLI   SFMDESCH+5,0        ANY INPUT IN THE DESCRIPTION?                
         BE    INVLDESC            NEED A DESCRIPTION                           
*                                                                               
VRSTND   TM    SFMSTNDH+4,X'02'    VALID HEX?                                   
         BZ    INVLSTND            INVALID START NODE                           
*                                                                               
VRENND   TM    SFMENNDH+4,X'02'    VALID HEX?                                   
         BZ    INVLENND            INVALID END NODE                             
*                                                                               
VRFLGPO  CLI   SFMPORS,C'P'        IS IT A PROGRAM?                             
         BE    VRFLGCR             THEN VALIDATE NEXT FLAG                      
         CLI   SFMPORS,C'S'        IS IT A SCREEN?                              
         BNE   VRFLGPO1            THEN VALIDATE NEXT FLAG                      
         CLI   SFMCRRE,C'Y'        SCREENS CANT BE CORE RESIDENT                
         BE    INVLPORS                                                         
         B     VRFLGCR                                                          
VRFLGPO1 CLI   SFMPORS,C'R'        IS IT CORE RESIDENT SCREEN?                  
         BNE   INVLPORS            INVALID ENTRY                                
         CLI   SFMCRRE,C'Y'        MUST ALSO SPECIFY CARE RESIDENT              
         BNE   INVLPORS                                                         
*                                                                               
VRFLGCR  CLI   SFMCRRE,C'Y'        CORE RESIDENT?                               
         BE    VRFLGDU                                                          
         CLI   SFMCRRE,C'N'        NOT CORE RESIDENT?                           
         BNE   INVLCRRE            INVALID ENTRY                                
*                                                                               
VRFLGDU  CLI   SFMDUMM,C'N'        CORE<>DUMMY?                                 
         BE    VRSPRE                                                           
         CLI   SFMDUMM,C'Y'        CORE=DUMMY?                                  
         BNE   INVLDUMM            INVALID ENTRY                                
         CLI   SFMCRRE,C'Y'        BOTH CORE-RES AND CORE=DUMMY?                
         BE    INVLCRDM            YES, CAN'T BE                                
*                                                                               
VRSPRE   CLI   SFMSPREH+5,0        NOTHING IN HERE?                             
         BE    VRFLGOF             NOTHING, CHECK FOR OFF-LINE ONLY             
VRSPRE1  TM    SFMSPREH+4,X'08'    VALID NUMERIC?                               
         BZ    INVLSPRE            INVALID SPARE BYTES                          
         ZIC   R4,SFMSPREH+5       LENGTH TO USE WITH PACK                      
         BCTR  R4,0                                                             
         EX    R4,*+8              CHANGE LENGTH IN PACK, PACK, BRANCH          
         B     *+10                                                             
         PACK  DUB,SFMSPRE(0)      PACK THE EBCDIC NUMBER                       
         CP    DUB(8),=P'2147483647'                                            
         BH    INVLOVFL            INTEGER EXCEEDED FULL WORD BOUND             
*                                                                               
VRFLGOF  CLI   SFMOFFL,C'N'        NOT OFF-LINE ONLY?                           
         BNE   *+16                                                             
         CLI   SFMPHSE,C'T'        BETTER BE AN ON-LINE PHASE                   
         BNE   INVLOFFL                                                         
         B     VRRDTE                                                           
*                                                                               
         CLI   SFMOFFL,C'Y'        OFF-LINE ONLY?                               
         BNE   INVLOFFL            INVALID ENTRY                                
         CLI   SFMCRRE,C'Y'        BOTH CORE-RES AND OFF-LINE?                  
         BE    INVLCROF            YES, CAN'T BE                                
         CLI   SFMDUMM,C'Y'        BOTH CORE=DUMMY AND OFF-LINE?                
         BE    INVLCROF            YES, CAN'T BE                                
*                                                                               
VRRDTE   LA    R2,SFMRDTEH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VRRDTE30            YES                                          
*                                                                               
         CLI   ACTNUM,ACTADD       ADD A NEW PHASE?                             
         BNE   VRRDTEX             NO, LEAVE IT ALONE                           
         CLI   SFMOFFL,C'Y'        OFFLINE ONLY?                                
         BE    VRRDTEX             YES, NEVER MIND                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)      GET TODAY'S DATE                
         GOTO1 ADDAY,DMCB,TODAY,DAYDATE,F'1'    GET TOMORROW'S DATE             
         GOTO1 DATCON,DMCB,(0,DAYDATE),(2,DATEBIN)                              
         MVC   RLODDATE,DATEBIN     COPY COMPRESSED DATE FOR LATER USE          
         GOTO1 DATCON,DMCB,(0,DAYDATE),(11,SFMRDTE) DISP RELOAD DATE            
         MVI   5(R2),8             LENGTH OF RELOAD DATE                        
         B     VRRDTE50                                                         
*                                                                               
VRRDTE30 CLI   SFMOFFL,C'Y'        IF OFFLINE ONLY                              
         BE    INVLFLD             THEN DON'T ALLOW INPUT                       
*                                                                               
         BAS   RE,VALIADTE         VALIDATE A DATE                              
*                                                                               
         LA    R6,PERVALBK                                                      
         USING PERVALD,R6                                                       
         GOTO1 DATCON,DMCB,(5,0),(2,DATEBIN)    GET TODAY'S DATE                
*                                                                               
         CLI   SFMANYD,C'Y'        ACCEPT ANY DATE?                             
         BE    VRRDTE40            YES                                          
*                                                                               
         CLC   PVALCSTA,DATEBIN    COMPARE INPUTTED DATE WITH TODAY             
         BL    INVLDATE            TELL USER MUST BE GREATER                    
         BH    VRRDTE40            GREATER THAN TODAY                           
         CLI   FACPCKID,1          TEST SYSTEM AND EQUAL TO TODAY?              
         BE    VRRDTE40            YES                                          
         CLI   FACPCKID,6          MEL SYSTEM AND EQUAL TO TODAY?               
         BNE   INVLDATE            NO                                           
VRRDTE40 MVC   RLODDATE,PVALCSTA   COPY COMPRESSED FOR LATER USE                
         MVC   8(8,R2),PVALCPER    RECOPY NEW VALID DATE                        
         DROP  R6                                                               
VRRDTE50 MVI   SFMANYD,C'N'        RESET 'ACCEPT ANY DATE' REQUEST              
         OI    SFMANYDH+6,X'80'                                                 
         OI    6(R2),X'80'         TRANSMIT NEW FORMAT                          
VRRDTEX  OI    4(R2),X'20'         RELOAD DATE HAS BEEN VALIDATED               
         EJECT                                                                  
VRRLST   DS    0H                                                               
         CLI   SFMRLS1H+5,0        EMPTY LIST?                                  
         BNE   VRRLST0                                                          
         CLI   SFMRLS2H+5,0                                                     
         BNE   VRRLST0                                                          
         CLC   =C'00',SFMPHSE+4    YES, IS IT THE ROOT?                         
         BNE   VRRLSTX             NO, ONLY ROOT CAN HAVE A LIST                
*                                                                               
         CLI   ACTNUM,ACTADD       ADDING THE ROOT?                             
         BE    VRRLSTX             YES, RELOAD LIST NOT REQUIRED                
         CLI   SFMRDTEH+5,0        NO, DO WE HAVE A RELOAD DATE?                
         BE    VRRLSTX                 NO, RELOAD LIST NOT REQUIRED             
         LA    R2,SFMRLS1H                                                      
         B     MISSFLD             YES, WE NEED A RELOAD LIST                   
*                                                                               
VRRLST0  CLC   =C'00',SFMPHSE+4    IS IT THE ROOT?                              
         BNE   INVLLIST            ONLY ROOT CAN HAVE A LIST                    
         CLI   SFMRDTEH+5,0        ANY RELOAD DATE?                             
         BE    MISSRDTE            NO, LIST NEEDS A RELOAD DATE                 
         CLI   ACTNUM,ACTADD       ADDING THE ROOT?                             
         BNE   VRRLST1             NO                                           
         LA    R2,SFMRLS1H         YES, ERROR IF A LIST WHEN ADDING             
         B     INVLFLD                                                          
*                                                                               
VRRLST1  TM    SFMRLS1H+4,X'20'    FIRST LINE IN LIST VALIDATED BEFORE?         
         BZ    VRRLSTA             NO                                           
         TM    SFMRLS2H+4,X'20'    SECOND LINE IN LIST VALIDATED ALSO?          
         BNZ   VR100               YES, BUILD THE RECORD                        
VRRLSTA  CLI   SFMRLS1H+5,0        DID WE GET ANYTHING IN 1ST LINE?             
         BNE   VRRLSTB             NOTHING IN 1ST LINE                          
*        CLI   SFMRLS2H+5,0        DID WE GET ANYTHING IN 2ND LINE?             
*        BE    VRRLSTX             VALIDATE THE LINE                            
         MVC   SFMRLS1H+5(1),SFMRLS2H+5  COPY LENGTH IN 2ND LINE                
         MVC   SFMRLS1,SFMRLS2     LINE 2 BECOMES LINE1                         
         MVC   SFMRLS2,SPACES      BLANK OUT LINE 2                             
         MVI   SFMRLS2H+5,0        NEW LENGTH IN LINE 2                         
         OI    SFMRLS1H+6,X'80'    DISPLAY BOTH LINES                           
         OI    SFMRLS2H+6,X'80'                                                 
VRRLSTB  CLC   =C'ALL',SFMRLS1     RELOAD ALL OF THEM?                          
         BE    VRRLSTX             YES, DONE VALIDATING RELOAD LIST             
         LA    R6,SFMRLS1H         POINT TO THE HEADER OF THE 1ST LINE          
         XR    R0,R0               ZERO COUNT                                   
         XC    THETABLE,THETABLE   CLEAR THE SORT TABLE                         
         LA    R2,SORTTABL         SORT TABLE                                   
VRRLSET  CLI   5(R6),0             ANYTHING IN CURRENT LINE?                    
         BE    VRRL2LP0            ALSO CHECKS IF SECOND LINE IS BLANK          
         GOTO1 SCANNER,DMCB,(R6),(12,BLOCK)                                     
         LA    R3,BLOCK            POINT TO BLOCK FILLED BY SCANNER             
         ZIC   R4,DMCB+4           LOAD UP NUMBER OF FIELDS                     
         LR    R5,R0                                                            
         LA    R5,0(R4,R5)         NEW COUNT OF RECORDS                         
         LR    R0,R5                                                            
VRRL1LP  CLI   1(R3),0             IS THERE A SECOND HALF?                      
         BNE   INVLRLST            THERE IS A SECOND HALF, SHOULDN'T BE         
         CLI   0(R3),2             IS IT A 2 DIGIT HEX?                         
         BNE   INVLRLST            BOUNDED IN X'00' TO X'FF'                    
         TM    2(R3),X'20'         IS IT A VALID HEX?                           
         BZ    INVLRLST                                                         
         LA    R5,12(R3)           LOAD ADDRESS OF WHERE HEX EBCDIC IS          
         GOTO1 HEXIN,DMCB,(R5),(R2),2   CONVERT EBCDIC AND PUT IN TABLE         
*        CLI   0(R2),0             RELOAD CONTROLLER?                           
*        BE    INVLRELD            YES, CONTROLLER CAN'T RELOAD ITSELF          
         LA    R2,L'SORTTABL(R2)   NEXT ENTRY IN TABLE                          
         LA    R3,L'BLOCKSZ(R3)    GO TO NEXT LINE IN BLOCK                     
         BCT   R4,VRRL1LP          ARE WE DONE WITH THE LINE YET?               
         LA    R4,SFMRLS2H                                                      
         CR    R6,R4               DID WE DO THE SECOND LINE YET?               
         BE    VRRL2LP0                                                         
         LR    R6,R4               NO, THEN DO THE SECOND LINE                  
         B     VRRLSET                                                          
VRRL2LP0 LA    R2,SORTTABL         POINT TO OUR SORTED TABLE                    
         LR    R5,R0               LOAD THE COUNT                               
         CH    R5,=H'2'            ONLY ONE ITEM?                               
         BNL   VRRLNOT1            NOT ONE ITEM                                 
         MVI   PREVHEX,1           ONE ITEM ONLY                                
         B     VRRLSTDN            DON'T NEED TO SORT OR ARRANGE                
VRRLNOT1 XC    DMCB,DMCB           CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A12' GET ADDR OF XSORT VIA CALLOV              
*&&US*&& MVC   DMCB+4(4),=X'D9000A50' GET ADDR OF QSORT VIA CALLOV              
         GOTO1 CALLOV,DMCB         RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         BNE   *+6                 NO, IT'S IN 1ST PARAMETER                    
         DC    H'0'                YES, DIE HORRIBLY                            
         MVC   VQSORT,DMCB         SAVE THAT ADDRESS FOR LATER                  
         GOTO1 VQSORT,DMCB,(R2),(R5),L'SORTTABL,L'SORTTABL,0                    
         LR    R4,R5               SAVE COUNTER BEFORE ANY MODIFICATION         
         MVC   PREVHEX,0(R2)       GET FIRST ITEM                               
         LA    R2,L'SORTTABL(R2)   GET NEXT ITEM TO COMPARE WITH                
         BCTR  R4,0                COMPARE 1ST AND 2ND ITEMS                    
VRRL2LP  CLC   PREVHEX,0(R2)       DID WE GET THE SAME ITEM TWICE?              
         BE    INVLITEM                                                         
         MVC   PREVHEX,0(R2)                                                    
         LA    R2,L'SORTTABL(R2)   GET NEXT ITEM TO COMPARE WITH                
         BCT   R4,VRRL2LP          LOOP UNTIL NO MORE ITEMS                     
* NOW PRINT OUT THE SORTED LIST                                                 
         MVC   SFMRLS1,SPACES      BLANK OUT RELOAD LIST LINE 1                 
         OI    SFMRLS1H+6,X'80'                                                 
         MVC   SFMRLS2,SPACES      BLANK OUT RELOAD LIST LINE 2                 
         OI    SFMRLS2H+6,X'80'                                                 
         STC   R5,PREVHEX          STORE COUNT TILL WE BUILD RECORD             
         LA    R3,SORTTABL         SOURCE OF SORTED LIST                        
         LA    R4,SFMRLS1          WHERE TO START DISPLAYING                    
         LA    R2,SFMRLS1X         BOUNDS                                       
VRDRRLS  GOTO1 HEXOUT,DMCB,(R3),(R4),1 HEX INTO EBCDIC                          
         LA    R3,1(R3)            MOVE OVER TO NEXT ITEM                       
         LA    R4,2(R4)            MOVE POINTER SO WE CAN PUT A COMMA           
         CR    R4,R2               END-OF-LINE?                                 
         BNE   VRDRRLS1            GET NEXT ITEM                                
         LA    R1,SFMRLS2X                                                      
         CR    R2,R1               DID WE GO TO THE SECOND LINE?                
         BE    VRRLSTDN            DID IT ALREADY                               
         LR    R2,R1               GO TO SECOND LINE                            
         LA    R4,SFMRLS2                                                       
VRDRRLS1 BCTR  R5,0                ONE LESS TO GO                               
         LTR   R5,R5               OUT OF ITEMS?                                
         BZ    VRRLSTDN                                                         
         LA    R1,SFMRLS2                                                       
         CR    R4,R1                                                            
         BE    VRDRRLS             DON'T PUT COMMA, STARTING NEW LINE           
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)            READY FOR NEXT ITEM                          
         B     VRDRRLS                                                          
*                                                                               
VRRLSTDN ZIC   R5,PREVHEX          LOAD UP THE COUNT                            
         LTR   R5,R5               NOTHING TO CHECK?                            
         BZ    VRRLSTX                                                          
         LA    R4,KEY                                                           
         USING CTPHRECD,R4                                                      
         LA    R3,SORTTABL                                                      
         MVC   AIO,AIO2            DON'T RUIN WHAT WAS THERE                    
VRRLSTLP MVC   CTPHHEXN+2(1),0(R3) CHECK IF PHASE EXISTS                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTPHPKEY),KEYSAVE                                          
         BNE   INVLPHSE            PHASE DOESN'T EXIST                          
         LA    R3,1(R3)            CHECK NEXT ITEM                              
         BCT   R5,VRRLSTLP                                                      
         MVI   CTPHHEXN+2,0        ONLY THE BASE CAN HAVE A LIST                
         MVC   AIO,AIO1            POINT IT BACK TO WHAT IT WAS                 
         DROP  R4                                                               
VRRLSTX  OI    SFMRLS1H+4,X'20'    VALIDATE BOTH RELOAD LIST LINES              
         OI    SFMRLS2H+4,X'20'                                                 
         EJECT                                                                  
* VALIDATE THE VERSION CONTROL LIST                                             
*                                                                               
VR100    XC    VERSELEM,VERSELEM                                                
****     B     VR106               ALLOW VERSION CONTROL ON ANY PHASE           
*                                                                               
         CLI   SFMPHSE,C'T'        OFF-LINE PHASE?                              
         BE    VR103                                                            
*&&UK                                                                           
         CLC   =C'ME',SFMPHSE      ME SYSTEM?                                   
         BNE   *+16                                                             
         CLI   SFMPHSE+4,C'1'      YES, IT DOESN'T HAVE THE 0                   
         BNE   BLDREC              NO VERSION CONTROL ELEMENT IF NOT 1          
         B     VR106                                                            
*&&                                                                             
         CLC   =C'01',SFMPHSE+4    YES, 01 PHASE?                               
         BNE   BLDREC              NO, VERSION CONTROL ELEMENT                  
         B     VR106                                                            
*                                                                               
VR103    CLC   =C'T00A',SFMPHSE    CORE-RES PHASE?                              
         BE    *+14                                                             
         CLC   =C'T00B',SFMPHSE                                                 
         BNE   VR104                                                            
         CLI   SFMLVELH+5,0        AND FOR A SPECIFIC LEVEL?                    
         BE    BLDREC                                                           
         B     VR108               THEN WE CAN ACCEPT VERSION CONTROL           
*                                                                               
VR104    CLC   =C'00',SFMPHSE+4    USING PRODUCTION LEVEL CONTROLLER?           
         BNE   BLDREC                                                           
*                                                                               
VR106    CLI   SFMLVELH+5,0                                                     
         BNE   BLDREC              NO, THEN JUST BUILD THE RECORD               
*                                                                               
VR108    CLI   SFMVRC1H+5,0        ANYTHING FOR THE VERSION CONTROL?            
         BE    BLDREC              NOTHING, BUILD THE RECORD                    
*                                                                               
         MVI   VERSELEM,CTPHVCEQ                                                
         MVI   VERSELEM+1,CTPHVOVQ  TEMPORARY LENGTH OF ELEMENT                 
*                                                                               
         GOTO1 SCANNER,DMCB,SFMVRC1H,(10,BLOCK)                                 
         CLI   DMCB+4,0                                                         
         BE    INVLVRCL                                                         
*                                                                               
         LA    R2,VERSNTRY         R2 = A(1ST ENTRY IN VERSION ELEM)            
         USING CTPHVFAC,R2                                                      
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4           R4 = NUMBER OF LINES                         
*                                                                               
VR100LP  CLI   0(R3),0             NO APPL-ID, SYSTEM, OR POWER CODE?           
         BE    INVLVRCL            NONE, ERROR                                  
*                                                                               
         CLI   1(R3),1             ONE BYTE VERSION LETTER?                     
         BNE   INVLVRCL            NO, ERROR                                    
*                                                                               
         CLI   22(R3),C'A'         VERSION (LEVEL) MUST BE A,B, OR C            
         BL    INVLVRCL                                                         
         CLI   22(R3),C'C'                                                      
         BH    INVLVRCL            OTHERWISE ERROR                              
*                                                                               
         LA    R5,FACIDTAB         SEE IF 1ST FIELD IS AN APPL-ID               
         OC    12(10,R3),SPACES                                                 
*                                                                               
VR110    CLC   12(4,R3),0(R5)      MATCH WITH THIS APPL-ID?                     
         BE    VR115                                                            
         LA    R5,L'FACIDTAB(R5)                                                
         CLI   0(R5),X'FF'                                                      
         BNE   VR110                                                            
         B     VR120               NOT AN APPL-ID, SEE IF APPL SYSTEM           
*                                                                               
VR115    MVC   CTPHVFAC,4(R5)      COPY THE APPLICATION NUMBER                  
         B     VR140               GET THE VERSION LETTER                       
*                                                                               
VR120    L     R5,ASELIST                                                       
         USING SELISTD,R5                                                       
         LH    R0,0(R5)            R6 = LENGTH OF TABLE ENTRY                   
         L     R1,2(R5)            R7 = A(END OF TABLE)                         
         LA    R5,6(R5)            R5 = A(FIRST TABLE ENTRY)                    
*                                                                               
VR120LP  CLC   SENAME,12(R3)       MATCH ON NAME?                               
         BE    *+12                                                             
         BXLE  R5,R0,VR120LP                                                    
         B     VR130               NOT AN APPL SYSTEM, SEE IF AGENCY            
*                                                                               
         MVC   CTPHVSEN,SESYS      COPY THE SYSTEM NUMBER                       
         B     VR140               GET THE VERSION LETTER                       
*                                                                               
VR130    CLI   0(R3),2             TWO BYTE AGENCY POWER CODE?                  
         BNE   INVLVRCL            NO, ERROR                                    
*                                                                               
         MVC   PREVKEY(L'CTPHPKEY),KEY    MAKE A COPY OF THE PHASE KEY          
*                                                                               
         XC    KEY,KEY             SEE IF VALID AGENCY ALPHA POWER CODE         
         LA    R5,KEY                                                           
         USING CT5KEY,R5                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,12(R3)                                                  
         DROP  R5                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'CT5KEY),KEYSAVE                                            
         BNE   INVLVRCL            NOT A VALID AGENCY ALPHA ID                  
*                                                                               
         MVC   KEY(L'PREVKEY),PREVKEY  RESTORE OUR PHASE KEY                    
         MVC   AIO,AIO1                                                         
         MVC   CTPHVAGY,12(R3)     COPY THE AGENCY ALPHA                        
*                                                                               
VR140    MVC   CTPHVVRS,22(R3)     COPY THE VERSION LETTER                      
         LA    R2,CTPHVNXT         R2 = A(NEXT VERSION ELEM ENTRY)              
*                                                                               
         ZIC   R1,VERSELEM+1       CALCULATE NEW ELEMENT LENGTH                 
         LA    R1,5(R1)                                                         
         STC   R1,VERSELEM+1                                                    
*                                                                               
         LA    R3,32(R3)           R3 = A(NEXT BLOCK ENTRY)                     
*                                                                               
         BCT   R4,VR100LP          LOOP UNTIL NO MORE BLOCK ENTRIES             
*                                                                               
* VALIDATE THE NOTIFY USER AFTER FIELD                                          
*                                                                               
VR150    LA    R2,SFMNDTEH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         BAS   RE,VALIADTE                                                      
*                                                                               
         LA    R6,PERVALBK                                                      
         USING PERVALD,R6                                                       
         GOTO1 DATCON,DMCB,(5,0),(2,DATEBIN1)                                   
*                                                                               
         CLC   PVALCSTA,DATEBIN1   COMPARE INPUTTED DATE WITH TODAY             
         BL    INVLDATE            TELL USER MUST BE GREATER                    
         MVC   DATEBIN1,PVALCSTA   COPY COMPRESSED FOR LATER USE                
         MVC   8(8,R2),PVALCPER    RECOPY NEW VALID DATE                        
         OI    6(R2),X'80'         TRANSMIT NEW FORMAT                          
         DROP  R6                                                               
         EJECT                                                                  
* HANDLE THE SYSTEM ELEMENT                                                     
BLDREC   MVI   ELCODE,CTPHSCEQ     REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM             SET UP THE SYSTEM ELEMENT                    
         USING CTPHSYSD,R3                                                      
         MVI   CTPHSCDE,CTPHSCEQ   LOAD UP THE ELEMENT CODE                     
         MVI   CTPHSYLN,CTPHSLNQ   LOAD UP THE LENGTH                           
*                                                                               
         MVC   HALF(1),SFMSTND     LOAD UP THE START NODE                       
         MVC   HALF+1(1),SFMENND   LOAD UP THE END NODE                         
         GOTO1 HEXIN,DMCB,HALF,CTPHSNDE,2   1 BYTE REP. OF NODES                
*                                                                               
         CLI   SFMSPREH+5,0        NOTHING IN SPARE BYTES?                      
         BE    BLDCRRE             AUTOMATICALLY ZERO                           
         ZIC   R4,SFMSPREH+5       LENGTH TO USE WITH PACK                      
         BCTR  R4,0                                                             
         EX    R4,*+8              CHANGE LENGTH IN PACK, PACK, BRANCH          
         B     *+10                                                             
         PACK  DUB,SFMSPRE(0)      PACK THE EBCDIC NUMBER                       
         CVB   R5,DUB              CONVERT PACKED DECIMAL TO BINARY             
         STCM  R5,15,CTPHSSPR      STORE THE 4 BYTES TO SPARE                   
*                                                                               
BLDCRRE  CLI   SFMCRRE,C'Y'        IS IT CORE RESIDENT?                         
         BNE   BLDDUMM             NO, CHECK FOR CORE=DUMMY                     
         OI    CTPHSFL1,CTPHSCRQ   SET THE FLAG FOR SUCH                        
BLDDUMM  CLI   SFMDUMM,C'Y'        CORE=DUMMY?                                  
         BNE   BLDOFFL             NO, CHECK FOR OFF-LINE ONLY                  
         OI    CTPHSFL1,CTPHSDMQ   SET THE FLAG FOR SUCH                        
BLDOFFL  CLI   SFMOFFL,C'Y'        IS IT OFF-LINE ONLY?                         
         BNE   BLDPORS             NO, CHECK FOR SCREEN                         
         OI    CTPHSFL1,CTPHSOFQ   SET THE FLAG FOR SUCH                        
BLDPORS  CLI   SFMPORS,C'S'        IS IT A SCREEN?                              
         BNE   BLDPORS1            NO, BUILD THE RELOAD DATE                    
         OI    CTPHSFL1,CTPHSSCQ   SET THE FLAG FOR SUCH                        
         B     BLDRDTE                                                          
BLDPORS1 CLI   SFMPORS,C'R'        TEST SPECIAL CORE RES SCREEN                 
         BNE   BLDRDTE                                                          
         OI    CTPHSFL1,CTPHSSCQ+CTPHSCSQ                                       
*                                                                               
BLDRDTE  CLI   SFMRDTEH+5,0        NO RELOAD DATE?                              
         BE    BLDSYSX             NONE                                         
         GOTO1 DATCON,DMCB,(2,RLODDATE),(19,CTPHSRDT)                           
*                                                                               
BLDSYSX  GOTO1 ADDELEM             ADD ELEMENT IN ELEM TO RECORD (AIO)          
         CLI   DMCB+12,0           SUCCESSFULL OPERATION?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* HANDLE THE RELOAD LIST ELEMENT                                                
         MVI   ELCODE,CTPHLCEQ     REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         CLI   SFMRLS1H+5,0        DO WE NEED TO ADD RELOAD LIST?               
         BE    BLDDESC             NO, BUILD DESCRIPTION INSTEAD                
         XC    ELEM,ELEM                                                        
         USING CTPHLSTD,R3                                                      
         LA    R6,CTPHLLST         POINT TO FIRST ENTRY IN LIST                 
         MVI   CTPHLCDE,CTPHLCEQ   LOAD UP THE ELEMENT CODE                     
         CLC   =C'ALL',SFMRLS1     LOAD ALL ITEMS?                              
         BNE   BLDLST1             NO                                           
         OI    CTPHLFLG,CTPHLALQ   FLAG IT ALSO                                 
         LA    R4,CTPHLOVQ                                                      
         STC   R4,CTPHLLEN         STORE THE LENGTH                             
         B     BLDLSTDN            ADD THE RELOAD LIST ELEMENT                  
BLDLST1  ZIC   R1,PREVHEX          # OF ITEM IN SORT TABLE                      
         LA    R1,CTPHLOVQ(R1)     LENGTH OF LIST ELEMENT                       
         MVC   CTPHLLST(L'THETABLE),THETABLE COPY WHOLE SORT TABLE OVER         
         STC   R1,CTPHLLEN         STORE THE LENGTH                             
BLDLSTDN GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0           SUCCESSFULL OPERATION?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* HANDLE THE DESCRIPTION ELEMENT                                                
BLDDESC  MVI   ELCODE,CTPHDCEQ     REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         USING CTPHDSCD,R3                                                      
         MVI   CTPHDCDE,CTPHDCEQ   LOAD UP THE ELEMENT CODE                     
         MVC   CTPHDDSC(L'SFMDESC),SFMDESC  COPY THE DESCRIPTION                
         ZIC   R5,SFMDESCH+5       GET THE LENGTH                               
         LA    R5,CTPHDOVQ(R5)                                                  
         STC   R5,CTPHDLEN         STORE THE LENGTH                             
         GOTO1 ADDELEM             ADD ELEMENT TO THE RECORD                    
         CLI   DMCB+12,0           SUCCESSFULL OPERATION?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* HANDLE THE COMMENT ELEMENTS                                                   
BLDCMNT  MVI   ELCODE,CTPHCCEQ     REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         LA    R4,1                LINE COUNTER                                 
         LA    R2,SFMCMT1H         FIRST LINE                                   
BLDCMTLP CLI   5(R2),0                                                          
         BE    INCRCMT                                                          
         XC    ELEM,ELEM                                                        
         USING CTPHCOMD,R3                                                      
         MVI   CTPHCCDE,CTPHCCEQ   LOAD UP THE ELEMENT CODE                     
         STC   R4,CTPHCLNM         STORE THE LINE NUMBER                        
         MVC   CTPHCTXT(L'SFMCMT1),8(R2)  COPY THE COMMENT                      
         ZIC   R5,5(R2)            GET THE LENGTH                               
         LA    R5,CTPHCOVQ(R5)                                                  
         STC   R5,CTPHCLEN         STORE THE LENGTH                             
         GOTO1 ADDELEM             ADD ELEMENT TO THE RECORD                    
         CLI   DMCB+12,0           SUCCESSFULL OPERATION?                       
         BE    *+6                                                              
         DC    H'0'                                                             
INCRCMT  CH    R4,=H'3'            3RD LINE DONE?                               
         BE    BLDVERS                                                          
         LA    R4,1(R4)            NO, GO TO THE NEXT LINE                      
         ZIC   R5,0(R2)            LENGTH TO GET TO NEXT LINE                   
         AR    R2,R5               ADD IT TO ADDRESS OF CURRENT LINE            
         B     BLDCMTLP            CONTINUE PROCESSING                          
         DROP  R3                                                               
         EJECT                                                                  
* HANDLE THE VERSION CONTROL ELEMENT                                            
BLDVERS  MVI   ELCODE,CTPHVCEQ     REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         CLI   VERSELEM,CTPHVCEQ   ANY VERSION CONTROL ELEMENT?                 
         BNE   XIT                 NO, EXIT                                     
*                                                                               
         MVC   ELEM,VERSELEM                                                    
*                                                                               
         USING CTPHVRSD,R3                                                      
         GOTO1 DATCON,DMCB,(2,DATEBIN1),(19,CTPHVDTE)                           
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO THE RECORD                    
         CLI   DMCB+12,0           SUCCESSFULL OPERATION?                       
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE                                                               
*                                                                               
* ON ENTRY:    (R2)                A(FIELD HEADER)                              
*                                                                               
* ON EXIT:     PERVALBK            PERVAL OUTPUT AREA                           
***********************************************************************         
VALIADTE NTR1                                                                   
         TM    4(R2),X'04'         VALID ALPHA?                                 
         BZ    VDTE10              NO                                           
         CLI   5(R2),3             GREATER THAN 3 LETTERS?                      
         BH    VDTE30              THEN PERVAL IT                               
         LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         GOTO1 VDAYVAL,DMCB,(R3),DUB,DUB+1                                      
         CLI   DUB,0               VALID ALPHA DAY?                             
         BE    VDTE30              MAYBE PERVAL CAN HANDLE IT                   
         MVC   DATEBIN(1),DUB+1    GET NUMBER EQUIVALENT OF DAY                 
         NI    DATEBIN,X'0F'       REMOVE THE DUPLICATE                         
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)  TODAY'S DATE                        
         GOTO1 GETDAY,DMCB,TODAY,DUB                                            
         MVC   DATEBIN+1(1),DMCB   GET NUMBER EQUIV. OF DAY FOR TODAY           
         ZIC   R1,DATEBIN                                                       
         CLC   DATEBIN(1),DATEBIN+1                                             
         BH    *+8                                                              
         LA    R1,7(R1)            ADD A WEEK AHEAD                             
         MVI   DATEBIN,0                                                        
         SH    R1,DATEBIN          DIFFERENCE IN # OF DAYS                      
         MVC   8(4,R2),=C'T(1)'                                                 
         STC   R1,10(R2)                                                        
         OI    10(R2),X'F0'        MAKE EBCDIC NUMERIC                          
         MVI   5(R2),4                                                          
         B     VDTE30              LET PERVAL HANDLE IT FROM HERE               
*                                                                               
VDTE10   TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    VDTE30              NO                                           
         CLI   5(R2),2             1 OR 2 DIGIT NUMERIC?                        
         BH    VDTE30              NO, GO THROUGH PERVAL                        
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
         BCTR  R1,0                -1 FOR PACK                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         CONVERT TO PACKED                            
         CVB   R0,DUB              CONVERT TO BINARY                            
         GOTO1 DATCON,DMCB,(5,0),(3,DUB)  GET TODAY'S DATE 3 BYTE BIN           
         MVC   DUB+3(3),DUB        MAKE A COPY                                  
         STC   R0,DUB+2            STORE THE DAY AWAY                           
         CLM   R0,1,DUB+5          DAY GREATER?                                 
         BH    VDTE20              YES, SAME MONTH                              
         IC    R0,DUB+1            GET MONTH(TODAY)                             
         AH    R0,=H'1'            NEXT MONTH                                   
         STC   R0,DUB+1            STORE IT                                     
         CLI   DUB+1,12            MONTH(TODAY) DECEMBER?                       
         BNH   VDTE20              NO                                           
         IC    R0,DUB              GET YEAR(TODAY)                              
         AH    R0,=H'1'            NEXT YEAR                                    
         STC   R0,DUB              STORE IT                                     
         MVI   DUB+1,1             MONTH IS JANUARY                             
VDTE20   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(11,8(R2))  CONVERT THE DATE                 
         MVI   5(R2),8             NEW LENGTH IS 8                              
VDTE30   LA    R3,8(R2)            PREPARE FIRST PARAMETER FOR PERVAL           
         ICM   R3,8,5(R2)          LENGTH OF INPUT                              
         O     R3,=X'40000000'     MM/DD  NOT  MM/YY                            
         LA    R4,PERVALBK         ADDR OF OUTPUT AREA                          
*        ICM   R4,8,LANGNUM        ONLY IF WE WANT DATE IN LANG FORMAT          
         O     R4,=X'40000000'     SINGLE DATE ONLY IS VALID                    
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
         TM    DMCB+4,X'03'        DID WE GET VALID INPUT?                      
         BNZ   INVLFLD             NO                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORDS ON SCREEN                                          *         
***********************************************************************         
LR       LA    R2,KEY                                                           
         USING CTPHRECD,R2                                                      
*                                                                               
         CLI   PREVFLAG,1          A PREVIOUS KEY EXAMINED?                     
         BNE   LR10                NO, THEN CONTINUE                            
         MVC   KEY(L'PREVKEY),PREVKEY     USE PREVIOUS KEY                      
         MVI   PREVFLAG,0                                                       
         B     LRFRST              LOOK FOR THE PREVIOUS KEY                    
*                                                                               
LR10     OC    KEY(L'CTPHPKEY),KEY   FIRST TIME THROUGH?                        
         BNZ   LRFRST              NO                                           
*                                                                               
         MVI   CTPHID,CTPHIDQ      COPY HEADER INFORMATION FOR KEY              
         MVI   CTPHSUBI,CTPHSUBQ                                                
*                                                                               
         CLI   SFLPHSEH+5,0        ANYTHING IN THE PHASE FIELD?                 
         BE    LRSVKEY             NO, LIST ALL PHASES                          
*                                                                               
         CLI   SFLPHSE,C'T'        OFF-LINE PHASE?                              
         BE    LR20                                                             
         ZIC   R1,SFLPHSEH+5       YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     LRSVKEY                                                          
         MVC   CTPHNAME(0),SFLPHSE                                              
*                                                                               
LR20     MVC   DUB,SFLPHSE                                                      
         MVI   DUB,C'0'            DON'T NEED THE BEGINNING T                   
         ZIC   R3,SFLPHSEH+5       FIND LENGTH OF INPUT                         
         CLI   SFLPHSEH+5,0                                                     
         BNE   LRCNVHEX                                                         
         LA    R3,1                DEFAULT LENGTH OF 1                          
LRCNVHEX GOTO1 HEXIN,DMCB,DUB,CTPHHEXN,(R3)  PHASE TO LOOK FOR                  
*                                                                               
LRSVKEY  MVC   SAVEKEY,KEY         SAVE THE KEY                                 
*                                                                               
LRFRST   GOTO1 HIGH                LOOK FOR FIRST RECORD                        
*                                                                               
LRPHSE   CLC   KEY(2),SAVEKEY      CHECK IF STILL A PHASE REC                   
         BNE   XIT                 NOT ANYMORE                                  
*                                                                               
         MVC   THEPHASE,CTPHNAME                                                
         CLI   CTPHNAME,0          OFFLINE?                                     
         BNE   LRPHSE05            YES                                          
         GOTO1 HEXOUT,DMCB,CTPHHEXN,THEPHASE,L'CTPHHEXN                         
         MVI   THEPHASE,C'T'                                                    
*                                                                               
LRPHSE05 CLI   SFLPHSEH+5,0        ALL PHASE RECORDS?                           
         BE    LRLANG              THEN CHECK THE LANGUAGES                     
*                                                                               
         CLI   SFLPHSE,C'T'        OFF-LINE PHASE?                              
         BE    LRPHSE10                                                         
         ZIC   R1,SFLPHSEH+5       YES, CHECK AGAINST THE NAME                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTPHNAME(0),SFLPHSE                                              
         BNE   XIT                                                              
         MVC   THEPHASE,CTPHNAME                                                
         B     LRLANG              NO CHANCE                                    
*                                                                               
LRPHSE10 ZIC   R3,SFLPHSEH+5       LOAD UP THE LENGTH                           
         BCTR  R3,0                DECREMENT FOR CLC                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   THEPHASE(0),SFLPHSE                                              
         BNE   XIT                                                              
*                                                                               
LRLANG   TM    FILTRFLG,X'40'      ALL LANGUAGES?                               
         BZ    LRLVEL              YES (ALL=0)                                  
         CLC   CTPHLANG,LANGNUM    IS FILTER ON LANGUAGE GOOD?                  
         BNE   LRNXT               NO, LOOK FOR THE NEXT ONE                    
*                                                                               
LRLVEL   TM    FILTRFLG,X'20'      ALL LEVELS?                                  
         BZ    LRRDTE              YES                                          
         CLC   CTPHLVL,LVELNUM     IS FILTER ON LEVEL GOOD?                     
         BNE   LRNXT                                                            
*                                                                               
LRRDTE   TM    FILTRFLG,X'10'      ALL RELOAD DATES?                            
         BZ    LRCDTE              YES, NOW SHOW THE LINE                       
         XC    DATADISP,DATADISP   ZERO OUT THE DISPLACEMENT                    
         MVI   DATADISP+1,CTPHOVEQ                                              
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHSCEQ     SYSTEM ELEMENT CODE                          
         BAS   RE,GETEL            POINT TO THE SYSTEM ELEMENT                  
         BE    *+6                 SUPPOSE TO EXIST                             
         DC    H'0'                DIE IF IT DOESN'T                            
         USING CTPHSYSD,R6                                                      
         CLC   RLDATE,CTPHSRDT     IS FILTER ON RELOAD DATE GOOD?               
         BNE   LRNXT               NO, LOOK FOR THE NEXT ONE                    
         DROP  R6                                                               
*                                                                               
LRCDTE   TM    FILTRFLG,X'08'      ALL CHANGE DATES?                            
         BZ    LRDISP              YES, NOW SHOW THE LINE                       
         XC    DATADISP,DATADISP   ZERO OUT THE DISPLACEMENT                    
         MVI   DATADISP+1,CTPHOVEQ                                              
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            POINT TO THE SYSTEM ELEMENT                  
         BNE   LRNXT               SKIP RECORD IF IT DOESN'T EXIST              
         USING ACTVD,R6                                                         
         CLC   CHDATE,ACTVCHDT     IS FILTER ON CHANGE DATE GOOD?               
         BNE   LRNXT               NO, LOOK FOR THE NEXT ONE                    
         DROP  R6                                                               
*                                                                               
LRDISP   MVC   LISTAR,SPACES       CLEAR OUT THE DISPLAY LINE                   
         MVC   LSTPHSE,THEPHASE    COPY PHASE NAME TO DISPLAY                   
         CLI   CTPHLVL,0           NO LEVEL?                                    
         BE    LRDISP1                                                          
         MVC   LSTLVEL,CTPHLVL     PUT LEVEL NEXT TO THE PHASE                  
LRDISP1  L     R3,AFAALANG         ADDRESS OF LANGUAGE TABLE                    
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF TABLE                                 
         LA    R3,6(R3)            FIRST ENTRY OF LANGUAGE TABLE                
LRDISPLP CLC   CTPHLANG,LANGCODE   SAME LANGUAGE?                               
         BE    *+10                YES                                          
         BXLE  R3,R4,LRDISPLP      NO, TRY THE NEXT ENTRY                       
         DC    H'0'                BAD CODE                                     
         MVC   LSTLANG,SPACES                                                   
         MVC   LSTLANG(L'LANGSHR),LANGSHR                                       
         DROP  R3                                                               
         EJECT                                                                  
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHSCEQ     GET THE SYSTEM ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                 DIE IF NO SYSTEM ELEMENT                     
         DC    H'0'                                                             
         USING CTPHSYSD,R6                                                      
         CLC   CTPHSRDT,=3X'00'    NO DATE?                                     
         BE    LRNODES             THEN DON'T SHOW IT                           
         GOTO1 DATCON,DMCB,(8,CTPHSRDT),(11,LSTRDTE)                            
LRNODES  GOTO1 HEXOUT,DMCB,CTPHSNDE,LSTNODE,1                                   
         EDIT  (B4,CTPHSSPR),(10,LSTSPRE),0,ALIGN=LEFT                          
         TM    CTPHSFL1,CTPHSCRQ                                                
         BZ    LRDUMM                                                           
         MVI   LSTCDOS,C'C'                                                     
LRDUMM   TM    CTPHSFL1,CTPHSDMQ                                                
         BZ    LROFFL                                                           
         MVI   LSTCDOS+1,C'D'                                                   
LROFFL   TM    CTPHSFL1,CTPHSOFQ                                                
         BZ    LRSCRN                                                           
         MVI   LSTCDOS+2,C'O'                                                   
LRSCRN   TM    CTPHSFL1,CTPHSSCQ                                                
         BZ    LRDESC                                                           
         MVI   LSTCDOS+3,C'S'                                                   
         DROP  R6                                                               
         EJECT                                                                  
LRDESC   L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHDCEQ     GET THE DESCRIPTION ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                 DIE IF NO DESCRIPTION ELEMENT                
         DC    H'0'                                                             
         USING CTPHDSCD,R6                                                      
         CLI   CTPHDLEN,L'LSTDESC+CTPHDOVQ  MORE THAN CAN FIT?                  
         BH    LRDSPMRE            YES                                          
         MVC   LSTDESC,SPACES                                                   
         ZIC   R3,CTPHDLEN         GET THE LENGTH OF THE DESCRP.                
         SH    R3,=H'3'            OVERHEAD OF 2 AND 1 FOR MVC                  
         EX    R3,*+8                                                           
         B     LRSHOW                                                           
         MVC   LSTDESC(0),CTPHDDSC COPY THE COMMENT OVER                        
LRDSPMRE MVC   LSTDESC,CTPHDDSC                                                 
         DROP  R6                                                               
         EJECT                                                                  
LRSHOW   GOTO1 LISTMON                                                          
LRNXT    GOTO1 SEQ                                                              
         B     LRPHSE                                                           
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* HEADER ROUTINE                                                                
*                                                                               
HDHK     NTR1                                                                   
         CLI   USEHDHK,C'Y'        OKAY TO USE HEADER?                          
         BNE   HDHKX               NO                                           
         MVC   H1+58(13),=C'PHASE LISTING'                                      
         MVI   H2+58,C'-'          UNDERLINE                                    
         MVC   H2+59(12),H2+58                                                  
         MVI   H3,0                                                             
* COLUMN HEADINGS                                                               
         MVC   H4(5),=C'PHASE'                                                  
         MVC   H4+8(4),=C'LANG'                                                 
         MVC   H4+15(6),=C'RELOAD'                                              
         MVC   H4+48(11),=C'DESCRIPTION'                                        
         MVC   H4+87(4),=C'NODE'                                                
         MVC   H4+92(3),=C'S/P'                                                 
         MVC   H4+98(7),=C'OPTIONS'                                             
         MVC   H4+109(12),=C'DATE CHANGED'                                      
         MVC   H4+124(5),=C'SPARE'                                              
         MVI   H5,C'-'          UNDERLINE                                       
         MVC   H5+1(131),H5                                                     
HDHKX    XIT1                                                                   
         SPACE 2                                                                
HEADING  SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE FACIDTAB                                                       
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
RELO     DS    A                                                                
ACTCOPY  EQU   15                                                               
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
INVLTLVL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TLVLMSG),TLVLMSG                                       
         LA    R2,SFMLVELH                                                      
         B     MYERROR                                                          
TLVLMSG  DC    C'ERROR: PRODUCTION LEVEL MUST EXIST FIRST'                      
*                                                                               
INVLDESC LA    R2,SFMDESCH                                                      
         B     MISSFLD                                                          
*                                                                               
INVLADD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
         LA    R2,SFMPHSEH                                                      
         B     MYERROR                                                          
ADDMSG   DC    C'ERROR: THE BASE FOR THIS PHASE HAS NOT BEEN ENTERED'           
*                                                                               
INVLSTND LA    R2,SFMSTNDH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLENND LA    R2,SFMENNDH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLPORS LA    R2,SFMPORSH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLCRRE LA    R2,SFMCRREH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLDUMM LA    R2,SFMDUMMH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLCRDM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CRDMMSG),CRDMMSG                                       
         LA    R2,SFMDUMMH                                                      
         B     MYERROR                                                          
CRDMMSG  DC    C'ERROR: CANNOT HAVE BOTH CORE RESIDENT AND CORE=DUMMY'          
*                                                                               
INVLSPRE LA    R2,SFMSPREH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLOVFL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OVFLMSG),OVFLMSG                                       
         LA    R2,SFMSPREH                                                      
         B     MYERROR                                                          
OVFLMSG  DC    C'ERROR: MAXIMUN NUMBER FOR THIS FIELD WAS SURPASSED'            
*                                                                               
INVLOFFL LA    R2,SFMOFFLH                                                      
         B     INVLFLD                                                          
*                                                                               
INVLCROF XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CROFMSG),CROFMSG                                       
         LA    R2,SFMOFFLH                                                      
         B     MYERROR                                                          
CROFMSG  DC    C'ERROR: OFF-LINE CANNOT BE CORE'                                
*                                                                               
MISSRDTE LA    R2,SFMRDTEH                                                      
         B     MISSFLD                                                          
*                                                                               
INVLDATE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DATEMSG),DATEMSG                                       
         B     MYERROR                                                          
DATEMSG  DC    C'ERROR: DATE MUST BE AFTER TODAY'                               
*                                                                               
INVLLIST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
         LA    R2,SFMRLS1H                                                      
         B     MYERROR                                                          
LISTMSG  DC    C'ERROR: ONLY THE CONTROLLER(00) CAN HAVE A RELOAD LIST'         
*                                                                               
INVLRLST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RLSTMSG),RLSTMSG                                       
         LA    R2,SFMRLS1H                                                      
         B     MYERROR                                                          
RLSTMSG  DC    C'ERROR: ONLY 2 DIGIT HEX NUMBERS SEPARATED BY COMMAS'           
*                                                                               
INVLRELD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RELDMSG),RELDMSG                                       
         LA    R2,SFMRLS1H                                                      
         B     MYERROR                                                          
RELDMSG  DC    C'ERROR: CONTROLLER CANNOT RELOAD ITSELF'                        
*                                                                               
INVLITEM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ITEMMSG),ITEMMSG                                       
         LA    R2,SFMRLS1H                                                      
         B     MYERROR                                                          
ITEMMSG  DC    C'ERROR: DUPLICATE ITEM IN LIST'                                 
*                                                                               
INVLVRCL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VRCLMSG),VRCLMSG                                       
         LA    R2,SFMVRC1H                                                      
         B     MYERROR                                                          
VRCLMSG  DC    C'ERROR: INVALID PARAMETER'                                      
*                                                                               
NOTEXIST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EXISTMSG),EXISTMSG                                     
         LA    R2,SFCPHS1H                                                      
         B     MYERROR                                                          
EXISTMSG DC    C'ERROR: RECORDS DO NOT EXIST'                                   
*                                                                               
CANTDELT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DLETEMSG),DLETEMSG                                     
         LA    R2,SFMRDTEH                                                      
         B     MYERROR                                                          
DLETEMSG DC    C'ENTER THE WORD  "DELETE"  HERE TO DELETE THE RECORD'           
*                                                                               
* R3 POINTS TO THE HEX EQUIV OF THE ITEM IN LIST THAT DOESN'T EXIST             
INVLPHSE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PHSEMSG),PHSEMSG                                       
         LA    R4,CONHEAD+L'PHSEMSG                                             
         GOTO1 HEXOUT,DMCB,(R3),(R4),1                                          
         LA    R2,SFMRLS1H                                                      
         B     MYERROR                                                          
PHSEMSG  DC    C'ERROR: ITEM OF THE SAME LEVEL HAS NOT BEEN ADDED: #'           
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* COPY A SET OF PHASE FROM ANOTHER SET OF PHASES.                     *         
***********************************************************************         
COPYFROM DS    0H                                                               
         NMOD1 0,**CPYF**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
*                                                                               
         LA    R2,SFCPHS2H         SECOND PHASE ENTRY                           
         CLI   8(R2),C'T'                                                       
         BNE   INVLFLD                                                          
         CLC   5(1,R2),SFCPHS1H+5  LENGTHS HAVE TO BE THE SAME                  
         BNE   INVLFLD                                                          
         LA    R5,9(R2)                                                         
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         GOTO1 HEXIN,DMCB,(R5),DUB,(R4)                                         
         CLI   DMCB+15,0                                                        
         BE    INVLFLD                                                          
         ZIC   R4,5(R2)                                                         
         XC    HEXPHASE,HEXPHASE                                                
         MVC   DUB(L'SFMPHSE),SFCPHS1                                           
         MVI   DUB,C'0'                                                         
         GOTO1 HEXIN,DMCB,DUB,HEXPHASE,(R4)                                     
         XC    HEXPHSE2,HEXPHSE2                                                
         MVC   DUB(L'SFMPHSE),SFCPHS2                                           
         MVI   DUB,C'0'                                                         
         GOTO1 HEXIN,DMCB,DUB,HEXPHSE2,(R4)                                     
         LA    R2,SFCLNG2H                                                      
         CLI   5(R2),0                                                          
         BNE   CPLANG0                                                          
         MVC   8(3,R2),=C'ENG'     DEFAULT IS ENGLISH                           
         MVI   5(R2),L'SFMLANG     DEFAULT INPUT LENGTH                         
         OI    6(R2),X'80'         DISPLAY THE DEFAULT                          
CPLANG0  CLI   5(R2),3             3 CHARACTER LANGUAGE CODE?                   
         BNE   INVLFLD                                                          
         L     R3,AFAALANG         ADDRESS OF LANGUAGE TABLE                    
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF THE TABLE                             
         LA    R3,6(R3)            FIRST ENTRY OF LANGUAGE TABLE                
*                                                                               
* CHECKING THROUGH THE LANGUAGE TABLE                                           
*                                                                               
CPLANGLP CLC   8(L'SFMLANG,R2),LANGSHR  MATCH ON SHORT LANGUAGE NAME?           
         BE    CPLANG1                                                          
         CLC   8(L'SFMLANG,R2),LANGSHRN  MATCH ON NATIVE LANGUAGE NAME?         
         BE    CPLANG1                                                          
         BXLE  R3,R4,CPLANGLP      WHILE R6<=R5, R6:=R6+R4                      
         B     INVLFLD             INVALID LANGUAGE                             
CPLANG1  MVC   LANGNUM2,LANGCODE   GOOD LANGUAGE CODE                           
         XI    LANGNUM2,X'FF'      FLIP THE BITS OF LANG CODE                   
*&&US*&& CLI   LANGNUM2,X'FE'      IN U.S., LANG EUK NOT ALLOWED                
*&&UK*&& CLI   LANGNUM2,X'FD'      IN U.K., LANG EUS NOT ALLOWED                
         BE    INVLFLD                                                          
         XI    LANGNUM2,X'FF'      FLIP THE BITS OF LANG CODE AGAIN             
         LA    R2,SFCLVL2H                                                      
         CLI   5(R2),0             DO WE HAVE TO TEST THE LEVEL?                
         BNE   CPYFROM1            NO WE DON'T, NO LEVEL INPUTTED               
         MVI   LVELNUM2,0                                                       
         B     CPYFROM2                                                         
CPYFROM1 CLI   8(R2),C'A'          VALID LEVELS BTWN A AND C                    
         BL    INVLFLD                                                          
         CLI   8(R2),C'C'                                                       
         BH    INVLFLD                                                          
         MVC   LVELNUM2,8(R2)                                                   
CPYFROM2 CLC   LVELNUM,LVELNUM2    LEVEL THE SAME?                              
         BNE   CPYFROM3                                                         
         CLC   LANGNUM,LANGNUM2    LANGUAGE THE SAME?                           
         BNE   CPYFROM3                                                         
         LA    R2,SFCPHS2H         POINT TO THE "TO" PHASE                      
         CLC   HEXPHASE,HEXPHSE2   PHASE THE SAME?                              
         BE    INVLFLD             YES, CAN'T COPY TO ITSELF                    
CPYFROM3 LA    R4,KEY              CHECK IF ANY 'TO' DETAILS EXIST              
         XC    KEY,KEY                                                          
         USING CTPHRECD,R4                                                      
         MVI   CTPHID,CTPHIDQ      PHASE IDENTIFIERS                            
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVC   CTPHHEXN,HEXPHASE   COPY PHASE SUFFIX                            
         MVC   SAVEKEY,KEY         MAKE A COPY                                  
         SR    R5,R5               RECORD COUNTER                               
         MVI   PHSCOUNT,0          NUMBER COPIED                                
CPYFRST  GOTO1 HIGH                GOTO THE FIRST RECORD                        
         CH    R5,=H'0'            FIRST TIME THROUGH?                          
         BE    CPYREC                                                           
         LR    R0,R5               MAKE A COPY OF THE COUNTER                   
CPYRDLP  GOTO1 SEQ                                                              
         BCT   R5,CPYRDLP                                                       
         LR    R5,R0               LOAD BACK THE COUNTER                        
CPYREC   CLC   =X'0501',KEY        MAKE SURE STILL PHASE REC                    
         BE    *+16                                                             
         CLI   PHSCOUNT,0          NOTHING COPIED AND NO MATCH?                 
         BE    NOTEXIST            YES, NONE                                    
         B     XIT                 NO, DONE COPYING                             
         GOTO1 HEXOUT,DMCB,CTPHHEXN,THEPHASE,L'CTPHHEXN                         
         MVI   THEPHASE,C'T'                                                    
         ZIC   R1,SFCPHS1H+5                                                    
         LA    R2,SFCPHS1H         POINT TO THE "TO" PHASE                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THEPHASE(0),8(R2)   MAKE SURE PHASE OKAY                         
         BE    *+16                                                             
         CLI   PHSCOUNT,0          NOTHING COPIED AND NO MATCH?                 
         BE    NOTEXIST            YES, NONE                                    
         B     XIT                 NO, DONE COPYING                             
         CLC   CTPHLANG,LANGNUM    LANGUAGES MATCH?                             
         BNE   CPYNEXT             NO, NEXT RECORD                              
         CLC   CTPHLVL,LVELNUM     LEVELS MATCH?                                
         BNE   CPYNEXT             NO, NEXT RECORD                              
         IC    R1,PHSCOUNT         NUMBER COPIED INCREMENTED BY 1               
         LA    R1,1(R1)                                                         
         STC   R1,PHSCOUNT                                                      
         MVC   PREVKEY,KEY         MAKE A COPY OF KEY BEING COPIED              
CPYCHECK MVI   THEPHASE,C'0'                                                    
         ZIC   R1,SFCPHS2H+5                                                    
         SH    R1,=H'2'            -1 FOR 'T', -1 FOR MVC                       
         EX    R1,*+8                                                           
         B     *+10                   IE: T15D??                                
         MVC   THEPHASE+1(0),SFCPHS2+1    TF05??                                
         GOTO1 HEXIN,DMCB,THEPHASE,CTPHHEXN,L'THEPHASE                          
         MVC   CTPHLANG,LANGNUM2                                                
         MVC   CTPHLVL,LVELNUM2    KEY IS COMPLETED                             
         MVC   PERVALBK(L'CTPHPKEY),KEY   JUST TEMPORARY STORAGE                
         GOTO1 HIGH                MAKE SURE NO DUPLICATE                       
         CLC   PERVALBK(L'CTPHPKEY),KEY   IS IT THE SAME?                       
         BE    CPYNEXT             YES, SKIP IT                                 
         MVC   KEY(L'PREVKEY),PREVKEY  NO, READ BACK THE SOURCE                 
         GOTO1 READ                WE KNOW IT'S THERE ALREADY                   
         MVC   KEY(L'CTPHPKEY),PERVALBK  FROM TEMPORARY STORAGE                 
         L     R6,AIO                                                           
         MVC   0(L'CTPHPKEY,R6),KEY  COPY KEY TO RECORD                         
*&&US                                                                           
CPYCHKED CLI   LVELNUM2,0          NOT A PRODUCTION LEVEL PHASE?                
         BE    CPYCOPY                                                          
         L     R6,AIO              TURN OF CORE-RES BIT                         
         MVI   ELCODE,CTPHSCEQ     IN THE SYSTEM ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO HAVE A SYSTEM ELEMENT                 
         USING CTPHSYSD,R6                                                      
         NI    CTPHSFL1,X'FF'-CTPHSCRQ                                          
         DROP  R6                                                               
*&&                                                                             
CPYCOPY  GOTO1 ADD                 SEE IF WE CAN ADD THIS RECORD                
         CLI   DMCB+12,0                                                        
         BE    CPYNEXT                                                          
         DC    H'0'                                                             
CPYNEXT  LA    R5,1(R5)            BUMP THE RECORD COUNTER                      
         MVC   KEY,SAVEKEY         KEY WE'RE LOOKING FOR                        
         B     CPYFRST                                                          
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT THE RECORDS ON PAPER (JCL PHASE)                              *         
***********************************************************************         
PR       DS    0H                                                               
         NMOD1 0,**PRNT**                                                       
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*  GET TODAYS DATE IN YYMMDD FORMAT                                             
         GOTO1 DATCON,DMCB,(05,DUMMY),(0,TODAY)                                 
*        MVC   DAYDATE,TODAY                                                    
         MVI   ALLOWLIN,2                                                       
PR10     LA    R2,KEY                                                           
         USING CTPHRECD,R2                                                      
         XC    KEY,KEY             SET UP TO GET FIRST PHASE RECORD             
         MVI   CTPHID,CTPHIDQ      LOAD UP IDENTIFIERS                          
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVC   SAVEKEY,KEY                                                      
*                                                                               
PRFRST   GOTO1 HIGH                GET FIRST RECORD                             
*                                                                               
PRCKPHSE CLC   KEY(2),SAVEKEY      PHASE RECORD?                                
         BNE   XIT                 NO, DONE                                     
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   DAYDATE,TODAY       RESET DATE TO TODAYS DATE                    
*                                                                               
PRLANG   TM    FILTRFLG,X'40'      FILTER ON A LANGUAGE?                        
         BZ    PRRECD              NO, ALL LANGUAGES ACCEPTED                   
         CLC   CTPHLANG,LANGNUM    DOES LANGUAGE MATCH FILTER?                  
         BNE   PRNXT               NO, GET THE NEXT RECORD                      
         EJECT                                                                  
PRRECD   CLI   CTPHNAME,0                                                       
         BE    PRRECD10                                                         
         MVC   PRTPHSE,CTPHNAME                                                 
         B     PRRECD20                                                         
*                                                                               
PRRECD10 GOTO1 HEXOUT,DMCB,CTPHHEXN,PRTPHSE,L'CTPHHEXN                          
         MVI   PRTPHSE,C'T'        PHASES BEGIN WITH A 'T'                      
*                                                                               
PRRECD20 CLI   CTPHLVL,0           NO LEVEL?                                    
         BE    *+10                NO LEVEL, PRINT THE LANGUAGE                 
         MVC   PRTLVEL,CTPHLVL     LEVEL NEXT TO THE PHASE                      
*                                                                               
         L     R3,AFAALANG         ADDR OF LANGUAGE TABLE                       
         USING LANGTABD,R3                                                      
         LH    R4,0(R3)            LENGTH OF TABLE ENTRIES                      
         L     R5,2(R3)            END OF TABLE                                 
         LA    R3,6(R3)            FIRST ENTRY IN THE LANGUAGE TABLE            
PRLNGLP  CLC   CTPHLANG,LANGCODE   SAME LANGUAGE?                               
         BE    *+10                                                             
         BXLE  R3,R4,PRLNGLP       NO, TRY THE NEXT ENTRY                       
         DC    H'0'                BAD LANGUAGE CODE SHOULDN'T EXIST            
         MVC   PRTLANG(L'LANGSHR),LANGSHR  COPY THE LANGUAGE ACROSS             
         DROP  R3                                                               
         EJECT                                                                  
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHSCEQ     GET THE SYSTEM ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                 DIE IF NO SYSTEM ELEMENT                     
         DC    H'0'                                                             
         USING CTPHSYSD,R6                                                      
*                                                                               
PRCORE   CLI   CORERES,C'N'        FILTER ON CORE RESIDENT?                     
         BE    PRDATE              CONTINUE WITH OTHER CHECK                    
         CLI   CTPHSFL1,CTPHSCRQ   DOES FLAG MATCH CORE RESIDENT VALUE          
         BE    PRDATE              CONTINUE WITH DATE                           
         CLI   CTPHSFL1,X'90'      A CORE RESIDENT SCREEN                       
         BNE   PRNXT               NO, THEN GET NEXT RECORD                     
*                                                                               
PRDATE   CLC   CTPHSRDT,=3X'00'    JAN00/00?                                    
         BE    PRNODES             YES, DON'T WANT IT THEN                      
         GOTO1 DATCON,DMCB,(8,CTPHSRDT),(11,PRTRDTE)                            
PRNODES  GOTO1 HEXOUT,DMCB,CTPHSNDE,PRTNODE,L'CTPHSNDE                          
         EDIT  (B4,CTPHSSPR),(10,PRTSPRE),0,ALIGN=LEFT                          
         MVI   PRTPORS,C'S'                                                     
         TM    CTPHSFL1,CTPHSSCQ                                                
         BNZ   PRCRRE                                                           
         MVI   PRTPORS,C'P'                                                     
PRCRRE   TM    CTPHSFL1,CTPHSCRQ                                                
         BZ    PRDUMM                                                           
         CLI   PRTLVEL,C'A'    IS IT TEST LEVEL A                               
         BNE   *+14             NO                                              
         MVC   PRTOPTN(12),=C'**C/R TEST**'                                     
         B     PRDESC                                                           
         CLI   PRTLVEL,C'B'    IS IT TEST LEVEL B                               
         BNE   *+14             NO                                              
         MVC   PRTOPTN(12),=C'**C/R TEST**'                                     
         B     PRDESC                                                           
         CLI   PRTLVEL,C'C'    IS IT TEST LEVEL C                               
         BNE   *+14             NO                                              
         MVC   PRTOPTN(12),=C'**C/R TEST**'                                     
         B     PRDESC                                                           
         MVC   PRTOPTN(13),=C'CORE-RESIDENT'                                    
         B     PRDESC                                                           
PRDUMM   TM    CTPHSFL1,CTPHSDMQ                                                
         BZ    PROFFL                                                           
         MVC   PRTOPTN(12),=C'CORE = DUMMY'                                     
         B     PRDESC                                                           
PROFFL   TM    CTPHSFL1,CTPHSOFQ                                                
         BZ    PRDESC                                                           
         MVC   PRTOPTN(13),=C'OFF-LINE ONLY'                                    
         EJECT                                                                  
PRDESC   L     R6,AIO                                                           
         MVI   ELCODE,CTPHDCEQ     GET THE DESCRIPTION ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPHDSCD,R6                                                      
         ZIC   R3,CTPHDLEN         GET LENGTH OF THE DESCRIPTION                
         SH    R3,=H'3'            OVERHEAD OF 2 AND 1 FOR MVC                  
         EX    R3,*+8                                                           
         B     PRADATE                                                          
         MVC   PRTDESC(0),CTPHDDSC   COPY DESCRIPTION OVER                      
         EJECT                                                                  
*                                                                               
* DATE PROCESSING                                                               
PRADATE  L     R6,AIO              POINT TO RECORD                              
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         CLI   SFRWEEK,C'Y'        DOES USER WANT RECS FOR LAST 2 WEEKS         
         BE    PRCHECK             YES, DO 2 WEEK PROCESSING                    
         BAS   RE,GETEL            USER WANTS ALL RECS                          
         BNE   PRCOMM              RELOAD LIST                                  
         B     PRPRTD                                                           
*                                                                               
* ONLY WANT RECORDS FOR THE LAST 14 DAYS                                        
* IF NO F1 ELEMENT, BYPASS RECORD SINCE USER WANTS ONLY NEW ONES                
PRCHECK  BAS   RE,GETEL            GET F1 ELEMENT                               
         BNE   PRNXT               IF NONE, GET ANOTHER RECORD                  
         GOTO1 ADDAY,DMCB,DAYDATE,LIMDATE,F'-14'                                
* CONVERT DATE TO BINARY FORMAT                                                 
         GOTO1 DATCON,DMCB,(0,LIMDATE),(3,BINDATE)                              
         CLC   BINDATE,ACTVCHDT    WAS RECORD CHANGED IN LAST 14 DAYS?          
         BH    PRNXT               NO, THEN DON'T PROCESS                       
*                                                                               
*  SEE IF RECORD WAS CHANGED IN THE LAST WEEK                                   
*  MAKE  DATE PRINTABLE                                                         
PRPRTD   GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,PRTDATEC)                           
*  FIND THE PREVIOUS MONDAY                                                     
PRDAY    GOTO1 GETDAY,DMCB,(DAY,DAYDATE),OUTDAY    DAY OF WEEK                  
         CLC   OUTDAY,=C'MON'   IS IT A MONDAY?                                 
         BE    PRWEEK                                                           
* DECREMENT DAY BY 1                                                            
         GOTO1 ADDAY,DMCB,DAYDATE,DAYDATE,F'-1'                                 
         B     PRDAY                                                            
* CONVERT DATE TO BINARY FORMAT                                                 
PRWEEK   GOTO1 DATCON,DMCB,(0,DAYDATE),(3,BINDATE)                              
         CLC   BINDATE,ACTVCHDT    WAS RECORD CHANGED IN LAST WEEK              
         BH    PRCOMM              NO, THEN DON'T PRINT ASTERICKS               
         MVC   PRTAST1,=C'**'                                                   
         MVC   PRTAST2,=C'**'                                                   
         EJECT                                                                  
PRCOMM   GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHCCEQ     GET COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
PRCOMMLP BNE   PRRLST              NO COMMENT. RELOAD LIST?                     
         USING CTPHCOMD,R6         COMMENT ELEMENT TEMPLATE                     
         ZIC   R3,CTPHCLEN         LOAD UP THE LENGTH                           
         SH    R3,=H'4'            OVERHEAD OF 3 AND 1 FOR MVC                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PRTDESC(0),CTPHCTXT COPY THE COMMENT OVER                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT OUT THE COMMENT                        
         BAS   RE,NEXTEL           SEE IF THERE IS ANOTHER COMMENT LINE         
         B     PRCOMMLP                                                         
         EJECT                                                                  
*                                                                               
* GET DATE LAST CHANGED IF THERE IS ONE                                         
*                                                                               
*  RELOAD LIST                                                                  
PRRLST   L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTPHLCEQ     GET RELOAD LIST ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   PRSKIPLN            NO RELOAD LIST                               
         USING CTPHLSTD,R6         RELOAD LIST TEMPLATE                         
         MVC   PRTLANG(12),=C'RELOAD LIST:'                                     
         CLI   CTPHLFLG,CTPHLALQ   ALL ITEMS?                                   
         BNE   PRLSTFEW            NO, JUST A FEW                               
         MVC   PRTPHSE+22(3),=C'ALL'                                            
         B     PRLSTLN             PRINT 'ALL'                                  
PRLSTFEW ZIC   R5,CTPHLLEN         # OF ITEMS TO BE RELOADED                    
         SH    R5,=H'3'            OVERHEAD OF 3                                
         BNP   PRSKIPLN            IGNORE BAD ELEMENTS                          
         LA    R3,CTPHLLST         SOURCE OF LIST                               
         LA    R4,PRTPHSE+22       WHERE TO OUTPUT                              
PRLSTLP  GOTO1 HEXOUT,DMCB,(R3),(R4),1   HEX INTO EBCDIC                        
         LA    R3,1(R3)            NEXT ITEM                                    
         LA    R4,2(R4)            MOVE PTR SO WE CAN PUT A COMMA               
         BCTR  R5,0                ONE LESS TO GO                               
         LTR   R5,R5               OUT OF ITEMS?                                
         BZ    PRLSTLN             YES, PRINT THE LINE                          
         MVI   0(R4),C','          OTHERWISE, PUT A COMMA                       
         LA    R4,1(R4)            ADVANCE PTR FOR NEXT ITEM                    
         B     PRLSTLP                                                          
         EJECT                                                                  
PRLSTLN  GOTO1 SPOOL,DMCB,(R8)                                                  
PRSKIPLN MVI   PRTPHSE,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
PRNXT    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PRCKPHSE            CHECK IF IT IS A PHASE RECORD                
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE CTSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMFBD          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMEBD          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMDBD          (OUR REPORT SCREEN OVERLAY)                  
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMCBD          (OUR COPY SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD        (SYSTEM AREAS)                               
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
ASELIST  DS    A                   A(SYSTEMS EXECUTIVE LIST)                    
HEXPHASE DS    XL3                 HEX EQUIVALENT OF THE PHASE                  
HEXPHSE2 DS    XL3                 HEX EQUIVALENT OF THE PHASE                  
THEPHASE DS    CL6                 EBCDIC EQUIV                                 
LANGNUM  DS    XL1                 LANGUAGE CODE                                
LANGNUM2 DS    XL1                 LANGUAGE CODE                                
LVELNUM  DS    CL1                 LEVEL REPRESENTATION                         
LVELNUM2 DS    CL1                 LEVEL REPRESENTATION                         
PHSCOUNT DS    XL1                 NUMBER OF PHASE RECS COPIED                  
RLDATE   DS    XL3                 RELOAD DATE FILTER (JULIAN)                  
CHDATE   DS    XL3                 CHANGE DATE FILTER (YMD BINARY)              
FILTRFLG DS    XL1                 TELLS WHAT IS SPECIFIC                       
CORERES  DS    XL1                 CORE RESIDENT FILTER FIELD                   
BINDATE  DS    XL3                 TODAYS DATE IN BINARY                        
DUMMY    DS    F                   DUMMY ADDRESS FOR DATCON                     
DAYDATE  DS    CL6                 DATE IN YYMMDD FORMAT                        
TODAY    DS    CL6                 DATE IN YYMMDD FORMAT                        
LIMDATE  DS    CL6                 14 DAYS BEFORE TODAY, IN YYMMDD              
DAYOUT   DS    CL6                 DATE IN YYMMDD FORMAT                        
DAY      DS    XL1                 DAY NUMBER                                   
NEGONE   DC    F'-1'                                                            
OUTDAY   DS    CL3                 DAY OF WEEK                                  
SAVEKEY  DS    CL25                SAVE AREA FOR THE KEY IN LISTS               
PREVKEY  DS    CL25                SAVE AREA FOR THE PREVIOUS KEY               
PREVFLAG DS    XL1                 FLAG INDICATING PREVIOUS                     
PERVALBK DS    CL58                STORAGE FOR PERVAL                           
PREVHEX  DS    CL1                 ITEM TO COMPARE WITH SORTED ITEM             
RLODDATE DS    XL2                 SAVED RELOAD DATE COMPRESSED                 
DATEBIN  DS    H                   DATE IN COMPRESSED BINARY FORMAT             
DATEBIN1 DS    H                   ANOTHER DATE                                 
BLOCKSZ  DS    0CL32               LENGTH OF BLOCK AREA FOR SCANNER             
THETABLE DS    0CL24                                                            
SORTTABL DS    24CL1               SORT TABLE USED FOR QSORT                    
VQSORT   DS    V                   ADDRESS OF QSORT                             
VDAYVAL  DS    V                   ADDRESS OF DAYVAL                            
USEHDHK  DS    CL1                 FLAG TO USE HEADER                           
RECFOUND DS    CL1                 FLAG FOR CONDITION RECOUND FOUND             
AFAALANG DS    A                   A(LANGUAGE TABLE)                            
FACPCKID DS    X                   FACPACK SYSTEM ID                            
DISPLIST DS    X                   DISP INTO LISTDIR OF SELECTION               
*                                                                               
VERSELEM DS    0XL(2+3+10*5)       VERSION CONTROL ELEMENT                      
         DS    XL2                 CODE AND LENGTH                              
         DS    XL3                 DATE                                         
VERSNTRY DS    10XL5               ENTRIES                                      
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPHSE  DS    CL6                                                              
LSTLVEL  DS    CL1                                                              
         DS    CL1                                                              
LSTLANG  DS    CL3                                                              
         DS    CL2                                                              
LSTRDTE  DS    CL8                                                              
         DS    CL4                                                              
LSTDESC  DS    CL28                                                             
         DS    CL1                                                              
LSTNODE  DS    CL2                                                              
         DS    CL3                                                              
LSTCDOS  DS    CL4                                                              
         DS    CL1                                                              
LSTSPRE  DS    CL10                                                             
         SPACE 3                                                                
*                                                                               
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRTPHSE  DS    CL6                                                              
PRTLVEL  DS    CL1                                                              
         DS    CL1                                                              
PRTLANG  DS    CL3                                                              
         DS    CL3                                                              
PRTRDTE  DS    CL8                                                              
         DS    CL1                                                              
PRTDESC  DS    CL64                                                             
         DS    CL1                                                              
PRTNODE  DS    CL2                                                              
         DS    CL3                                                              
PRTPORS  DS    CL1                                                              
         DS    CL1                                                              
PRTOPTN  DS    CL13                                                             
         DS    CL1                                                              
PRTAST1  DS    CL2                                                              
PRTDATEC DS    CL8                                                              
PRTAST2  DS    CL2                                                              
         DS    CL1                                                              
PRTSPRE  DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTSFM0B   05/01/02'                                      
         END                                                                    
