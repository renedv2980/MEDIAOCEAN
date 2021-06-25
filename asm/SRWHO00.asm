*          DATA SET SRWHO00    AT LEVEL 008 AS OF 05/14/19                      
*PHASE T12800A                                                                  
*INCLUDE FAXPTAB                                                                
         TITLE '$WHOAMI - CONNECT/LINE/TERMINAL STATUS'                         
         PRINT NOGEN                                                            
WHOAMI   CSECT                                                                  
         NMOD1 WORKL,**$WHO**,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   IPARMS,0(R1)        SAVE COPY OF INPUT PARAMETERS                
*                                                                               
         L     R9,ATWA                                                          
         USING SRWHOFFD,R9         R9=A(TWA)                                    
         L     R8,AMYUTL                                                        
         USING UTLD,R8             R8=A(LOCAL UTL ENTRY)                        
         L     R6,TUTLXADR                                                      
         USING XAUTLD,R6           R6=A(LOCAL XA UTL ENTRY)                     
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7         R7=A(COMFACS)                                
         BRAS  RE,INIT                                                          
         SAM31                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE TERMINAL ID AND DISPLAY CONNECT DATA                       *         
***********************************************************************         
WHOM01   MVI   VIRTUAL,C'N'        VIRTUAL TERMINALS ARE SPECIAL                
         TM    TVIFLAG,TVIVIRT                                                  
         BZ    *+8                                                              
         MVI   VIRTUAL,C'Y'                                                     
         MVI   INQUIRE,C'N'                                                     
         TM    TSTAT1,TSTATDDS     DDS CAN ASK FOR A TERMINAL BY #              
         BZ    WHOM02                                                           
         OI    FLAG,X'80'          SET THIS IS A DDS TERMINAL                   
*                                                                               
         CLC   TESTWORD,SRVP2      SPECIAL DDS STUFF                            
         BNE   *+8                                                              
         OI    FLAG,X'01'                                                       
         CLI   SRVP1H+5,0          ANY INPUT IN P1                              
         BE    WHOM02                                                           
*                                                                               
WHOM01B  CLC   AUTOSWAP,SRVP1      ASKED TO CHANGE AUTOSWAP STATUS?             
         BNE   WHOM01C             NO                                           
         XI    TSTAT8,TST8ASWP     TOGGLE AUTOSWAP                              
         B     WHOM01X                                                          
*                                                                               
WHOM01C  CLC   SRVP1(3),=C'TK='    ASKED TO ADD/CHANGE TICKET NUMBER            
         BNE   *+12                                                             
         LA    RE,3                                                             
         B     WHOM01D                                                          
*                                                                               
         CLC   SRVP1(7),=C'TICKET='                                             
         BNE   WHOM01DA                                                         
         LA    RE,7                                                             
WHOM01D  LA    R1,ERROR1           VALIDATE TICKET NUMBER                       
         OC    TUSER,TUSER                                                      
         BZ    ERR                                                              
         LLC   RF,SRVP1H+5                                                      
         SR    RF,RE               RF=L'TICKET NUMBER                           
         CHI   RF,1                                                             
         BL    ERR                                                              
         CHI   RF,16                                                            
         BH    ERR                                                              
         LA    RE,SRVP1(RE)        POINT TO TICKET NUMBER                       
         MVC   WORK,SPACES                                                      
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         MVC   TTICKETN,WORK       SET TICKET NUMBER IN UTL                     
         J     WHOM01DX                                                         
*                                                                               
WHOM01DA CLC   SRVP1(6),=C'TICKET' KEYWORD FOR P2 TICKET                        
         JE    WHOM01DB                                                         
         CLC   SRVP1(3),=C'TKT'                                                 
         JNE   WHOM01E                                                          
*                                                                               
WHOM01DB LA    R1,ERROR1           VALIDATE TICKET NUMBER                       
         OC    TUSER,TUSER                                                      
         BZ    ERR                                                              
         CLI   SRVP2H+5,1                                                       
         JL    ERR                                                              
         CLI   SRVP2H+5,16                                                      
         JH    ERR                                                              
         MVC   WORK,SPACES                                                      
         LLC   RF,SRVP2H+5                                                      
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SRVP2                                                    
         MVC   TTICKETN,WORK       SET TICKET NUMBER IN UTL                     
*                                                                               
WHOM01DX TM    TDDSBITS,TDDSNOUP   IF NO-UPDATE DUE TO NO-TICKET                
         JZ    WHOM01X                                                          
         NI    TTEST,255-TTESTNOU  RE-ENABLE UPDATES                            
         NI    TDDSBITS,255-TDDSNOUP                                            
         NI    TSTATB,255-TSTATROM                                              
         J     WHOM01X                                                          
*                                                                               
WHOM01E  B     WHOM04              NOW TRY TERMINAL NUMBER                      
*                                                                               
WHOM01X  B     WHOM16              HAVE PROCESSED SPECIAL P1 VALUE              
*                                                                               
WHOM02   GOTO1 CTERMVAL,TPL,(X'30',0),(R8),TO24=Y                               
         SAM31                                                                  
         B     WHOM06                                                           
*                                                                               
WHOM04   MVI   INQUIRE,C'Y'        DDS CAN ASK FOR SPECIFIC TERMINAL            
         GOTO1 CTERMVAL,TPL,(X'30',SRVP1H),0,TO24=Y                             
         SAM31                                                                  
         TM    TPL,X'40'                                                        
         BZ    WHOM06                                                           
         LA    R1,ERROR1           INVALID INPUT FORMAT                         
         B     ERR                                                              
*                                                                               
WHOM06   TM    TPL,X'20'           TERMINAL RECORD FOUND?                       
         BZ    WHOM08              NO                                           
         CLI   VIRTUAL,C'Y'        VIRTUAL TERMS DONT HAVE RECORDS              
         BE    WHOM08                                                           
         LA    R1,ERROR2           TERMINAL RECORD NOT FOUND                    
         B     ERR                                                              
*                                                                               
WHOM08   ICM   R8,15,TPLAUTL       R8=A(INPUT TERM NUM UTL ENTRY)               
         BNZ   WHOM10                                                           
         LA    R8,DUMUTL           OR DUMMY TERMINAL                            
         XR    RE,RE                                                            
         ICM   RE,7,TPL+1          POINT TO RETURNED TERMINAL ID                
         MVC   TSYM,0(RE)                                                       
         CLI   8(RE),C' '          USE VTAM LUID IF PRESENT                     
         BNH   *+10                                                             
         MVC   TSYM,8(RE)                                                       
*                                                                               
WHOM10   TM    TPL,X'20'           TERMINAL RECORD FOUND?                       
         BZ    WHOM12                                                           
         XC    TPLAREC,TPLAREC                                                  
         XC    TPLAREL,TPLAREL                                                  
         B     WHOM14                                                           
*                                                                               
WHOM12   LA    RE,TERMREC          COPY TERMINAL RECORD LOCALLY                 
         LHI   RF,2000                                                          
         XR    R0,R0                                                            
         ICM   R0,7,TPLAREC+1                                                   
         JZ    *+2                                                              
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,TERMREC                                                       
         XR    R0,R0                                                            
         ICM   R0,7,TPLAREC+1                                                   
         SR    RF,RF                                                            
         ICM   RF,7,TPLAREL+1      RF=A(TRM DEFN ELEMENT)                       
         BZ    *+8                                                              
         SR    RF,R0                                                            
         AR    RF,RE                                                            
         STM   RE,RF,TPLAREC                                                    
*                                                                               
WHOM14   OC    TNUM,TNUM           TEST IF TERMINAL IN UTL                      
         BNZ   WHOM16                                                           
         MVC   SRVCID+12(8),=CL8'NOT YET '                                      
         MVC   SRVCSS+12(8),=CL8'LOGGED  '                                      
         MVC   SRVCPG+12(8),=CL8'ON TO   '                                      
         MVC   SRVLNG+12(8),=CL8'VTAM    '                                      
         XC    DMCB(16),DMCB                                                    
         ST    R8,DMCB+4                                                        
         LA    RE,TERMREC                                                       
         ST    RE,DMCB+8                                                        
         GOTO1 ATERMBLD,DMCB,TO24=Y BUILD DUMMY UTL ENTRY                       
         SAM31                                                                  
         B     WHOM54                                                           
*                                                                               
WHOM16   ST    R8,AMYUTL           SAVE A(UTL)                                  
         OC    TPRNT,TPRNT         TEST FOR PRINTER                             
         BNZ   WHOM54                                                           
         OC    TUSER,TUSER         CONNECTED TO USER ID                         
         BNZ   WHOM16A             NO                                           
         XC    SRVPID,SRVPID       CLEAR PERSON DATA                            
         XC    SRVPSWD,SRVPSWD                                                  
         B     WHOM22                                                           
WHOM16A  SR    R0,R0               USER ID NUMBER IF CANT FIND IT               
         ICM   R0,3,TUSER                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVCID+12(6),DUB                                                 
*                                                                               
WHOM16B  LA    R2,IO               READ CONNECTED USER ID                       
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TUSER                                                    
         BRAS  RE,READ                                                          
         BNZ   WHOM17                                                           
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
         XR    RF,RF                                                            
WHOM16C  CLI   CTDSCEL,0           FIND ALPHA-ID ELEMENT                        
         BE    WHOM17                                                           
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R2,RF,WHOM16C                                                    
         MVC   SRVCID+12(10),CTDSC                                              
*                                                                               
WHOM17   CLI   TSYS,0              TEST IF CONNECTED TO A SYSTEM                
         BE    WHOM22                                                           
         GOTO1 CGETFACT,DMCB,0,TO24=Y                                           
         SAM31                                                                  
*                                                                               
         L     R5,0(R1)                                                         
         L     R5,FASYSLST-FACTSD(R5)                                           
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING SYSLSTD,R5          R5=A(SYSTEM LIST)                            
WHOM17A  CLC   TOVSYS,SYSLNUM                                                   
         BE    *+10                                                             
         BXLE  R5,RE,WHOM17A                                                    
         DC    H'0'                                                             
         MVC   SRVCSS+12(7),SYSLNAME                                            
*                                                                               
         L     R5,ASELIST          PICK UP SELIST ENTRY                         
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING SELISTD,R5                                                       
         CLC   TSYS,SESYS                                                       
         BE    *+10                                                             
         BXLE  R5,RE,*-10                                                       
         DC    H'0'                                                             
         ST    R5,ASYS             SAVE A(SELIST ENTRY)                         
*                                                                               
WHOM18   L     R5,SEPGMS           PROGRAM                                      
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING PGMLSTD,R5                                                       
WHOM18A  CLC   PGMNUM,TPRG                                                      
         BNE   *+14                                                             
         MVC   SRVCPG+12(7),PGMNAME                                             
         B     WHOM20                                                           
         BXLE  R5,RE,WHOM18A                                                    
         MVC   SRVCPG+12(4),=C'Pgm='                                            
         MVC   HALF,TPRG                                                        
         GOTO1 CHEXOUT,DMCB,HALF,SRVCPG+16,1,(24,0)                             
         SAM31                                                                  
*                                                                               
WHOM20   SR    R0,R0               TEST IF PERSON ID KNOWN                      
         ICM   R0,3,TPERSON                                                     
         BNZ   WHOM20A                                                          
         ICM   R0,3,TPASSWD                                                     
         BNZ   WHOM20A                                                          
         XC    SRVPID,SRVPID       CLEAR PERSON DATA                            
         XC    SRVPSWD,SRVPSWD                                                  
         B     WHOM22                                                           
WHOM20A  CVD   R0,DUB              PREFILL WITH PERSON/PASSWORD NUMBER          
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVPID+12(6),DUB                                                 
         LA    R2,IO               READ CONNECTED PERSON ID                     
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,TAGYSEC                                                  
         OC    TAGYPER,TAGYPER                                                  
         BZ    *+10                                                             
         MVC   SA0KAGY,TAGYPER                                                  
         STCM  R0,3,SA0KNUM                                                     
         BRAS  RE,READ                                                          
         BNZ   WHOM21                                                           
*                                                                               
WHOM20B  LA    R2,SA0DATA          FIND PERSONAL ID ELEMENT                     
         USING SAPALD,R2                                                        
         XR    RF,RF                                                            
WHOM20C  CLI   SAPALEL,0                                                        
         BE    WHOM21                                                           
         CLI   SAPALEL,SAPASELQ                                                 
         BE    WHOM20F                                                          
         CLI   SAPALEL,SAPALELQ                                                 
         BE    WHOM20E                                                          
WHOM20D  IC    RF,SAPALLN                                                       
         BXH   R2,RF,WHOM20C                                                    
WHOM20E  XC    SRVPID+12(8),SRVPID+12                                           
         IC    RF,SAPALLN                                                       
         AHI   RF,-3                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRVPID+12(0),SAPALPID                                            
         B     WHOM21                                                           
WHOM20F  TM    FLAG,X'01'          SPECIAL TEST                                 
         BZ    WHOM20D                                                          
         MVC   SRVTE6(12),=CL12'Password'                                       
         MVC   SRVTE6+12(10),2(R2)                                              
         B     WHOM20D                                                          
*                                                                               
WHOM21   CLI   TPASSEXP,0          TEST IF PASSWORD EXPIRATION DEFINED          
         BNE   WHOM21A                                                          
         XC    SRVPSWD,SRVPSWD     CLEAR EXPIRY DAYS FIELD                      
         B     WHOM22                                                           
WHOM21A  LLC   R0,TPASSEXP                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVPSWD+12(3),DUB                                                
         TM    TSTAT7,TST7PSWN     TEST IF WITHIN WARNING PERIOD                
         BZ    *+10                                                             
         MVC   SRVPSWD+15(2),=C',W'                                             
*                                                                               
WHOM22   L     R5,ASSB             COUNTRY                                      
         L     R5,SSBACTRY-SSBD(R5)                                             
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING CTRYTABD,R5                                                      
         CLC   CTRYCODE,TCTRY                                                   
         BE    *+10                                                             
         BXLE  R5,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   SRVCTY+12(11),CTRYNAMN                                           
         LA    RE,SRVCTY+12                                                     
         LA    RF,SRVCTY+12+11                                                  
WHOM22A  CLI   0(RE),0                                                          
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         LA    RE,1(RE)                                                         
         NI    0(RE),X'BF'                                                      
         CR    RE,RF                                                            
         BL    WHOM22A                                                          
         CLI   SRVCTY+12,C'U'                                                   
         BNE   *+10                                                             
         OC    SRVCTY+12(3),=CL3' '                                             
*                                                                               
WHOM23   L     R5,ASSB             LANGUAGE                                     
         L     R5,SSBALANG-SSBD(R5)                                             
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING LANGTABD,R5                                                      
         CLC   LANGCODE,TLANG                                                   
         BE    *+10                                                             
         BXLE  R5,R4,*-10                                                       
         DC    H'0'                                                             
         MVC   SRVLNG+12(12),LANGFULN                                           
         NC    SRVLNG+13(11),LOWER                                              
         DROP  R5                                                               
*                                                                               
WHOM24   L     RE,ASSB             FACPAK SYSTEM ID                             
         USING SSBD,RE                                                          
         LA    R2,SRVFAC                                                        
         MVC   0(8,R2),=C'Facpak#N'                                             
         EDIT  SSBSYSID,(2,7(R2)),ALIGN=LEFT                                    
         TM    FLAG,X'80'          IF NOT DDS DON'T DISPLAY FACPAK#             
         BNZ   *+10                                                             
         MVC   0(9,R2),=C'Appl-id  '                                            
         MVC   12(8,R2),SSBVTID                                                 
         MVC   SRVCO7,0(R2)        SET FACPAK ID AGAIN IN OLD POSITION          
         OC    TUSER,TUSER                                                      
         BZ    WHOM24X                                                          
         TM    TSTATB,TSTATWRF                                                  
         BZ    WHOM24X                                                          
         MVC   16(6,R2),=CL6' '                                                 
         L     RE,ASSB                                                          
         L     RE,SSBAFID-SSBD(RE) A(FACIDTAB)                                  
         LLC   RF,TUPDFAC          GET UPDATIVE FACPAK ID NUM FOR SYS           
         SLL   RF,3                                                             
         AR    RE,RF                                                            
         MVI   16(R2),C'('                                                      
         MVC   17(4,R2),0(RE)      UPDATIVE FACPAK ID NAME                      
         MVI   21(R2),C')'                                                      
WHOM24X  AHI   R2,NEXTLINE                                                      
         DROP  RE                                                               
*                                                                               
WHOM25   LA    R2,SRVCO8           CURRENT SESSION                              
         MVC   0(8,R2),=C'Session '                                             
         LLC   R1,TSESSION                                                      
         AHI   R1,1                                                             
         STC   R1,12(R2)                                                        
         OI    12(R2),X'C0'                                                     
         MVI   13(R2),C' '                                                      
         L     RE,ASSB             EXTRACT MAX SESSION INFO FROM SSB            
         MVC   FULL+0(2),SSBSSMAX-SSBD(RE)                                      
         MVC   FULL+2(2),SSBSSMXP-SSBD(RE)                                      
         TM    TSTATC,TSTCXSES     TEST IF TRM SUPPORTS EXTRA SESSIONS          
         BO    WHOM25A                                                          
         LHI   R0,4                OLD CMV SUPPORTS 4 SESSIONS                  
         LHI   R1,4                                                             
         CLC   FULL+2(2),FULL+0                                                 
         BNH   *+8                                                              
         LHI   R1,5                PLUS EXTRA HIDDEN SESSION                    
         STH   R0,FULL                                                          
         STH   R1,FULL+2                                                        
WHOM25A  LH    R0,FULL                                                          
         STC   R0,14(R2)                                                        
         OI    14(R2),C'0'         MAXIMUM LOGICAL SESSIONS                     
         MVI   15(R2),C' '                                                      
         CLC   FULL+2(2),FULL+0                                                 
         BNH   *+8                                                              
         MVI   15(R2),C'+'         EXTRA PHYSICAL SESSIONS                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,1,TSSBITS        ACTIVE SESSION BITS                          
         LA    R5,16(R2)                                                        
         LA    R1,1                                                             
WHOM25B  SRDL  RE,1                MOVE OUT NEXT SESSION ACTIVE BIT             
         MVI   0(R5),C'*'                                                       
         LTR   RF,RF                                                            
         BZ    WHOM25C                                                          
         STC   R1,0(R5)                                                         
         OI    0(R5),X'C0'                                                      
WHOM25C  LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         SR    RF,RF                                                            
         BCT   R0,WHOM25B                                                       
WHOM25X  LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM26   MVC   0(13,R2),=C'AutoSwap    N'                                       
         TM    TSTAT8,TST8ASWP                                                  
         BZ    *+8                                                              
         MVI   12(R2),C'Y'                                                      
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM27   TM    FLAG,X'80'          IF CALLING TRM=DDS DO SPECIALS               
         BZ    WHOM52                                                           
         OC    TUSER,TUSER                                                      
         BZ    WHOM28                                                           
         MVC   0(9,R2),=C'Id number'                                            
         EDIT  (B2,TUSER),(5,12(R2)),ALIGN=LEFT                                 
         LR    R5,R0               BUMP TO POSTION AFTER USERID NUM             
         AR    R5,R2                                                            
         LA    R5,12(R5)                                                        
         MVI   0(R5),C'/'                                                       
         MVC   1(2,R5),TAGY                                                     
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'/'                                                       
         MVC   HALF,TAGYB                                                       
         GOTO1 CHEXOUT,DMCB,HALF,1(R5),1,(24,0)                                 
         SAM31                                                                  
         LA    R2,NEXTLINE(R2)                                                  
*                                  SE NUMBER                                    
WHOM28   CLI   TSYS,0                                                           
         BE    WHOM29                                                           
         MVC   0(9,R2),=C'SE number'                                            
         EDIT  (B1,TSYS),(3,12(R2)),ALIGN=LEFT                                  
         L     RF,ASYS                                                          
         LR    R1,R0               BUMP TO POSTION AFTER SE NUMBER              
         AR    R1,R2                                                            
         MVI   12(R1),C'/'                                                      
         MVC   12+1(7,R1),SENAME-SELISTD(RF)                                    
         LA    R2,NEXTLINE(R2)                                                  
*                                  TEST LEVEL                                   
WHOM29   TM    TTEST,TTESTCIL+TTESTLVL                                          
         BZ    WHOM30                                                           
         MVC   0(10,R2),=C'Test level'                                          
         MVC   DUB(1),TTEST                                                     
         NI    DUB,TTESTLVL                                                     
         OI    DUB,X'C0'                                                        
         LA    R5,14(R2)                                                        
         MVC   12(1,R2),DUB                                                     
         CLI   DUB,X'C0'                                                        
         BNE   *+14                                                             
         LA    R5,2(R5)                                                         
         MVC   12(3,R2),=C'Yes'                                                 
         TM    TTEST,TTESTCIL                                                   
         BZ    *+10                                                             
         MVC   0(8,R5),=C',cil=yes'                                             
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM30   CLI   TSYS,0              PRGM AUTH                                    
         BE    WHOM31                                                           
         MVC   0(9,R2),=C'Prgm auth'                                            
         MVC   HALF,TAUTH                                                       
         GOTO1 CHEXOUT,DMCB,HALF,12(R2),2,(24,0)                                
         SAM31                                                                  
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM31   TM    TTEST,TTESTTAC      TEST ID                                      
         BZ    WHOM32                                                           
         MVC   DUB(4),TACCS                                                     
         L     R0,DUB                                                           
         BRAS  RE,HEXADD                                                        
         MVC   0(11,R2),=C'A(Tst ntry)'                                         
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
         L     R5,DUB                                                           
         USING TSTTABD,R5                                                       
         MVC   0(7,R2),=C'Test Id'                                              
         MVC   12(4,R2),TSTACCS                                                 
         LA    R2,NEXTLINE(R2)                                                  
         B     WHOM44                                                           
*                                                                               
WHOM32   CLI   TSYS,0              LIMIT ACCESS                                 
         BE    WHOM44                                                           
         OC    TACCS,TACCS                                                      
         BZ    WHOM38                                                           
         MVC   0(11,R2),=C'Limit Acc  '                                         
         MVC   12(4,R2),TACCS                                                   
         TM    FLAG,X'01'          SHOW FIELD IN HEX                            
         BZ    WHOM32A                                                          
         MVC   FULL,TACCS                                                       
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
         B     WHOM37                                                           
WHOM32A  EQU   *                                                                
*&&UK                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    WHOM37                                                           
         USING SELISTD,R5                                                       
         CLI   SENAME,C'M'         TEST MEDIA SYSTEM                            
         BE    WHOM36                                                           
         CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WHOM37                                                           
         MVC   HALF,TACCS+1                                                     
         GOTO1 CHEXOUT,DMCB,HALF,13(R2),2,(24,0)                                
         SAM31                                                                  
         MVC   17(1,R2),TACCS+3                                                 
         B     WHOM37                                                           
*&&                                                                             
*&&US                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    WHOM37                                                           
         USING SELISTD,R5                                                       
*                                                                               
         CLI   SENAME,C'S'         TEST SPOT                                    
         BE    WHOM33                                                           
         CLI   SENAME,C'N'         TEST NETWORK                                 
         BE    WHOM33                                                           
         CLI   SENAME,C'P'         TEST PRINT                                   
         BNE   WHOM35                                                           
WHOM33   BRAS  RE,DISLACC                                                       
         B     WHOM37                                                           
*                                                                               
WHOM35   CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WHOM37                                                           
         CLI   TACCS,C'T'          TALENT ACCESS CODES                          
         BNE   WHOM37                                                           
         MVC   HALF,TACCS+1                                                     
         GOTO1 CHEXOUT,DMCB,HALF,13(R2),2,(24,0)                                
         SAM31                                                                  
         MVC   17(1,R2),TACCS+3                                                 
         B     WHOM37                                                           
*&&                                                                             
WHOM36   MVC   FULL,TACCS                                                       
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
*                                                                               
WHOM37   LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM38   OC    TACCS2,TACCS2       LIMIT ACCESS 2 FIELD                         
         BZ    WHOM44                                                           
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
         MVC   0(11,R2),=C'Limit Acc#2'                                         
         MVC   12(4,R2),TACCS2                                                  
         TM    FLAG,X'01'          SHOW FIELD IN HEX                            
         BZ    WHOM38A                                                          
         MVC   FULL,TACCS2                                                      
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
         B     WHOM44                                                           
WHOM38A  EQU   *                                                                
*&&UK                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    WHOM42                                                           
         USING SELISTD,R5                                                       
         CLI   SENAME,C'M'         TEST MEDIA SYSTEM                            
         BE    WHOM40                                                           
         CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WHOM42                                                           
         MVC   HALF,TACCS2+1                                                    
         GOTO1 CHEXOUT,DMCB,HALF,13(R2),2,(24,0)                                
         SAM31                                                                  
         MVC   17(1,R2),TACCS2+3                                                
         B     WHOM42                                                           
*&&                                                                             
*&&US                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    WHOM42                                                           
         USING SELISTD,R5                                                       
         CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WHOM39                                                           
         CLI   TACCS2,C'T'         TALENT ACCESS CODES                          
         BNE   WHOM39                                                           
         MVC   HALF,TACCS2+1                                                    
         GOTO1 CHEXOUT,DMCB,HALF,13(R2),2,(24,0)                                
         SAM31                                                                  
         MVC   17(1,R2),TACCS2+3                                                
         B     WHOM42                                                           
                                                                                
WHOM39   CLI   SENAME,C'S'         TEST SPOT                                    
         BE    WHOM39A                                                          
         CLI   SENAME,C'N'         TEST NETWORK                                 
         BE    WHOM39A                                                          
         CLI   SENAME,C'P'         TEST PRINT                                   
         BNE   WHOM42                                                           
WHOM39A  BRAS  RE,DISLACC                                                       
         B     WHOM42                                                           
*&&                                                                             
WHOM40   MVC   FULL,TACCS2                                                      
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
*                                                                               
WHOM42   LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM44   TM    TTEST,TTESTNOU      UPDATE                                       
         BZ    WHOM48                                                           
         MVC   0(6,R2),=C'Update'                                               
         MVC   12(2,R2),=C'No'                                                  
         TM    TTEST,TTESTTAC      TEST IF TACCS IS A(TSTTAB)                   
         BZ    WHOM46                                                           
         L     R5,DUB                                                           
         USING TSTTABD,R5                                                       
         OC    TSTLOW,TSTLOW                                                    
         BZ    *+10                                                             
         MVC   14(4,R2),=C',log'                                                
WHOM46   LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM48   OC    TPASSWD,TPASSWD     PASS NAME                                    
         BZ    WHOM52                                                           
         TM    TFLAG,TFLAGSEC      DON'T DISPLAY SECRET CODES                   
         BO    WHOM52                                                           
*                                                                               
         LA    R2,IO                                                            
         USING CTTREC,R2                                                        
         XC    CTTREC,CTTREC                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKEY+23(2),TPASSWD                                             
         BRAS  RE,READ                                                          
         BNE   WHOM52                                                           
*                                                                               
         LA    R2,CTTDATA                                                       
         SR    RF,RF                                                            
WHOM50   CLI   0(R2),0             LOOK FOR PASSIVE ELEMENT                     
         JE    *+2                                                              
         CLI   0(R2),X'03'                                                      
         BE    *+12                                                             
         IC    RF,1(R4)                                                         
         BXH   R2,RF,WHOM50                                                     
         MVC   0(9,R2),=C'Pass name'                                            
         MVC   12(10,R2),10(R4)                                                 
         LA    R2,NEXTLINE(R2)                                                  
         DROP  R2                                                               
*                                                                               
WHOM52   L     R5,ASSB             BULK UPDATE MODE                             
         USING SSBD,R5                                                          
         TM    SSBSTAT5,SSB5BULK   TEST IF BULK UPLOAD MODE                     
         BZ    WHOM54                                                           
         LA    R2,SRVFLNK          DO NOT MOVE - CMV LOOKS AT THIS              
         LLH   RF,SSBTMSL          SAVE OF TEMPEST/TEMPSTR PAGE                 
         SRL   RF,10               DIVIDE BY 1024                               
         MVI   12(R2),C'B'         BULK ON                                      
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  13(2,R2),DUB                                                     
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TERMINAL DATA                                                       *         
***********************************************************************         
WHOM54   LA    R2,SRVTNO+12        TERM NUMBER AND ADDRESS                      
         EDIT  (B2,TNUM),(4,0(R2)),ALIGN=LEFT                                   
         MVC   SRVLUID+12(8),TLUID                                              
*                                                                               
         MVCDD SRVTTY+12(10),SR#PRR                                             
         OC    TPRNT,TPRNT                                                      
         BZ    WHOM56                                                           
         TM    TTYPE,TTYPERMC                                                   
         BZ    WHOM58                                                           
         MVCDD SRVTTY+12(10),SR#SHTL                                            
         B     WHOM58                                                           
*                                                                               
WHOM56   MVC   SRVTTY+12(10),=CL10'3270'                                        
         TM    TTYPE,TTYPE327                                                   
         BO    WHOM58                                                           
         MVC   SRVTTY+12(10),=CL10'Virtual'                                     
         CLI   VIRTUAL,C'Y'                                                     
         BE    WHOM58                                                           
         MVC   SRVTTY+12(10),=CL10'ICC'                                         
         TM    TTYPE,TTYPEICC                                                   
         BO    WHOM58                                                           
         MVC   SRVTTY+12(10),=CL10'TWX'                                         
         TM    TTYPE,TTYPETWX                                                   
         BO    WHOM58                                                           
         MVC   SRVTTY+12(10),=CL10'RJE'                                         
         TM    TTYPE,TTYPE378                                                   
         BO    WHOM58                                                           
         MVC   SRVTTY+12(10),=CL10'Courier'                                     
*                                                                               
WHOM58   TM    FLAG,X'80'          EXTRA INFO FOR DDS TERMINALS                 
         BZ    WHOM64                                                           
         OC    TNUM,TNUM           UNLESS NOT IN UTL                            
         BZ    WHOM64                                                           
         LA    R2,SRVTE8                                                        
*                                  OFFICE CODE                                  
         MVC   0(11,R2),=C'Office code'                                         
         MVC   12(1,R2),TOFFCODE                                                
         LA    R2,NEXTLINE(R2)                                                  
*                                  A(UTL)                                       
         LR    R0,R8                                                            
         BAS   RE,HEXADD                                                        
         MVC   0(6,R2),=C'A(utl)'                                               
*                                                                               
         CLI   INQUIRE,C'Y'        TEST FOR VTAM INQUIRE                        
         BE    WHOM62                                                           
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
         ICM   R0,15,TBUFF         A(TBUFF)                                     
         BZ    WHOM60                                                           
         BAS   RE,HEXADD                                                        
         MVC   0(8,R2),=C'A(tbuff)'                                             
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
WHOM60   L     R5,ATCB             A(TCB ENTRY)                                 
         LH    RE,0(R5)                                                         
         ICM   RF,15,2(R5)                                                      
         AHI   R5,6                                                             
         USING TCBD,R5                                                          
         C     R8,TCBUTL                                                        
         BE    *+12                                                             
         BXLE  R5,RE,*-8                                                        
         B     WHOM64                                                           
*                                                                               
         LR    R0,R5                                                            
         BAS   RE,HEXADD                                                        
         MVC   0(11,R2),=C'A(tcb ntry)'                                         
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
         L     R0,TCBTWA           A(TWA)                                       
         BAS   RE,HEXADD                                                        
         MVC   0(6,R2),=C'A(twa)'                                               
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
         L     R0,TCBMAP           A(PHASE MAP)                                 
         BAS   RE,HEXADD                                                        
         MVC   0(11,R2),=C'A(phse map)'                                         
         LA    R2,NEXTLINE(R2)                                                  
*                                                                               
         L     R0,TCBPGMA          A(PROGRAM)                                   
         BAS   RE,HEXADD                                                        
         MVC   0(10,R2),=C'A(program)'                                          
         LA    R2,NEXTLINE(R2)                                                  
         B     WHOM64                                                           
         EJECT                                                                  
***********************************************************************         
* VTAM INQUIRE                                                        *         
***********************************************************************         
WHOM62   CLI   VIRTUAL,C'Y'                                                     
         BE    WHOM64                                                           
*                                                                               
         LA    R2,32(R2)                                                        
         GOTO1 ALCM,DMCB,VTGETCID,(R8),0,TO24=Y                                 
         SAM31                                                                  
*                                                                               
         MVC   DUB,8(R1)                                                        
         MVC   0(10,R2),=C'Inquire fb'                                          
         GOTO1 CHEXOUT,DMCB,DUB+4,12(R2),2,(24,0)                               
         SAM31                                                                  
         LA    R2,64(R2)                                                        
*                                                                               
         MVC   0(10,R2),=C'Utl cid   '                                          
         MVC   FULL,TCID                                                        
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
         LA    R2,32(R2)                                                        
*                                                                               
         MVC   0(10,R2),=C'Vtam cid  '                                          
         GOTO1 CHEXOUT,DMCB,DUB,12(R2),4,(24,0)                                 
         SAM31                                                                  
         LA    R2,64(R2)                                                        
*                                                                               
         MVC   0(10,R2),=C'TSTAT3/4/5'                                          
         MVC   FULL(3),TSTAT3                                                   
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),3,(24,0)                                
         SAM31                                                                  
         LA    R2,NEXTLINE(R2)                                                  
         EJECT                                                                  
***********************************************************************         
* LINE DATA                                                           *         
***********************************************************************         
WHOM64   ICM   R5,15,TPLAREL       VTAM - LINE NODE/ID/CU/DV                    
         BZ    WHOM70                                                           
         USING CTTRMD,R5           R5=A(TERMINAL DEFN ELEMENT)                  
*                                                                               
         LLC   RF,CTTRMNDE                                                      
         MHI   RF,NODETBLL                                                      
         LA    RF,NODETBL(RF)                                                   
         LA    RE,NODETBLX                                                      
         CR    RF,RE                                                            
         BNL   *+10                                                             
         MVC   SRVNODE+12(3),0(RF)                                              
         MVC   SRVLID+12(4),CTTRMLNE                                            
*                                                                               
         CLI   CTTRMCU,0           DISPLAY HEX CONTROL UNIT                     
         BE    WHOM66                                                           
         GOTO1 CHEXOUT,DMCB,CTTRMCU,SRVCU+12,1,(24,0)                           
         SAM31                                                                  
*                                                                               
WHOM66   CLI   CTTRMDV,0           DISPLAY HEX DEVICE ADDRESS                   
         BE    WHOM68                                                           
         GOTO1 CHEXOUT,DMCB,CTTRMDV,SRVDV+12,1,(24,0)                           
         SAM31                                                                  
*                                                                               
WHOM68   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* SOX CONNECT DATA                                                    *         
***********************************************************************         
WHOM70   LA    R2,SRVLI5           POINT TO FIELD TO BE USED                    
*                                                                               
WHOM72   OC    TUSER,TUSER         DATA IF NOT CONNECTED                        
         BNZ   WHOM74                                                           
         MVC   0(8,R2),=C'........'                                             
         L     RE,ASSB                                                          
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'P'          #4 PROD/TEST FACPAK                          
         TM    SSBSYSFL-SSBD(RE),X'80'                                          
         BZ    *+8                                                              
         MVI   0(R2),C'T'                                                       
         LA    R2,2(R2)                                                         
         MVI   0(R2),C'C'          #6 CLIENT/DDS TERMINAL                       
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'D'                                                       
         B     WHOM80                                                           
*                                                                               
WHOM74   MVI   0(R2),C'U'          #1 UPDATIVE/READONLY SYSTEM                  
         TM    TSTATB,TSTATROS                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'U'          #2 UPDATIVE/READONLY MODE                    
         TM    TSTATB,TSTATROM                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'.'          #3 OK/WRONG FACPAK                           
         TM    TSTATB,TSTATWRF                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'W'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'P'          #4 PROD/TEST FACPAK                          
         TM    TSTATB,TSTATTSF                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'T'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'C'          #5 CLIENT/DDS PERSON                         
         TM    TSTATB,TSTATDPE                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'D'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'C'          #6 CLIENT/DDS TERMINAL                       
         TM    TSTATB,TSTATDTE                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'D'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'.'          #7 PPS AGENCY                                
         TM    TSTATB,TSTATPPS                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'P'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'.'          #8 DDS PERSON OVERRIDE                       
         TM    TSTATB,TSTATDPO                                                  
         BZ    *+8                                                              
         MVI   0(R2),C'D'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),C'U'          CONNECTED WITH U=N                           
         TM    TTEST,TTESTNOU                                                   
         BZ    *+8                                                              
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(2,R2),TAGY        AGENCY ALPHA ID                              
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),TAGYSEC     AGENCY ALPHA ID FOR SECURITY                 
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),TAGYPER     AGENCY ALPHA ID FOR PERSON ID                
         LA    R2,2(R2)                                                         
         OC    TPERSON,TPERSON                                                  
         BZ    *+14                                                             
         MVC   HALF,TPERSON                                                     
         B     WHOM78                                                           
         OC    TPASSWD,TPASSWD                                                  
         BZ    WHOM80                                                           
         MVC   HALF,TPASSWD                                                     
WHOM78   GOTO1 CHEXOUT,DMCB,HALF,0(R2),2,(24,0)                                 
         SAM31                                                                  
*                                                                               
WHOM80   TM    TSTAT1,TSTATDDS     SHOW TICKET NUMBER                           
         BZ    WHOM90                                                           
*                                                                               
         CLI   TTICKETN,0                                                       
         BE    WHOM90                                                           
         MVC   SRVTE7(4),=CL4'Tkt='                                             
         MVC   SRVTE7+4(16),TTICKETN                                            
         EJECT                                                                  
***********************************************************************         
* PC PROGRAM CONNECT DATA                                             *         
***********************************************************************         
WHOM90   LA    R2,SRVLI7           OUTPUT PC PROGRAM INFO IF DEFINED            
         SR    R0,R0                                                            
         ICM   R0,3,TXPNUM                                                      
         BZ    WHOM94                                                           
         TM    TXPTYPE,TXPTPC                                                   
         BZ    WHOM94                                                           
         CVD   R0,DUB              OUTPUT PC APP PROGRAM NUMBER                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R2),DUB                                                      
         L     RF,=V(FAXPTPC)      POINT TO PC PROGRAM TABLE                    
         A     RF,RELO                                                          
         USING FAXPTABD,RF                                                      
         LA    RF,L'FAXPNTRY(RF)   BUMP PAST FIRST ENTRY                        
*                                                                               
WHOM91   CLI   FAXPTYPE,0          SEARCH FOR NUMBER TO GET NAME                
         BE    WHOM94                                                           
         TM    FAXPTYPE,FAXPTPC                                                 
         BZ    WHOM92                                                           
         CLM   R0,3,FAXPNUM                                                     
         BE    WHOM93                                                           
WHOM92   LA    RF,L'FAXPNTRY(RF)                                                
         B     WHOM91                                                           
WHOM93   MVC   0(L'FAXPNEXE,R2),FAXPNEXE                                        
         DROP  RF                                                               
*                                                                               
WHOM94   OC    TXPVER,TXPVER       PC PROGRAM VERSION NUMBER                    
         BZ    EXIT                                                             
         LA    R2,SRVLI8                                                        
         CLC   TXPVER,=X'FFFFFF'   INVALID VALUE                                
         BNE   *+14                                                             
         MVC   0(7,R2),=C'?.?.?.?'                                              
         B     EXIT                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,14,TXPVER        A.B.CC.DD IN LEFT 3 BYTES                    
*                                                                               
         SR    RE,RE               A                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C'.'                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         SR    RE,RE               B                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C'.'                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         SR    RE,RE               CC                                           
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
         MVI   3(R2),C'.'                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         SR    RE,RE               DD                                           
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1                                                                   
         MVI   VTAM,C'Y'           BTAM CODE REMOVED                            
*                                                                               
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF          RF=A(SYSFACS)                                
         MVC   ASSB,VSSB                                                        
         MVC   ATERMBLD,VTERMBLD                                                
         MVC   ASELIST,VSELIST                                                  
         MVC   ATCB,VTCB                                                        
         MVC   ALCM,VLCM                                                        
         DROP  RF                                                               
         L     RE,ASSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
*                                                                               
         TM    TSTAT1,TSTATDDS     DDS TERMINAL CAN SHOW MORE                   
         BZ    EXITOK                                                           
         CLC   SRVID+7(5),=C',????'                                             
         BNE   EXITOK                                                           
         OI    FLAG,X'81'          SET DDS AND SIMULATE TESTWORD                
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY LIMIT ACCESS                                                          
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER                     
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE)           
* EXIT - APWORK+0(6)=LIMIT ACCESS CODE                                          
***********************************************************************         
DISLACC  NTR1                                                                   
*&&US                                                                           
         ICM   R5,15,ASYS          A(SELIST ENTRY)                              
         BZ    DLACX                                                            
         USING SELISTD,R5                                                       
*----------------------------------------------------------------------         
* US MEDIA LIMITED ACCESS (SPOT,NET,PRINT,TRAFFIC)                              
*----------------------------------------------------------------------         
DLAC040  CLI   TACCS,C'$'          OFFICE LIST                                  
         BE    DLACX                                                            
         CLI   TACCS,C'+'          MARKET                                       
         BE    DLACX                                                            
         CLI   TACCS,C'*'          OFFICE OR CLIENT GROUP?                      
         BNE   DLAC080             NO: THIS IS A CLIENT                         
         CLI   TACCS+1,C'*'        TWO CHARACTER OFFICE?                        
         BE    DLAC060             YES: PROCESS TWO CHARACTER OFFICE            
         CLC   TACCS+2(2),=H'0'    ZEROES MEANS ONE BYTE OFFICE                 
         BE    DLAC060                                                          
         CLC   TACCS+2(2),=CL2' '  SPACES ALSO MEAN ONE BYTE OFFICE             
         BE    DLAC060                                                          
*----------------------------------------------------------------------         
* CLIENT GROUP LIMITED ACCESS                                                   
*----------------------------------------------------------------------         
         CLI   TACCS+2,C'A'        MAKE SURE THIS IS A VALID CLT GROUP          
         BL    DLAC041             YES: CONTINUE                                
         CLI   TACCS+2,C'0'        MAKE SURE THIS IS A VALID CLT GROUP          
         BNL   DLAC041             YES: CONTINUE                                
         MVC   12(L'CTSYSLMT,R2),TACCS                                          
         MVI   12+L'CTSYSLMT+1(R2),C'?'                                         
         B     DLACX               INVALID CLIENT GROUP                         
*                                                                               
DLAC041  LA    R1,SPCGRTAB                                                      
DLAC042  CLC   2(1,R1),TACCS+1     FIND CLIENT GROUP ID IN TABLE                
         BE    DLAC044             FOUND IT                                     
         CLI   0(R1),C'Z'          C'Z' MARKS THE END OF THE TABLE              
         BNE   DLAC043             NOT THERE YET: CONTINUE                      
         MVC   12(L'CTSYSLMT,R2),TACCS                                          
         MVI   12+L'CTSYSLMT+1(R2),C'?'                                         
         B     DLACX                                                            
DLAC043  LA    R1,L'SPCGRTAB(,R1)                                               
         B     DLAC042                                                          
*                                                                               
DLAC044  MVC   12(4,R2),=C'    '                                                
         MVI   12(R2),C'*'                                                      
         MVC   13(2,R2),0(R1)      CLIENT GROUP ID ALPHA CODE                   
*                                                                               
         LA    R1,14(,R2)                                                       
         CLI   0(R1),C' '          IS THIS A SINGLE CHARACTER ID                
         BH    DLAC045             NO                                           
         TM    2(R3),X'F0'         IS THE NEXT BYTE A DIGIT?                    
         BNO   DLAC046             NO: CONTINUE TO PROCESS                      
         MVC   14(1,R2),TACCS+2    YES: DISPLAY CHARACTER                       
         B     DLACX                                                            
*                                                                               
DLAC045  LA    R1,1(,R1)                                                        
         TM    TACCS+2,X'F0'       IS THIS A DIGIT?                             
         BNO   DLAC046             YES: PROCESS                                 
         MVC   15(1,R2),TACCS+2    NO: IT'S A CHARACTER                         
         MVI   17(R2),C'?'         SOMETHING IS STRANGE                         
         B     DLACX                                                            
*                                                                               
DLAC046  IC    RF,TACCS+2                                                       
         SRL   RF,4                                                             
         STC   RF,0(R1)                                                         
         OI    0(R1),X'F0'                                                      
         LA    R1,1(,R1)                                                        
*                                                                               
         TM    TACCS+2,X'0F'                                                    
         BO    DLACX                                                            
         MVC   0(1,R1),TACCS+2                                                  
         OI    0(R1),X'F0'                                                      
         LA    R1,1(,R1)                                                        
*                                                                               
         TM    TACCS+3,X'F0'                                                    
         BO    DLACX                                                            
         IC    RF,TACCS+3                                                       
         SRL   RF,4                                                             
         STC   RF,0(R1)                                                         
         OI    0(R1),X'F0'                                                      
         LA    R1,1(,R1)                                                        
*                                                                               
         TM    TACCS+3,X'0F'                                                    
         BO    DLACX                                                            
         MVC   0(1,R1),TACCS+3                                                  
         OI    0(R1),X'F0'                                                      
         LA    R1,1(,R1)                                                        
         B     DLACX                                                            
*----------------------------------------------------------------------         
* OFFICE LIMITED ACCESS                                                         
*----------------------------------------------------------------------         
DLAC060  LA    RF,WBLOCK                                                        
         USING OFFICED,RF                                                       
         XC    WBLOCK,WBLOCK                                                    
         MVC   OFCSYS,SENAME       SYSTEM ID (S, N, P)                          
         MVC   OFCAGY,TAGY                                                      
         MVC   OFCOFC,TACCS+1      1 BYTE OFFICE                                
         DROP  RF                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'     GET OFFICER ADDRESS                   
         GOTO1 CCALLOV,DMCB,TO24=Y                                              
         SAM31                                                                  
         CLI   4(R1),X'FF'                                                      
         BE    DLAC100                                                          
*                                                                               
         L     RF,DMCB                    CALL OFFICER                          
         GOTO1 (RF),DMCB,(C'2',WBLOCK),ACOMFACS,TO24=Y                          
         SAM31                                                                  
*                                                                               
         LA    R1,WBLOCK                                                        
         USING OFFICED,R1                                                       
         TM    OFCINDS,OFCINOLA+OFCIOINV   NOT USING 2 OFFS OR INVALID          
         BNZ   DLACX                                                            
         MVI   12(R2),C'*'                                                      
         MVI   13(R2),C'*'                 (**OF)                               
         MVC   14(L'OFCOFC2,R2),OFCOFC2    DISPLAY 2 BYTE OFFICE VALUE          
         B     DLACX                                                            
         DROP  R1                                                               
*----------------------------------------------------------------------         
* CLIENT LIMITED ACCESS                                                         
*----------------------------------------------------------------------         
DLAC080  CLI   SENAME,C'P'         PRINT                                        
         BE    DLACX               NO: NEED FOR UNPACKING                       
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A15'    CLUNPK                                 
         GOTO1 CCALLOV,DMCB,TO24=Y                                              
         SAM31                                                                  
         CLI   4(R1),X'FF'                                                      
         BE    DLAC100                                                          
*                                                                               
         MVC   FULL,TACCS                                                       
         L     RF,DMCB                   CLUNPK                                 
         GOTO1 (RF),DMCB,FULL,12(R2),TO24=Y                                     
         SAM31                                                                  
         B     DLACX                                                            
*                                                                               
DLAC100  MVC   FULL,TACCS                                                       
         GOTO1 CHEXOUT,DMCB,FULL,12(R2),4,(24,0)                                
         SAM31                                                                  
*&&                                                                             
DLACX    B     EXITOK                                                           
         DROP  R5                                                               
                                                                                
***********************************************************************         
* HANDY ROUTINES                                                      *         
***********************************************************************         
READ     NTR1                                                                   
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,IO,IO,TO24=Y                         
         SAM31                                                                  
         TM    DMCB+8,X'FF'                                                     
         B     EXIT                                                             
*                                                                               
HEXADD   NTR1                                                                   
         ST    R0,DUB                                                           
         GOTO1 CHEXOUT,DMCB,DUB,12(R2),4,(24,0)                                 
         SAM31                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
ERR      XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(40),0(R1)                                                 
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP1H+6,X'40'                                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
SPACES   DC    CL16' '                                                          
AUTOSWAP DC    CL8'AUTOSWAP'                                                    
TESTWORD DC    CL8'WHATISIT'                                                    
ERROR1   DC    CL40'** ERROR ** TERMINAL INPUT NOT VALID   '                    
ERROR2   DC    CL40'** ERROR ** TERMINAL RECORD NOT FOUND  '                    
LOWER    DC    24X'BF'                                                          
DMREAD   DC    CL8'DMREAD  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* TABLES                                                                        
*   NODETAB NOW IN CTREPLUTAB                                                   
***********************************************************************         
       ++INCLUDE CTREPLUTAB                                                     
*                                                                               
       ++INCLUDE SPCGRTAB                                                       
                                                                                
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AMYUTL   DS    A                   A(MY LOCAL UTL ENTRY)                        
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
AMYSE    DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
*                                                                               
TPL      DS    F                                                                
TPLAUTL  DS    A                                                                
TPLAREC  DS    A                                                                
TPLAREL  DS    A                                                                
*                                                                               
ASSB     DS    A                                                                
ATERMBLD DS    A                                                                
ASELIST  DS    A                                                                
ATCB     DS    A                                                                
ALCM     DS    A                                                                
*                                                                               
RECLEN   DS    H                                                                
HALF     DS    H                                                                
FULL     DS    F                                                                
*                                                                               
RELO     DS    A                                                                
ASYS     DS    A                                                                
NEXTLINE EQU   96                                                               
SAVELINE DS    CL8                                                              
VTAM     DS    CL1                                                              
INQUIRE  DS    CL1                                                              
VIRTUAL  DS    CL1                                                              
FLAG     DS    CL1                                                              
DUMUTL   DS    CL256                                                            
WBLOCK   DS    CL48                       WORK BLOCK                            
*                                                                               
IO       DS    2000C                                                            
*                                                                               
TERMREC  DS    2000C                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
SRWHOFFD DSECT                                                                  
         DS    CL64                                                             
* SRWHOFFD                                                                      
       ++INCLUDE SRWHOFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER FACPAK DSECTS                                                 *         
***********************************************************************         
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* CTGENFILE/SEACSFILE                                                           
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FASYSLSTD                                                                     
       ++INCLUDE FASYSLSTD                                                      
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
* SRDDEQUS                                                                      
       ++INCLUDE SRDDEQUS                                                       
* FAXPTABD                                                                      
       ++INCLUDE FAXPTABD                                                       
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRWHO00   05/14/19'                                      
         END                                                                    
