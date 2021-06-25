*          DATA SET SPOMS18    AT LEVEL 012 AS OF 01/03/07                      
*PHASE T23418A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T23418 - E-MAIL INVITE                                                
*                                                                               
*  COMMENTS:                                                                    
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T23400), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPOMSC8 (T234C8) -- INVITE (VK AND VR)                       
*                                                                               
*  OUTPUTS: SENDS AND E-MAIL                                                    
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
T23418   TITLE 'SPOMS18 - E-MAIL INVITE'                                        
T23418   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BOXDL+WIDEDL,*T23418*,R7,RR=R3,CLEAR=YES                         
         LR    R0,RC               OUR BOX AREA (TO GET 198 CHAR WIDE)          
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
*                                                                               
         ST    R0,ABOX             OUR BOX AREA (TO GET 198 CHAR WIDE)          
         ST    R0,TWAVBOX                                                       
         LR    R1,R0                                                            
         USING BOXD,R1                                                          
         AH    R0,=Y(BOXDL)                                                     
         ST    R0,BOXAWIDE                                                      
         DROP  R1                                                               
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE    INITIALIZE PFKEYS                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
***                                                                             
* MEDIA                                                                         
***                                                                             
         LA    R2,EMLMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALIMED                                                          
***                                                                             
* BUYER                                                                         
***                                                                             
         LA    R2,EMLBYRH          VALIDATE BUYER                               
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALIBUYR,DMCB,8(R2)                                              
         BNE   ERRBYR                                                           
***                                                                             
* STATION                                                                       
***                                                                             
         LA    R2,EMLSTNH          VALIDATE STATION                             
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALISTA                                                          
***                                                                             
* CLIENT                                                                        
***                                                                             
         XC    BCLT,BCLT           CLEAR CLIENT                                 
         LA    R2,EMLCLTH                                                       
         CLI   5(R2),0             ANY INPUT FOR CLIENT                         
         BE    VK50                NO...DON'T VALIDATE                          
         GOTO1 VALICLT                                                          
***                                                                             
* BUILD KEY (WE DON'T STORE RECS FOR E-MAIL INVITE SO BUILD AGENCY REC)         
***                                                                             
VK50     XC    KEY,KEY             BUILD AGENCY RECORD                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                VR                                   *         
***********************************************************************         
VR       DS    0H                                                               
***                                                                             
* DISPLAY DESTINATION AND ROUTE                                                 
***                                                                             
         MVC   SVKEY,KEY          SAVE OFF KEY FOR RD4ROUTE & BUYER REC         
         XC    EMLDEST,EMLDEST    CLEAR DESTINATION                             
         OI    EMLDESTH+4,X'20'   JUST FOR RD4ROUTE                             
         GOTO1 RD4ROUTE,DMCB,EMLDESTH,EMLROUTH                                  
         LA    R2,EMLROUTH                                                      
         LA    R0,EMLROUT+L'EMLROUT    RIGHT-BOUND                              
         CLC   =C'FAX: ',8(R2)                                                  
         BNE   ERRINV                                                           
         LA    R2,8+5(R2)                                                       
VR01     CLI   0(R2),X'40'         FIND FIRST SPACE                             
         BNH   VR02                                                             
         LA    R2,1(R2)                                                         
         CR    R2,R0                                                            
         BL    VR01                                                             
         B     VR02Z               NOTHING AFTER FAX: ????                      
*                                                                               
VR02     CLI   0(R2),X'40'         NOW FIND FIRST NON-SPACE (FAX NUM)           
         BH    VR02A                                                            
         LA    R2,1(R2)                                                         
         CR    R2,R0                                                            
         BL    VR02                                                             
         B     VR02Z               NO FAX NUMBER                                
*                                                                               
VR02A    MVC   EMLFAX,0(R2)                                                     
         XC    0(L'EMLFAX,R2),0(R2)                                             
VR02Z    OC    EMLFAX,EMLFAX                                                    
         BZ    VR03                                                             
         OI    EMLFAXH+6,X'80'     TRANSMIT                                     
         NI    EMLFAXH+1,X'F7'     TURN OFF HIGH-INTENSITY                      
         B     VR05                                                             
*                                                                               
VR03     MVC   EMLFAX(12),=C'???-???-????'                                      
         OI    EMLFAXH+6,X'80'     TRANSMIT                                     
         OI    EMLFAXH+1,X'08'     HIGH INTENSITY                               
***                                                                             
* DISPLAY SENDER'S NAME                                                         
***                                                                             
VR05     MVC   EMLSNAM(L'QBUYER),QBUYER                                         
         OI    EMLSNAMH+6,X'80'    TRANSMIT                                     
***                                                                             
* DISPLAY SENDER'S E-MAIL                                                       
***                                                                             
VR10     LA    R4,KEY              READ BUYER REC                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ    X'0D'                                        
         MVI   BYRKSUB,BYRKSUBQ    X'31'                                        
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,EMLBYR                                                   
         OC    BYRKBYR,SPACES                                                   
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   KEY,SVKEY                                                        
*                                                                               
         LA    R2,EMLSEMLH         CLEAR E-MAIL                                 
         XC    8(L'EMLSEML,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCD2Q     BUYER DESCRIPTION2 ELEMENT CODE              
         BAS   RE,GETEL            ANY ELEMENTS?                                
         BNE   ERREML              NO                                           
*                                                                               
         USING BYRDSCD2,R6                                                      
         MVC   EMLSEML,BYREMAIL    E-MAIL                                       
         DROP  R6                                                               
***                                                                             
* VALIDATE RECEIVER'S NAME                                                      
***                                                                             
VR20     LA    R2,EMLRNAMH         RECEIVER'S NAME                              
         GOTO1 ANY                 REQUIRED                                     
***                                                                             
* VALIDATE SENDER'S E-MAIL                                                      
***                                                                             
VR30     LA    R2,EMLREMLH         RECEIVER'S E-MAIL ADDRESS                    
         GOTO1 ANY                 REQUIRED                                     
         LA    R2,EMLREML                                                       
         BAS   RE,VALEMAIL                                                      
*                                                                               
VRX      CLI   PFKEY,0             WAS A PF KEY HIT                             
         BE    *+8                                                              
         BAS   RE,CHKPF            YES CHECK IT                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                             CHKPF                                   *         
*        CHECK WHICH PF KEY WAS HIT AND RESPOND ACCORDINGLY           *         
***********************************************************************         
CHKPF    NTR1                                                                   
         CLI   PFKEY,6             TRANSMIT E-MAIL?                             
         BNE   CPX                 NO                                           
*                                                                               
         MVC   TWADEST,=X'1F9D'    ID OF DDSEMAIL                               
         BAS   RE,PR               PRINT REPORT NOW                             
         XC    TWADEST,TWADEST                                                  
*                                                                               
CPX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                             VALEMAIL                                *         
***********************************************************************         
VALEMAIL NTR1                                                                   
*                                                                               
         LA    R2,EMLREMLH                                                      
         ZIC   R1,5(R2)                                                         
         LA    R3,EMLREML                                                       
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
*                              PR                                     *         
***********************************************************************         
PR       NTR1                                                                   
         MVI   WHEN,X'40'          SET REPORT TO PRINT NOW                      
         MVI   TWAWHEN,0                                                        
*                                                                               
         LA    R1,NOTHING                                                       
         ST    R1,SPECS                                                         
*                                                                               
PR10     L     R1,TWAVBOX          TO GET 198 CHAR WIDE REPORTS                 
         ST    R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'198'                                                 
         DROP  R1                                                               
*                                                                               
         BAS   RE,OPENSPQ          OPEN PRINT QUEUE                             
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         L     RE,TWAVBOX                                                       
         USING BOXD,RE                                                          
         L     RE,BOXAWIDE                                                      
         USING WIDED,RE                                                         
         LA    R0,XP1              FILL XP1 - XSPACES WITH SPACES               
         LA    R1,L'XP1*5                                                       
         LR    R2,R0                                                            
         XR    R3,R3                                                            
         MVI   BYTE,C' '                                                        
         ICM   R3,8,BYTE                                                        
         MVCL  R0,R2                                                            
         LA    R0,XHEAD1           FILL XHEAD - XHEAD14 WITH SPACES             
         LA    R1,L'XHEAD1*14                                                   
         MVCL  R0,R2                                                            
         LA    R0,XMID1            FILL XMID1 - XMID2 WITH SPACES               
         LA    R1,L'XMID1*2                                                     
         MVCL  R0,R2                                                            
         MVC   XMID1,XSPACES                                                    
         MVC   XMID2,XSPACES                                                    
*                                                                               
         LA    R1,XP                                                            
         ST    R1,APRTLINE                                                      
         DROP  RE                                                               
*                                                                               
         MVC   4(5,R1),=C'*HDR*'   HEADER RECORD                                
         MVI   69(R1),C'M'                                                      
         BAS   RE,PRTLINE                                                       
*                                                                               
         L     R3,APRTLINE                                                      
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'SP'      SPOT                                         
         MVC   EDIPROG,=C'EML'     E-MAIL                                       
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'RCP'     RECIPIENT                                    
         LA    R2,EDICOMN                                                       
         ZIC   R4,EMLRNAMH+5                                                    
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),EMLRNAM     RECEIVER'S NAME                              
*                                                                               
         LA    R2,1(R2,R4)                                                      
         MVI   1(R2),C'<'                                                       
         ZIC   R4,EMLREMLH+5                                                    
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),EMLREML     RECEIVER'S E-MAIL                            
         LA    R2,3(R2,R4)                                                      
         MVI   0(R2),C'>'                                                       
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'RPY'     REPLY TO                                     
         MVC   EDICOMN(5),=C'EHELP'                                             
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'CCR'     CC                                           
         MVC   EDICOMN(24),=C'INBOXADMIN@DDSINBOX.COM,'                         
         MVC   EDICOMN+24(L'EMLSEML),EMLSEML                                    
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDIIDEN,=C'SUB'     SUBJECT                                      
         MVC   EDICOMN(43),=C'GET ORDERS VIA SECURE WEB CONNECTION TO D+        
               DS'                                                              
         BAS   RE,PRTLINE                                                       
         DROP  R3                                                               
*                                                                               
         LA    R2,EMLSNAM+L'EMLSNAM-1                                           
         LA    R4,L'EMLSNAM                                                     
*                                                                               
         CLI   0(R2),X'40'                                                      
         BH    *+12                                                             
         BCTR  R4,0                                                             
         BCTR  R2,0                                                             
         B     *-12                                                             
                                                                                
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),EMLSNAM                                                  
         LA    R3,2(R3,R4)                                                      
         MVC   0(25,R3),=C'SENT YOU THIS INVITATION.'                           
         BAS   RE,PRTLINE                                                       
*                                                                               
         XC    0(80,R3),0(R3)                                                   
         BAS   RE,PRTLINE                                                       
*                                                                               
         LA    R1,MESSAGE1         MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
*                                                                               
         XC    0(80,R3),0(R3)                                                   
         BAS   RE,PRTLINE                                                       
*                                                                               
         LA    R1,MSG2A            MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
         LA    R1,MSG2B            MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
         LA    R1,MSG2C            MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
         LA    R1,MSG2D            MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
*                                                                               
         XC    0(80,R3),0(R3)                                                   
         BAS   RE,PRTLINE                                                       
*                                                                               
         LA    R1,MESSAGE3         MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
*                                                                               
         XC    0(80,R3),0(R3)                                                   
         BAS   RE,PRTLINE                                                       
*                                                                               
         LA    R1,MESSAGE4         MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
*                                                                               
         XC    0(80,R3),0(R3)                                                   
         BAS   RE,PRTLINE                                                       
*                                                                               
         LA    R1,MESSAGE5         MESSAGE TO PRINT ENDS WITH X'FF'             
         BAS   RE,PRNTMSG                                                       
*                                                                               
***************                                                                 
* DONE WITH OUR REPORT                                                          
***************                                                                 
PRCLOSE  L     R3,APRTLINE                                                      
         MVC   0(26,R3),=CL26'*** END OF DDS MESSAGE ***'                       
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BAS   RE,PRTLINE                                                       
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                          PRNTMSG                                    *         
***********************************************************************         
PRNTMSG  NTR1                                                                   
*                                                                               
PM00     SR    R4,R4               LETTER COUNTER (MAX 80)                      
         L     R3,APRTLINE                                                      
PM01     SR    R2,R2               WORD COUNTER                                 
*                                                                               
PM05     CLI   0(R1),X'FF'         END OF MESSAGE?                              
         BE    PMMVWORD            YES                                          
PM06     CLI   0(R1),X'40'         END OF WORD?                                 
         BE    PMMVWORD            YES                                          
         AHI   R1,1                BUMP MESSAGE PTR                             
         AHI   R2,1                BUMP WORD LENGTH COUNTER                     
         B     PM05                                                             
*                                                                               
PMMVWORD AR    R4,R2                                                            
         CHI   R4,79               WILL CURRENT LINE HOLD THIS WORD?            
         BH    PMPRNLN             NO                                           
         AHI   R4,1                ADD 1 FOR THE SPACE                          
*                                                                               
PMEX     SR    R1,R2               POINT TO BEGINNING OF WORD TO MOVE           
         EX    R2,*+8              STORE THE WORD ON PRINT LINE                 
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       ** EXECUTED **                               
         LA    R3,1(R3,R2)         BUMP PRINT QUEUE LINE                        
         AR    R1,R2               POINT TO SPACE AFTER WORD JUST MOVED         
         CLI   0(R1),X'FF'         END OF MESSAGE?                              
         BNE   PMNEXT              NO                                           
         SHI   R3,1                YES--DON'T PUT X'FF' TO SCREEN..             
         MVI   0(R3),X'40'         BUT PUT A SPACE THERE                        
         B     PMX                                                              
*                                                                               
PMPRNLN  BAS   RE,PRTLINE          PRINT THE LINE                               
         CLI   0(R1),X'FF'         END OF MESSAGE?                              
         BNE   PM20                                                             
         L     R3,APRTLINE                                                      
         B     PMEX                YES                                          
PM20     SR    R1,R2                                                            
         B     PM00                                                             
*                                                                               
PMNEXT   LA    R1,1(R1)            FIRST LETTER OF NEXT WORD OR X'FF'           
         B     PM01                                                             
*                                                                               
PMX      BAS   RE,PRTLINE                                                       
         B     EXIT                                                             
***********************************************************************         
*                          PRTLINE                                    *         
*                 R1 = A(198 CHAR PRINT LINE)                         *         
***********************************************************************         
PRTLINE  NTR1                                                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                            OPENSPQ                                  *         
*                 SETS UP THE PRINT QUEUE FOR US                      *         
***********************************************************************         
OPENSPQ  NTR1                                                                   
         MVC   REMUSER,=C'DAR'                                                  
         LA    R1,SPOOLKEY                                                      
         USING PQPLD,R1                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=CL11'DARE ORDER'                                         
         OI    GENSTAT3,NOCLRSPK   DON'T CLEAR SPOOL KEY IN OPENPQ              
         MVI   PLCLASS,C'G'                                                     
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         DROP  R1                                                               
         XC    BIGSPLKY,BIGSPLKY                                                
         LA    R1,BIGSPLKY                                                      
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
         MVC   QLRETNL,=H'48'      SET PRINTQ RETAIN TIMES  48 ACTIVE           
         MVC   QLRETND,=H'24'                               24 PRINTED          
         MVI   QLLINET,X'C0'       SET WIDTH TO WHAT IS IN QLLINEW              
         MVI   QLLINEW,198                                                      
         DROP  R1                                                               
*                                                                               
         GOTO1 OPENPQ              OPEN PRINTQ MYSELF                           
*                                                                               
         LA    R6,DOXMELEM         SAVE THE REPORT NUMBER IN ORIGIN ID          
         USING DOXMTELD,R6                                                      
         MVC   DOXMTOID,TWAORIG                                                 
         DROP  R6                                                               
*                                                                               
OPQX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRMIS   MVC   ERRNUM,=AL2(1)       MISSING INPUT FIELD                         
         B     SPERREX                                                          
*                                                                               
ERRBYR   MVC   ERRNUM,=AL2(66)      INVALID BUYER                               
         B     SPERREX                                                          
*                                                                               
ERRINV   MVC   ERRNUM,=AL2(951)     INVALID                                     
         B     SPERREX                                                          
*                                                                               
ERREML   MVC   ERRNUM,=AL2(953)     BUYER MISSING E-MAIL                        
         B     SPERREX                                                          
*                                                                               
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
*                                                                               
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
NOTHING  DC    H'0'                                                             
*                                                                               
MESSAGE1 DC    C'You are invited to join a growing number of stations a+        
               nd non-DARE reps taking advantage of the DDS Inbox, a ne+        
               w method of receiving orders from media buying '                 
         DC    C'organizations who use DDS '                                    
         DC    C'systems. The DDS Inbox delivers orders to your desktop+        
                via a secure internet connection. When orders arrive yo+        
               u will be notified via e-mail. Orders can be printed an'         
         DC    C'd/or stored on your desktop or network.',X'FF'                 
                                                                                
MSG2A    DC    C'Requirements for the DDS Inbox are:',X'FF'                     
MSG2B    DC    C'        PC running Windows',X'FF'                              
MSG2C    DC    C'        Internet Access',X'FF'                                 
MSG2D    DC    C'        Internet Explorer 5.5',X'FF'                           
                                                                                
MESSAGE3 DC    C'If you have an internal network and technical people w+        
               ho support it, there are other considerations.',X'FF'            
                                                                                
MESSAGE4 DC    C'Go to http://www.ddsinbox.com to learn more about how +        
               you can receive orders via the DDS Inbox. You can also '         
         DC    C'send an email to ehelp@DDSInbox.com if you have questi+        
               ons.',X'FF'                                                      
*                                                                               
MESSAGE5 DC    C'I look forward to seeing your name in the growing list+        
                of DDS Inbox users!',X'FF'                                      
*                                                                               
CHARTAB  DC    C'.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                         
         DC    X'FF'                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* DESTOV                                                                        
         DC    AL1(PF06X-*,06,0,0,0,PFTRETRN)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPOMSDSCTS                                                     
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
BOXDL    EQU   *-BOXD                                                           
*                                                                               
       ++INCLUDE DDWIDED                                                        
WIDEDL   EQU   *-WIDED                                                          
*                                                                               
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT                             
       ++INCLUDE SPOMSFFD          BASE SCREEN                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC8D          MAINTENANCE SCREEN                           
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD          LAYOUT FOR ++DDS REC FOR EASYLINK            
         EJECT                                                                  
       ++INCLUDE CTGENFAX          FAX RECORDS                                  
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
*                     START OF SAVED STORAGE                          *         
***********************************************************************         
MYAREAD  DSECT                                                                  
RELO     DS    A                                                                
APRTLINE DS    A                   A(EXTENDED PRINT LINE)                       
SAVEKEY  DS    XL48                                                             
ERRNUM   DS    CL2                 FOR ERRORS                                   
BIGSPLKY DS    XL128               BIG SPOOLKEY                                 
DOXMELEM DS    XL(DOXMTLNQ)        TRANSMISSION ELEMENT                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPOMS18   01/03/07'                                      
         END                                                                    
