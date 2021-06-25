*          DATA SET ACMRK43    AT LEVEL 056 AS OF 03/18/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045076.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T61643A                                                                  
ACMRK43 TITLE 'ROOT ROUTINES - 4'                                               
*                                                                               
* NSHE 001 100504 OVERLAY CREATED AS OTHER ROUTINE MODULES NOW FULL             
* NSHE 008 060105 1002445 ACCQ  FIX DISPLAY SCREEN TO BE PROTECTED              
*                                                                               
ACMRK43  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MK43**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,RC                                                         
         USING TWAD,R6                                                          
         USING SAVED,R7                                                         
         SRL   RF,32-8             BRANCH INDEX HELD IN HOB RF                  
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     GRDINIT             INITIALISE GRID DISPLAY                      
         B     DISGRID             DISPLAY FOR GRID OUTPUT                      
         B     FGRTSAR             FORMAT TSAR RECORD TO GRID                   
         B     DISPDUM             DISPLAY DUMMY LINES ON SCREEN                
         B     UPDGRID             UPDATE RECORD VIA GRID                       
         B     REPELEM             REPLACE ELEMENT                              
         B     SCRNCLR             SCREEN CLEAR                                 
         B     SCRNDIM             SCREEN DIMENSIONS                            
         B     TSTCOL              TEST COLUMN                                  
         B     SUBDIS              SUBSTITUTE COLUMN FOR DISPLAY                
         B     GETMID              GET MARKER ID FOR GRIDS                      
                                                                                
ROU4L    MVI   DUB,0               SET CC LOW                                   
         B     ROU4CC                                                           
ROU4H    MVI   DUB,2               SET CC HIGH                                  
         B     ROU4CC                                                           
ROU4E    MVI   DUB,1               SET CC EQUAL                                 
ROU4CC   CLI   DUB,1                                                            
                                                                                
ROU4X    XIT1  ,                                                                
ROU4ER3  MVI   DUB,1               SET CC EQUAL                                 
ROU4CCR3 CLI   DUB,1                                                            
ROU4XR3  XIT1  REGS=(R3)                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SCREEN LINES - BUILD GRID DETAIL LINES AND HEADERS       *         
* ON ENTRY     R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
* ON EXIT      R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
* REGS USED: R2=A(DUMMY SCREEN LINES)                                 *         
*            R3=A(COLUMN TABLE)                                       *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
GRDINIT  MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
         MVI   GRDNDOR,0                                                        
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,GRDI80           DUMMY PRINT RTN                              
         ST    RF,DLCBAPR                                                       
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBINIT    INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         MVI   TEMP,C' '               BUILD DETAIL OF REQUEST                  
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         MVC   TEMP(L'GRDACNM),GRDACNM TELL PC GRID TO PUT IT ON SCREEN         
         LA    R5,TEMP+L'GRDACNM                                                
         MVC   0(L'LC@ACC,R5),LC@ACC                                            
         LA    R5,L'LC@ACC-1(,R5)                                               
         CLI   0(R5),C' '          SKIP ALL SPACES                              
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'='                                                       
         MVC   2(L'ACTKULA,R5),ACCOUNT+(ACTKUNT-ACTKEY)                         
         LA    R5,2+L'ACTKULA(R5)                                               
         CLI   0(R5),C' '          SKIP ALL SPACES                              
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(,R5)                                                        
         MVC   0(L'ACCNAME,R5),ACCNAME                                          
         LA    R5,L'ACCNAME(R5)                                                 
         CLI   0(R5),C' '          SKIP ALL SPACES                              
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'('          BUILD (BAL=NNNNN.NN) EXPRESSION              
         MVC   2(L'OP3BAL,R5),OP3BAL                                            
         MVI   2+L'OP3BAL(R5),C'='                                              
         LA    R5,3+L'OP3BAL(R5)  R5=A(OUTPUT) FOR CUREDIT                      
         CURED ACCBAL,(17,(R5)),2,ALIGN=LEFT,FLOAT=-                            
         AR    R5,R0               ADD SIGNIFICANT L'AMOUNT                     
         MVI   0(R5),C')'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         LA    RE,TEMP                                                          
         SR    R5,RE               LENGTH OF DOWNLOAD LINE                      
         BAS   RE,BDORTXT                                                       
*                                                                               
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         CLI   XTYPE,TYPVCA                                                     
         BNE   GRDI01                                                           
         MVC   TEMP(L'AC10VCA),AC10VCA                                          
         LHI   R1,L'AC10VCA-1                                                   
         B     *+14                                                             
GRDI01   MVC   TEMP(L'LC@CROR),LC@CROR                                          
         LHI   R1,L'LC@CROR-1                                                   
*        LA    R5,TEMP                                                          
         LA    R5,TEMP(R1)                                                      
*        LA    R5,L'LC@CROR-1(R5)                                               
         CLI   0(R5),C' '          FIND LAST CHAR                               
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(R5)                                                         
         CLI   XACTOLAY,CSOLAY                                                  
         BE    GRDI01A                                                          
         CLI   XACTOLAY,VTOLAY     VENDCASH/APPROVE                             
         BNE   GRDI02                                                           
GRDI01A  MVC   0(L'LC@APRVD-1,R5),LC@APRVD                                      
         LA    R5,L'LC@APRVD-2(R5)                                              
*        MVC   0(L'LC@SEL,R5),LC@SEL                                            
*        LA    R5,L'LC@SEL-1(R5)                                                
         B     GRDI06                                                           
GRDI02   CLI   XACTOLAY,CAOLAY                                                  
         BNE   GRDI04                                                           
         MVC   0(L'LC@ATH,R5),LC@ATH                                            
         LA    R5,L'LC@ATH-1(R5)                                                
         B     GRDI06                                                           
GRDI04   CLI   XACTOLAY,COOLAY                                                  
         BNE   GRDI05                                                           
         MVC   0(L'LC@OFFST,R5),LC@OFFST                                        
         LA    R5,L'LC@OFFST-1(R5)                                              
         B     GRDI06                                                           
GRDI05   MVC   0(L'LC@HOLD,R5),LC@HOLD                                          
         LA    R5,L'LC@HOLD-1(R5)                                               
*                                                                               
GRDI06   CLI   0(R5),C' '          FIND LAST CHAR                               
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   2(L'LC@ON,R5),LC@ON                                              
         MVC   2+L'LC@ON(L'LC@ACC,R5),LC@ACC                                    
         LA    R5,2+L'LC@ON+L'LC@ACC-1(,R5)                                     
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   2(L'ACTKULA,R5),ACCOUNT+(ACTKUNT-ACTKEY)                         
         LA    R5,2+L'ACTKULA(R5)                                               
         CLI   0(R5),C' '          SKIP ALL SPACES                              
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(,R5)                                                        
*                                                                               
         OC    CONTRA,CONTRA       TEST ANY CONTRA A/C?                         
         BZ    GRDI08                                                           
         MVC   0(L'LC@AND,R5),LC@AND                                            
         LA    R5,L'LC@AND-1(,R5)                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   2(L'LC@CTRA,R5),LC@CTRA                                          
         LA    R5,2+L'LC@CTRA-1(,R5)                                            
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    RF,RF                                                            
         IC    RF,CONTRAXL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R5),CONTRA+1                                                 
         LA    R5,2+1+1(RF,R5)                                                  
*                                                                               
GRDI08   MVC   0(L'LC@UDET,R5),LC@UDET                                          
         LA    R5,L'LC@UDET-1(,R5)                                              
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   2(L'LC@OPTS,R5),LC@OPTS                                          
         LA    R5,2+L'LC@OPTS-1(,R5)                                            
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'='                                                       
         SR    RF,RF                                                            
         ICM   RF,1,MRKOPTH+5                                                   
         BNZ   GRDI10                                                           
         MVC   2(L'LC@NONE,R5),LC@NONE NO OPTIONS                               
         LA    R5,2+L'LC@NONE(,R5)                                              
         B     GRDI12                                                           
*                                                                               
GRDI10   BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R5),MRKOPT                                                   
         LA    R5,2+1(RF,R5)                                                    
*                                                                               
GRDI12   LA    RE,TEMP                                                          
         SR    R5,RE               R5=LENGTH OF FIELD                           
         BAS   RE,BDORTXT          BUILD OPTION DOWNLOAD LINE                   
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTO1 VDLFLD,DLCBD                                                     
         L     RF,DLCBTOTL                                                      
         STC   RF,LINSUSED         NUMBER OF PRINT LINES                        
         STC   RF,GRDNDOR          NUMBER OF DETAIL OF REQ. LINES               
         L     R2,DLCBAPL          NEXT AVAILABLE SCREEN LINE                   
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,GRDI80           DUMMY PRINT RTN FOR INIT                     
         ST    RF,DLCBAPR                                                       
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR REPORT                             
*                                                                               
         L     R3,ADISTAB                                                       
         USING DISTABD,R3                                                       
GRDI20   CLI   DISCOL,EOT          END OF TABLE                                 
         BE    GRDIX                                                            
         ST    R3,SAVER3                                                        
         GOTO1 ATSTCOL             CHECK COLUMN IS VALID                        
         BNE   GRDI78              NOT VALID - GET NEXT COLUMN                  
         OC    DISADDR,DISADDR     IF THIS COLUMN A SUBSTITUTE ONE              
         BNZ   GRDI22              NO - SOMETHING PRESENT                       
         GOTO1 ASUBDIS             GO READ FOR SUBSTITUTE COLUMN                
         BNE   GRDI78              NOTHING FOUND THAT IS VALID                  
         GOTO1 ATSTCOL             CHECK COLUMN IS VALID                        
         BNE   GRDI78              NOT VALID - GET NEXT COLUMN                  
GRDI22   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         SR    RE,RE                                                            
         MVC   LARFADDR,DISADDR                                                 
         EX    0,LARF                                                           
         SR    RE,RE                                                            
         IC    RE,DISLEN           LENGTH OF COLUMN - 1                         
         SHI   RE,1                                                             
         EX    RE,*+4                                                           
         MVC   DLCBFLD(0),0(RF)    MOVE COLUMN HEADING                          
         LA    R5,DLCBFLD(RE)      END OF COLUMN HEADING                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*&&UK                                                                           
         TM    DISCIND,DISCI2CQ    IS COLUMN FOR SECONDARY CURRENCY             
         BZ    GRDI24              NO                                           
*                                                                               
         OC    SCNDCURT,SCNDCURT   HAVE WE GOT A SECOND CURRENCY                
         BZ    GRDI26              NO                                           
         TM    SCURIND1,SCURINVD   IS THE SECOND CURRENCY VALID                 
         BO    GRDI26              NO                                           
         MVC   1(L'CURTCUR,R5),SCNDCURT+(CURTCUR-CURTABD)                       
         LA    R5,1+L'CURTCUR(R5)                                               
         B     GRDI28                                                           
*                                                                               
GRDI24   TM    DISCIND,DISCIFCQ    DO WE REPLACE DISADD2                        
         BZ    GRDI26              NO                                           
*                                                                               
         TM    SAFCIND1,SAFCI1SC   YES - NEED TO CHECK HAVE SINGLE              
         BZ    GRDI26                        FOREIGN CURRENCY                   
         MVC   1(L'CURTCUR,R5),FORECURT+(CURTCUR-CURTABD)                       
         LA    R5,1+L'CURTCUR(R5)                                               
         B     GRDI28                                                           
*&&                                                                             
*                                                                               
GRDI26   OC    DISADD2,DISADD2                                                  
         BZ    GRDI30                                                           
         MVI   1(R5),C'|'                                                       
         LA    R5,1(R5)                                                         
         SR    RE,RE               CLEAR RE                                     
         MVC   LARFADDR,DISADD2                                                 
         EX    0,LARF                                                           
         SR    RE,RE                                                            
         IC    RE,DISLEN2          LENGTH OF COLUMN - 1                         
         SHI   RE,1                                                             
         EX    RE,*+4                                                           
         MVC   1(0,R5),0(RF)       MOVE COLUMN HEADING                          
         AHI   RE,1                END OF COLUMN HEADING                        
         AR    R5,RE                                                            
GRDI28   CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
GRDI30   MVI   1(R5),C'*'                                                       
*                                                                               
         TM    DISCIND4,DISI4ID                                                 
         BZ    GRDI32                                                           
         MVC   2(2,R5),=C'ID'                                                   
         B     GRDI34                                                           
GRDI32   GOTO1 VHEXOUT,DMCB,DISCOL,2(R5),L'DISCOL,0                             
*                                                                               
GRDI34   OC    DISCIND2,DISCIND2                                                
         BNZ   GRDI36                                                           
         OC    DISCIND4,DISCIND4       TEST ANY COLUMN INFO.                    
         BNZ   GRDI36                                                           
         OC    DISCIND3,DISCIND3       TEST ANY COLUMN INFO.                    
         BZ    GRDI76                  NO - CALL DLFLD                          
GRDI36   MVI   4(R5),C'*'                                                       
         LA    R5,5(,R5)                                                        
         TM    DISCIND3,DISI3TRE                                                
         BZ    GRDI38                                                           
         MVI   0(R5),C'Z'                                                       
         LA    R5,1(R5)                                                         
         OC    DISCIND2,DISCIND2                                                
         BZ    GRDI76                                                           
GRDI38   TM    DISCIND2,DISI2DTE                                                
         BZ    GRDI40                                                           
         MVI   0(R5),C'D'                                                       
         LA    R5,1(R5)                                                         
GRDI40   TM    DISCIND2,DISI2NUM                                                
         BZ    GRDI48                                                           
         MVI   0(R5),C'N'                                                       
         LA    R5,1(R5)                                                         
         TM    DISCIND2,DISI2NTO                                                
         BNZ   GRDI46                                                           
*&&UK                                                                           
         TM    DISCIND,DISCI2CQ    IS COLUMN FOR SECONDARY CURRENCY             
         BZ    GRDI42              NO                                           
*                                                                               
         OC    SCNDCURT,SCNDCURT   HAVE WE GOT A SECOND CURRENCY                
         BZ    GRDI46              NO                                           
         TM    SCURIND1,SCURINVD   IS THE SECOND CURRENCY VALID                 
         BO    GRDI46              NO                                           
         B     GRDI44                                                           
*                                                                               
GRDI42   TM    DISCIND,DISCIFCQ    SINGLE FOREIGN CURRENCY COLUMN               
         BZ    GRDI44              NO                                           
*                                                                               
         TM    SAFCIND1,SAFCI1SC   IF SINGLE FOREIGN CURRENCY SET NO            
         BZ    GRDI46                                     RENAME                
*&&                                                                             
GRDI44   MVI   0(R5),C'T'                                                       
         LA    R5,1(R5)                                                         
         B     GRDI48                                                           
*                                                                               
GRDI46   MVI   0(R5),C'G'                                                       
         LA    R5,1(R5)                                                         
GRDI48   TM    DISCIND2,DISI2CKB                                                
         BZ    GRDI50                                                           
         MVC   0(2,R5),=C'AX'                                                   
         LA    R5,2(R5)                                                         
GRDI50   TM    DISCIND2,DISI2RGH                                                
         BZ    GRDI52                                                           
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)                                                         
GRDI52   TM    DISCIND,DISCI2CQ    IS COLUMN FOR SECONDARY CURRENCY             
         BZ    GRDI54              NO                                           
*&&UK                                                                           
         OC    SCNDCURT,SCNDCURT   HAVE WE GOT A SECOND CURRENCY                
         BZ    GRDI56              NO                                           
         TM    SCURIND1,SCURINVD   IS THE SECOND CURRENCY VALID                 
         BO    GRDI56              NO                                           
*&&                                                                             
GRDI54   TM    DISCIND4,DISI4HDE   DO WE WANT TO HIDE THIS COLUMN               
         BZ    GRDI58                                                           
GRDI56   MVI   0(R5),C'H'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
GRDI58   TM    DISCIND,DISCI2CQ    IS COLUMN FOR SECONDARY CURRENCY             
*&&UK                                                                           
         BZ    GRDI60              NO                                           
*                                                                               
         OC    SCNDCURT,SCNDCURT   HAVE WE GOT A SECOND CURRENCY                
         BZ    GRDI62              NO                                           
         TM    SCURIND1,SCURINVD   IS THE SECOND CURRENCY VALID                 
         BZ    GRDI64              YES                                          
*                                                                               
GRDI60   TM    DISCIND,DISCIFCQ    SINGLE FOREIGN CURRENCY COLUMN               
         BZ    GRDI62              NO                                           
*                                                                               
         TM    SAFCIND1,SAFCI1SC   IF SINGLE FOREIGN CURRENCY SET NO            
         BNZ   GRDI64                                     RENAME                
*&&                                                                             
GRDI62   TM    DISCIND2,DISI2REN                                                
         BZ    GRDI66                                                           
GRDI64   MVI   0(R5),C'F'                                                       
         LA    R5,1(R5)                                                         
GRDI66   TM    DISCIND2,DISI2REL   DO WE HAVE A RELATIONSHIP GROUP              
         BZ    GRDI68              NO                                           
         TM    DISCIND4,DISI4MST   IS THIS THE MASTER COLUMN OF GROUP           
         BZ    GRDI67              NO                                           
         MVC   0(6,R5),=C'B#  M#'  YES - MARK COLUMN AS SUCH FOR PC             
         GOTO1 VHEXOUT,DMCB,DISRELA,2(R5),L'DISRELA,0                           
         LA    R5,6(R5)                                                         
         B     GRDI68                                                           
GRDI67   MVC   0(5,R5),=C'B#  #'                                                
         GOTO1 VHEXOUT,DMCB,DISRELA,2(R5),L'DISRELA,0                           
         LA    R5,5(R5)                                                         
GRDI68   TM    DISCIND2,DISI2EDT                                                
         BZ    GRDI76                                                           
         OC    DISSECU,DISSECU                                                  
         BZ    GRDI70                                                           
         USING SECD,R1                                                          
         L     R1,ASECBLK                                                       
         TM    SECINDS,SECIOLD                                                  
         BNZ   GRDI76                                                           
         GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),DISSECU                        
         BNE   GRDI76                                                           
GRDI70   TM    DISCIND3,DISI3MUL                                                
         BNZ   GRDI72                                                           
         MVI   0(R5),C'X'                                                       
         LA    R5,1(R5)                                                         
GRDI72   TM    DISCIND2,DISI2DTE                                                
         BZ    GRDI74                                                           
         MVI   0(R5),C'E'                                                       
         B     GRDI76                                                           
GRDI74   MVC   0(2,R5),=C'E#'                                                   
         EDIT  DISDLEN,(3,2(R5)),ALIGN=LEFT,FILL=0,ZERO=NOBLANK                 
         MVI   5(R5),C'#'                                                       
*                                                                               
GRDI76   GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
GRDI78   L     R3,SAVER3                                                        
         SR    RF,RF                                                            
         IC    RF,DISLN            LENGTH OF COLUMN ENTRY                       
         AR    R3,RF                                                            
         B     GRDI20                                                           
*                                                                               
GRDI80   L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
*                                                                               
GRDIX    MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTO1 VDLFLD,DLCBD                                                     
         L     RF,DLCBTOTL             NUM OF HEADING LINES                     
         MHI   RF,L'DUMLIN1            RF=LENGTH OF HEADINGS                    
         L     R2,ADUMLINE             R2=A(FIRST SCREEN LINE)                  
         SR    R1,R1                                                            
         IC    R1,GRDNDOR                                                       
         MHI   R1,L'DUMLIN1                                                     
         LA    R2,0(R1,R2)             R2=A(FIRST HEADING LINE)                 
         GOTO1 VSQASHER,DMCB,(R2),(RF) SQUASH THE HEADINGS                      
         ICM   RF,15,DMCB+4                                                     
         LA    RF,2(,RF)           RF=NEW LENGTH OF HEADINGS                    
         SR    RE,RE                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         SR    R1,R1                                                            
         IC    R1,GRDNDOR          NO. OF DETAIL OF REQ. LINES                  
         LA    RF,0(R1,RF)                                                      
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    *+8                                                              
         LA    RF,1(,RF)           RF=NEW TOTAL NUMBER OF LINES                 
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
         GOTO1 ADISPDUM,0          DISPLAY DUMMY SCREEN LINES ON SCREEN         
         B     ROU4X                                                            
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*        BUILD DOWNLOAD DATA                                          *         
* ON ENTRY     R2=A(DUMMY SCREEN LINES)                               *         
*              R4=A(DOWNLOAD BLOCK)                                   *         
*              R5=LENGTH OF TEXT DATA                                 *         
*              TEMP=TEXT DATA UPTO 74 CHARACTERS                      *         
***********************************************************************         
         SPACE 2                                                                
         USING DLCBD,R4                                                         
BDORTXT  LR    R0,RE                                                            
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         CHI   R5,L'DLCBFLD                                                     
         BH    *+14                                                             
         MVC   DLCBFLD,TEMP                                                     
         B     BDTXTP              PUT TEXT INTO DOWNLOAD BLOCK                 
*                                                                               
         CHI   R5,L'DUMLIN1-4      MAKE SURE IT'S NOT TOO LONG                  
         BH    BDTXT06             YES - DON'T CALL DLFLD                       
         OI    DLCBFLG1,DLCBFXFL   IF CAN'T FIX IN DLCBFLD,                     
         MVC   DLCBFLX,TEMP        MOVE DATA TO EXTENDED FIELD                  
         B     BDTXTP                                                           
*                                                                               
BDTXT06  L     RE,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         AH    RE,DLCBNUMC         RE=A(NEXT AVAIL CHR IN PRINT LINE)           
         MVI   0(RE),C'"'                                                       
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,RE),TEMP        MOVE IN FIELD                                
         LA    RE,1+1(RF,RE)       END OF FIELD                                 
         MVI   0(RE),C'"'                                                       
         L     RF,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         SR    RE,RF                                                            
         LA    RE,2(,RE)           TOTAL LENGTH FROM START OF LINE              
         L     RF,DLCBTOTC         BUMP TOTAL NUMBER OF CHARS                   
         AR    RF,RE                                                            
         ST    RF,DLCBTOTC                                                      
         SRDL  RE,32                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         STH   RE,DLCBNUMC         BUMP NUM OF CHARS THIS LINE                  
         MVC   DLCBNUMF,=H'1'      BUMP NUM OF FIELDS THIS LINE                 
         L     RE,DLCBTOTL         BUMP TOTAL NUMBER OF LINES                   
         AR    RE,RF                                                            
         ST    RE,DLCBTOTL                                                      
         MHI   RF,L'DUMLIN1                                                     
         L     RE,DLCBAPL          BUMP NUM OF DOWNLOAD PRINT LINES             
         AR    RE,RF                                                            
         ST    RE,DLCBAPL                                                       
         B     BDTXTX                                                           
*                                                                               
BDTXTP   STC   R5,DLCBLEN          LENGTH OF FIELD                              
         GOTO1 VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
         B     BDTXTX                                                           
*                                                                               
BDTXTX   MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTO1 VDLFLD,DLCBD                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        FORMAT A TSAR RECORD INTO GRID SCREEN LINES                  *         
*                                                                     *         
* REGS USED: R2=A(DUMMY SCREEN LINES)                                 *         
*            R3=A(COLUMN TABLE)                                       *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
DISGRID  XR    R2,R2                                                            
         XC    MRKMSG,MRKMSG                                                    
         GOTO1 ASCRNCLR,DMCB,0                                                  
         GOTO1 ASCRNDIM,DMCB,(0,GRDDAT1H)                                       
         NI    DISIND,X'FF'-DISIEOF                                             
         MVI   DISIND2,0                                                        
         TM    DISIND,DISIRST                                                   
         BNZ   DISG10                                                           
         LH    R2,DISNEXT                                                       
         B     DISG12                                                           
*                                                                               
DISG10   AHI   R2,1                                                             
DISG12   CH    R2,DISMAX                                                        
         BNH   DISG20                                                           
         XR    R2,R2                                                            
         AHI   R2,1                                                             
         OI    DISIND,DISIEOF          SET END                                  
         TM    DISIND,DISINIT      TEST WHETHER WE DISPLAYED ANYTHING           
         BZ    DISG14                                                           
         TM    DISIND2,DISEOGR     TEST WHETHER WE HAVE SET END OF GRID         
         BNZ   DISG35              YES                                          
         OI    DISIND2,DISEOGR     SET END OF GRID REQUIRED FOR FGRTSAR         
         GOTO1 AFGRTSAR                                                         
         GOTO1 ADISPDUM                                                         
         BE    DISG35                                                           
         NI    DISIND,X'FF'-DISIEOF  REMOVE SET END                             
         B     DISG30                                                           
DISG14   OI    PCDRIVEN,PCGRMSG1                                                
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     DISG50                                                           
*                                                                               
DISG20   STH   R2,DISNUM           SET RECORD NUMBER AND GET RECORD             
         GOTO1 ATSARGET,DISNUM                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             FILTER THE RECORD                            
         BNE   DISG10              GET NEXT IF FILTERED OUT                     
         NI    DISIND2,X'FF'-(DISNOTFS+DISMULT)                                 
         GOTO1 AFGRTSAR                                                         
*        CHI   R2,3                FOR DEBUGGING                                
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         GOTO1 ADISPDUM                                                         
         BNE   DISG30                                                           
         CH    R2,DISMAX                                                        
         BNE   DISG10                                                           
         OI    DISIND2,DISEOGR     SET END OF GRID DATA DONE                    
         B     DISG10                                                           
DISG30   STH   R2,DISNEXT                                                       
DISG35   TM    DISIND,DISIRST                                                   
         BNZ   DISG40                                                           
         MVC   MRKMSG(L'GRDMSG1),GRDMSG1                                        
         B     DISG45                                                           
DISG40   NI    DISIND,X'FF'-(DISIRST)                                           
         MVC   MRKMSG(L'GRDMSG2),GRDMSG2                                        
         SR    RF,RF                                                            
         IC    RF,GRDNDOR          NO. DETAIL OF REQ. LINES                     
         AHI   RF,GRDSTRTQ         ADD POS. OF GRID START LINE                  
         CURED (RF),(2,MRKMSG+GRDPDATQ),0,ALIGN=LEFT                            
         GOTO1 AGETMID,DMCB,0     GET ENQUIRY ID NUMBER                         
         L     RF,0(,R1)                                                        
         MVC   MRKMSG+L'GRDMSG2(L'MIDREF),0(RF)                                 
DISG45   OI    PCDRIVEN,PCGRMSG2                                                
DISG50   LA    R2,MRKOPTH                                                       
         ST    R2,FVADDR                                                        
         TM    DISIND,DISIEOF                                                   
         BZ    ROU4X                                                            
         NI    DISIND,X'FF'-DISINIT                                             
         OI    DISIND,DISIRST                                                   
         B     ROU4X                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO GRID SCREEN LINES                  *         
*                                                                     *         
* REGS USED: R2=A(DUMMY SCREEN LINES)                                 *         
*            R3=A(COLUMN TABLE)                                       *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
FGRTSAR  TM    DISIND,DISINIT      TEST WHETHER GRID INITIALIZED                
         BNZ   FGRT10                                                           
         GOTO1 AGRDINIT                                                         
         OI    DISIND,DISINIT                                                   
*                                                                               
FGRT10   MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,FGRT300          DUMMY PRINT RTN                              
         ST    RF,DLCBAPR                                                       
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR SCREEN LINE                        
         TM    DISIND2,DISEOGR     ARE WE BUILDING END OF GRID DATA             
         BNZ   FGRTX               YES                                          
*                                                                               
         USING DISTABD,R3                                                       
FGRT12   L     R3,ADISTAB          COLUMN TABLE                                 
FGRT14   CLI   DISCOL,EOT                                                       
         BE    FGRTX                                                            
         ST    R3,SAVER3                                                        
         GOTO1 ATSTCOL             DO WE WANT COLUMN 1?                         
         BNE   FGRT210                                                          
         OC    DISADDR,DISADDR     IF THIS COLUMN A SUBSTITUTE ONE              
         BNZ   FGRT16              NO - SOMETHING PRESENT                       
         GOTO1 ASUBDIS             GO READ FOR SUBSTITUTE COLUMN                
         BNE   FGRT210             NOTHING FOUND THAT IS VALID                  
         GOTO1 ATSTCOL             CHECK COLUMN IS VALID                        
         BNE   FGRT210             NOT VALID - GET NEXT COLUMN                  
FGRT16   LA    R5,DLCBFLD                                                       
         TM    DISIND2,DISNOTFS    CHECK NOT FIRST TIME                         
         BNZ   FGRT48                                                           
         TM    DISIND2,DISMULT     CHECK DEALING WITH MULTIPLE ROWS             
         BNZ   FGRT18              YES                                          
         MVC   DLCBFLD(5),=C'#O0 #' NO                                          
         LA    R5,DLCBFLD+5                                                     
         TM    TSARINDS,TSARDISQ   ARE WE DISPLAYING ONLY                       
         BZ    FGRT48                                                           
         MVC   DLCBFLD(6),=C'#O0W #'   YES                                      
         LA    R5,DLCBFLD+6                                                     
         B     FGRT48                                                           
FGRT18   MVC   DLCBFLD(9),=C'#O1E24X #' SET TREE VIEW                           
         LA    R5,DLCBFLD+9                                                     
         TM    TSARINDS,TSARDISQ   ARE WE DISPLAYING ONLY                       
         BZ    FGRT48                                                           
         MVC   DLCBFLD(9),=C'#O1E24W #' YES - SET TREE VIEW                     
FGRT48   OC    DISDDATA,DISDDATA                                                
         BZ    FGRT50              DATA NOT HELD ON TSARREC                     
         SR    RE,RE                                                            
         MVC   LARFADDR,DISDDATA                                                
         EX    0,LARF              RF=A(DATA)                                   
         B     FGRT120                                                          
*                                                                               
FGRT50   CLC   IODA,TSARDADR       IS DISK ADDRESS SAME AS LAST TIME            
         BE    FGRT102                                                          
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
FGRT102  L     R2,AIOBUFF                                                       
         USING TRNRECD,R2                                                       
         LA    RF,TRNRFST          RF=A(FIRST ELEMENT)                          
         DROP  R2                                                               
         USING FFTELD,RF                                                        
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,ELTOSKIP         ELEMENTS TO SKIP                             
FGRT104  CLI   FFTEL,0             END OF RECORD                                
         BE    FGRT128             YES                                          
         CLC   FFTEL,DISELEQ       DOES IT MATCH ELEMENT ON TABLE               
         BNE   FGRT110             NO                                           
         CLI   DISETYPE,0          DO WE HAVE TO FIND A CERTAIN TYPE            
         BE    FGRT106             NO                                           
         CLC   FFTTYPE,DISETYPE    CHECK MATCHES TYPE REQUIRED IN TABLE         
         BNE   FGRT110                                                          
FGRT106  TM    DISIND2,DISMULT     ARE WE DOING THE MULTIPLE ROWS               
         BZ    FGRT112             NO                                           
         TM    DISCIND3,DISI3MUL   DOES COLUMN HAVE MULTIPLE ROWS               
         BZ    FGRT128             NO - PUT OUT EMPTY COLUMN                    
         LTR   R1,R1               HAVE WE REACH CORRECT ELEMENT                
         BZ    FGRT112             YES - PUT OUT DATA FROM THIS ELEMENT         
         SHI   R1,1                SUBTRACT COUNT AND GO TO NEXT EL             
FGRT110  IC    RE,FFTLN            BUMP TO NEXT ELEMENT                         
         AR    RF,RE                                                            
         B     FGRT104                                                          
*                                                                               
FGRT112  TM    DISCIND3,DISI3MUL   DOES THIS COLUMN HAVE MULTIPLE ROWS          
         BZ    FGRT118             NO                                           
         TM    DISIND2,DISMULT     ARE WE DEALING WITH MULTIPLE ROWS            
         BNZ   FGRT118             YES - PUT OUT DATA                           
         ST    RF,SAVERF           WORK OUT HOW MANY EXTRA ELEMENTS OF          
         SR    R1,R1               THIS TYPE ARE PRESENT                        
FGRT114  IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         CLI   FFTEL,0                                                          
         BE    FGRT116                                                          
         CLC   FFTEL,DISELEQ                                                    
         BNE   FGRT114                                                          
         AHI   R1,1                ADD UP NUMBER OF EXTRA ELEMENTS              
         B     FGRT114                                                          
*                                                                               
FGRT116  STC   R1,NUMELEMS         STORE NUMBER OF ELEMENTS                     
         XC    ELTOSKIP,ELTOSKIP                                                
         L     RF,SAVERF           RESTORE RF                                   
*                                                                               
FGRT118  SR    RE,RE                                                            
         IC    RE,FFTLN            RE=TOTAL LENGTH OF ELEMENT                   
         SR    R1,R1                                                            
         IC    R1,DISEDISP         R1=DISPLACEMENT ON ELEMENT TO DATA           
         CR    R1,RE               IS DISPLACEMENT GREATER THAN LENGTH          
         BNL   FGRT128             SEND EMPTY COLUMN                            
         AR    RF,R1               RF=A(DATA)                                   
         SR    RE,R1               RE=LENGTH OF VARIABLE LENGTH DATA            
         TM    DISCIND4,DISI4VAR   IS DATA VARIABLE IN LENGTH                   
         BNZ   FGRT122             YES                                          
FGRT120  SR    RE,RE                                                            
         IC    RE,DISDLEN          LENGTH OF DATA                               
FGRT122  TM    DISCIND2,DISI2NUM   WHAT TYPE OF DATA IS IT?                     
         BNZ   FGRT140             NUMERICAL DATA                               
         TM    DISCIND2,DISI2DTE                                                
         BNZ   FGRT130             DATE DATA                                    
         TM    DISCIND2,DISI2CKB                                                
         BNZ   FGRT160             CHECKBOX DATA                                
         TM    DISCIND4,DISI4STA                                                
         BNZ   FGRT170             STATUS DEPENDENT DATA                        
         CHI   RE,L'DLCBFLD        IS DATE LONGER THAN FIELD                    
         BNH   FGRT126                                                          
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         STC   RE,BYTE2            STORE LENGTH                                 
         SHI   RE,1                SUBTRACT 1 FOR EX                            
         EX    RE,*+4                                                           
         MVC   TEMP(0),0(RF)       MOVE DATA TO TEMP                            
         BAS   RE,FGDLLTXT                                                      
         B     FGRT200                                                          
FGRT126  SHI   RE,1                SUBTRACT 1 FOR EX                            
         EX    RE,*+4                                                           
         MVC   0(0,R5),0(RF)       MOVE COLUMN DATA                             
FGRT128  BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT130  TM    DISCIND3,DISI3MOS   IS THE DATE A MONTH YEAR                     
         BZ    FGRT132                                                          
         MVC   WORK(L'TSARMOS),0(RF)                                            
         CLC   WORK(L'TSARMOS),SPACES                                           
         BNH   FGRT131                                                          
         MVI   WORK+2,X'01'        SET TO FIRST DAY                             
         GOTO1 VDATCON,DMCB,(DISDFORM,WORK),(9,(R5))                            
FGRT131  BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT132  TM    DISCIND3,DISI3DAY                                                
         BNZ   FGRT134                                                          
         MVC   WORK(3),0(RF)                                                    
         GOTO1 VDATCON,DMCB,(DISDFORM,WORK),(17,(R5))                           
         BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT134  DS    0H                                                               
*&&US                                                                           
         CLC   TSARFMMD,SPACES                                                  
         BNH   FGRT135                                                          
         MVC   WORK(L'TSARFMMD),TSARFMMD                                        
         LA    R0,17               SET FOR MMMDD/YY OUTPUT                      
         CLI   WORK+L'TSARFMMD-1,X'00'                                          
         BNE   *+12                                                             
         LA    R0,9                SET FOR MMM/YY OUTPUT                        
         MVI   WORK+L'TSARFMMD-1,X'01'                                          
         GOTO1 VDATCON,DMCB,(DISDFORM,WORK),((R0),(R5))                         
*&&                                                                             
FGRT135  BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT140  TM    DISCIND3,DISI3EDR   EDIT EXCHANGE RATE                           
         BNZ   FGRT158                                                          
         TM    DISCIND3,DISI3DR    NUMBER ONLY                                  
         BZ    FGRT142                                                          
         TM    TSARINDS,TSARDRQ                                                 
         BNZ   FGRT144                                                          
         B     FGRT128                                                          
FGRT142  TM    DISCIND3,DISI3CR                                                 
         BZ    FGRT144                                                          
         TM    TSARINDS,TSARDRQ                                                 
         BNZ   FGRT128                                                          
*                                                                               
FGRT144  CLI   DISNTYPE,DISNPCKD   IS NUMERICAL DATA PACKED                     
         BNE   FGRT152             NO                                           
         CLI   DISDLEN,6           TEST LENGTH IS PL6                           
         BE    *+6                 YES                                          
         DC    H'0'                DIE IF WE DON'T HAVE CURED CALL              
         OC    0(6,RF),0(RF)       ANY DATA PRESENT                             
         BZ    FGRT128                                                          
         ZAP   DUB,0(6,RF)                                                      
         TM    DISCIND,DISCIPCQ    IS COLUMN PRIMARY CURRENCY                   
         BNZ   FGRT148                                                          
*&&UK                                                                           
         TM    DISCIND,DISCIFCQ    IS COLUMN FOREIGN CURRENCY                   
         BNZ   FGRT150                                                          
         TM    DISCIND3,DISI3SEC   DO WE NEED TO CALC SECONDARY VALUE           
         BZ    FGRT146             NO                                           
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0             CALCULATE SECONDARY VALUE                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
FGRT146  CURED (P8,DUB),(16,(R5)),SCNDCURT,MINUS=YES,ALIGN=LEFT                 
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT148  CURED (P8,DUB),(16,(R5)),COMPCURT,MINUS=YES,ALIGN=LEFT                 
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*&&                                                                             
*&&US                                                                           
FGRT148  CURED (P8,DUB),(16,(R5)),2,MINUS=YES,ALIGN=LEFT                        
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*&&                                                                             
*&&UK                                                                           
FGRT150  CLC   TSARAFCC,SPACES                                                  
         BNH   FGRT128             PUT OUT BLANK COLUMN IF NO CURRENCY          
         USING CURTABD,RF                                                       
         L     RF,ACURRTAB         SEARCH CURRENCY TABLE                        
         LA    R0,CURRTABN                                                      
         CLI   CURTCUR,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF WE DON'T FIND CURRENCY                
         CLC   CURTCUR,TSARAFCC                                                 
         BE    *+14                                                             
         LA    RF,L'CURRTAB(RF)                                                 
         BCT   R0,*-24                                                          
         DC    H'0'                                                             
         CURED (P8,DUB),(16,(R5)),CURTABD,MINUS=YES,ALIGN=LEFT,        X        
               CURSYMB=YES                                                      
         ORG   *-2                                                              
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE FOREIGN CURRENCY                 
         BZ    *+8                                                              
         OI    0(R1),B'00000100'   EXCLUDE CURRENCY SYMBOL                      
         BASR  RE,RF                                                            
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*&&                                                                             
FGRT152  CLI   DISNTYPE,DISNBINY   MUST BE BINARY NUMERICAL DATA                
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT                                   
         CLI   DISDLEN,1           IS IT ON BYTE                                
         BNE   FGRT154             NO                                           
         CURED (B1,(RF)),(3,(R5)),0,MINUS=YES,ALIGN=LEFT                        
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT154  CLI   DISDLEN,2           MUST BE 2                                    
         BE    *+6                                                              
         DC    H'0'                DIE OF NOT                                   
         TM    DISCIND4,DISI4ID                                                 
         BZ    FGRT156                                                          
         L     RE,ATSARBLK                                                      
         LA    RF,TSRNUM-TSARD(RE)                                              
FGRT156  CURED (B2,(RF)),(5,(R5)),0,MINUS=YES,ALIGN=LEFT                        
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT158  GOTO1 AEDTRAT,DMCB,(RF),(16,(R5))                                      
         BAS   RE,FGDLNUM          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT160  TM    0(RF),TSARDISQ      IS ITEM DISPLAY ONLY                         
         BZ    FGRT162             NO                                           
         MVI   0(R5),C' '          SET SPACE TO DENOTE NOT AVAILABLE            
         B     FGRT166                                                          
FGRT162  TM    0(RF),TSARMKQ       IS ITEM ALREADY MARKED                       
         BZ    FGRT164             NO                                           
         MVC   0(2,R5),=C'-1'                                                   
         B     FGRT166                                                          
FGRT164  MVI   0(R5),C'0'          SET 0 FOR NO MARKING AND AVAILABLE           
FGRT166  BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT170  CLI   DISISTAT,TSARDRQ                                                 
         BE    FGRT174                                                          
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,DISISTAT       TEST STATUS MATCHES DATA                     
         CLC   BYTE,DISISTAT                                                    
         BNE   FGRT172             NO MATCH                                     
         MVC   0(L'AC@YES,R5),AC@YES                                            
         B     FGRT178                                                          
FGRT172  MVC   0(L'AC@NO,R5),AC@NO                                              
         B     FGRT178                                                          
*                                                                               
FGRT174  MVC   0(L'AC@DR,R5),AC@DR                                              
         TM    0(RF),TSARDRQ                                                    
         BNZ   FGRT178                                                          
         MVC   0(L'AC@DR,R5),SPACES                                             
         MVC   0(L'AC@CR,R5),AC@CR                                              
FGRT178  BAS   RE,FGDLTXT          BUILD COLUMN                                 
         B     FGRT200                                                          
*                                                                               
FGRT200  OI    DISIND2,DISNOTFS                                                 
FGRT210  L     R3,SAVER3           RESTORE R3                                   
         SR    RF,RF                                                            
         IC    RF,DISLN            BUMP TO NEXT COLUMN ENTRY                    
         AR    R3,RF                                                            
         B     FGRT14                                                           
         DROP  R3                                                               
*                                                                               
FGRT300  L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
*                                                                               
FGRTX    MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         TM    DISIND2,DISEOGR     ARE WE BUILDING END OF GRID DATA             
         BNZ   FGRTX20             YES                                          
*                                                                               
         CLI   NUMELEMS,0                                                       
         BE    FGRTX20                                                          
         OI    DISIND2,DISMULT                                                  
         NI    DISIND2,X'FF'-DISNOTFS                                           
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         IC    R1,NUMELEMS                                                      
         IC    RE,ELTOSKIP                                                      
         SHI   R1,1                                                             
         AHI   RE,1                                                             
         STC   R1,NUMELEMS                                                      
         STC   RE,ELTOSKIP                                                      
         B     FGRT12                                                           
*                                                                               
FGRTX20  SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         A     RF,DLCBTOTL         NUM OF LINES PUT TO DUMMY SCREEN             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
         TM    DISIND2,DISEOGR     ARE WE BUILDING END OF GRID DATA             
         BNZ   FGRTX30             YES                                          
         CLC   DISNUM,DISMAX       HAVE WE REACHED LAST RECORD                  
         BNE   FGRTXIT                                                          
FGRTX30  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 VDLFLD,DLCBD        END OF RECORD                                
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
FGRTXIT  SR    RF,RF                   CLEAR RF                                 
         IC    RF,LINSUSED             NUM OF HEADING LINES                     
         MHI   RF,L'DUMLIN1            RF=LENGTH OF HEADINGS                    
         L     R2,ADUMLINE             R2=A(FIRST SCREEN LINE)                  
         GOTO1 VSQASHER,DMCB,(R2),(RF) SQUASH THE HEADINGS                      
         ICM   RF,15,DMCB+4                                                     
         LA    RF,2(,RF)           RF=NEW LENGTH OF HEADINGS                    
         SR    RE,RE                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    *+8                                                              
         LA    RF,1(,RF)           RF=NEW TOTAL NUMBER OF LINES                 
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
         B     ROU4X                                                            
         SPACE 2                                                                
***********************************************************************         
*        BUILD DOWNLOAD DATA                                          *         
* ON ENTRY     R2=A(DUMMY SCREEN LINES)                               *         
*              R4=A(DOWNLOAD BLOCK)                                   *         
*              FOR FGDLTXT/FGDLNUM                                    *         
*                  - DLCBFLD = TEXT/NUMERIC DATA                      *         
*              FOR FGDLLTXT (PUT LONG TEXT DOWNLOAD FIELD)            *         
*                  - TEMP    = TEXT DATA UPTO 200 CHARACTERS          *         
*                  - BYTE2 = LENGTH OF TEXT DATA                      *         
***********************************************************************         
         SPACE 1                                                                
FGDLTXT  LR    R0,RE               SAVE RETURN REGITER                          
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         B     FGDLPUT                                                          
*                                                                               
FGDLNUM  LR    R0,RE                                                            
         MVI   DLCBTYP,DLCBNUM     NUMERIC DATA                                 
         CLC   DLCBFLD,SPACES      ANY DATA                                     
         BH    FGDLPUT                                                          
         MVI   DLCBTYP,DLCBTXT     MOVE IN "" IF NOT APPLICABLE                 
         B     FGDLPUT                                                          
*                                                                               
FGDLLTXT LR    R0,RE               BUILD LONG TEXT FIELD                        
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         SR    RF,RF                                                            
         ICM   RF,1,BYTE2                                                       
         BZ    FGDLPUT                                                          
         CHI   RF,L'DLCBFLD                                                     
         BH    FGLTXT02                                                         
         MVC   DLCBFLD,TEMP        MOVE DATA TO DOWNLOAD FIELD                  
         B     FGLTXT04                                                         
*                                                                               
FGLTXT02 CHI   RF,L'DUMLIN1-4      MAKE SURE IT'S NOT TOO LONG                  
         BH    FGLTXT06            YES - DON'T CALL DLFLD                       
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD                           
         MVC   DLCBFLX,TEMP        MOVE DATA TO EXTENDED FIELD                  
                                                                                
FGLTXT04 STC   RF,DLCBLEN          SET LENGTH                                   
         B     FGDLPUT                                                          
*                                                                               
FGLTXT06 LA    RE,TEMP             HANDLE LONG FIELD                            
FGLTXT08 CLI   0(RE),C'"'          TEST IF (EOTCHR) DEFINED                     
         BNE   *+8                                                              
         MVI   0(RE),C''''         YES REPLACE BY ALTERNATE CHAR                
         LA    RE,1(,RE)                                                        
         BCT   RF,FGLTXT08                                                      
*                                                                               
         L     RE,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         AH    RE,DLCBNUMC         RE=A(NEXT AVAIL CHR IN PRINT LINE)           
         MVI   0(RE),C'"'                                                       
         IC    RF,BYTE2                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,RE),TEMP        MOVE IN FIELD                                
         LA    RE,1+1(RF,RE)       END OF FIELD                                 
         MVI   0(RE),C'"'                                                       
         L     RF,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         SR    RE,RF                                                            
         LA    RE,2(,RE)           TOTAL LENGTH FROM START OF LINE              
         L     RF,DLCBTOTC         BUMP TOTAL NUMBER OF CHARS                   
         AR    RF,RE                                                            
         ST    RF,DLCBTOTC                                                      
         SRDL  RE,32                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         STH   RE,DLCBNUMC         BUMP NUM OF CHARS THIS LINE                  
         MVC   DLCBNUMF,=H'1'      BUMP NUM OF FIELDS THIS LINE                 
         L     RE,DLCBTOTL         BUMP TOTAL NUMBER OF LINES                   
         AR    RE,RF                                                            
         ST    RE,DLCBTOTL                                                      
         MHI   RF,L'DUMLIN1                                                     
         L     RE,DLCBAPL          BUMP NUM OF DOWNLOAD PRINT LINES             
         AR    RE,RF                                                            
         ST    RE,DLCBAPL                                                       
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
*                                                                               
FGDLPUT  MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* UPDATE TSAR RECORDS FROM GRID                                       *         
*  - UNPROTECT SCREEN FIRST                                           *         
*  - DATA COMES FROM PC IN THE FOLLOWING FORMAT                       *         
*    TSAR 6 CHARS/COLUMN ID 2 CHARS/ROW 1 CHAR/LENGTH 3 CHARS/DATA    *         
*    SEMI COLON DENOTES END OF RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
UPDGRID  TM    TWAMODE3,TWAM3INT   HAVE WE BEEN HERE BEFORE                     
         BZ    UGRID100            NO - CLEAR AND UNPROTECT SCREEN              
*                                                                               
         LA    R2,GRDDAT1          START OF SCREEN                              
         LA    R4,L'GRDDAT1        LENGTH OF LINE ON SCREEN                     
         B     UGRID10                                                          
         USING DISTABD,R3                                                       
UGRID02  CLI   0(R2),X'5E'         HAVE WE REACHED END OF THIS RECORD           
         BNE   UGRID12             NO - READ FOR NEXT DATA                      
                                                                                
         GOTO1 GETDATA,DMCB,(1,(R2)),(R4)                                       
                                                                                
UGRID04  TM    UPDIND2,UPDIREC     HAVE WE CHANGED ANY RECORD DATA              
         BZ    UGRID08             NO                                           
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO2Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO2Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOPUT+IOACCMST+IO1Q                                           
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    UGRID06                                                          
*&&UK                                                                           
         MVC   AIOBUFF,AIOBUFF1                                                 
         GOTO1 VPROMOTE,DMCB,AIOBUFF,ACOM                                       
         MVC   TSARDADR,DMCB+8                                                  
         NI    TSARRSTA,X'FF'-TRNSARCH                                          
*&&                                                                             
*&&US*&& DC    H'0'                                                             
         B     UGRID08                                                          
*                                                                               
UGRID06  GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
UGRID08  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
*                                                                               
UGRID10  ST    R2,SAVER3                                                        
         ST    R4,SAVERF                                                        
         GOTO1 GETDATA,DMCB,(6,(R2)),(R4)                                       
         OC    TEMP(6),TEMP                                                     
         BZ    UGRID100                                                         
         L     R2,SAVER3                                                        
         L     R4,SAVERF                                                        
         CLI   0(R2),C':'          HAVE WE REACHED END                          
         BE    UGRID200            YES                                          
         GOTO1 GETDATA,DMCB,(6,(R2)),(R4)                                       
         PACK  DUB,TEMP(6)                                                      
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         MVC   HALF,FULL+2                                                      
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    UPDIND2,X'FF'-UPDIREC                                            
UGRID12  GOTO1 GETDATA,DMCB,(2,(R2)),(R4)                                       
         GOTO1 VHEXIN,DMCB,TEMP,BYTE,2                                          
         GOTO1 GETDATA,DMCB,(1,(R2)),(R4)                                       
         PACK  DUB,TEMP(1)                                                      
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         MVC   CHAR3,FULL+3        STORE ROW NUMBER OF UPDATE DATA              
         GOTO1 GETDATA,DMCB,(3,(R2)),(R4)                                       
         PACK  DUB,TEMP(3)                                                      
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         MVC   BYTE2,FULL+3        STORE LENGTH OF UPDATE DATA                  
         CLI   BYTE2,0                                                          
         BE    UGRID14                                                          
         GOTO1 GETDATA,DMCB,(BYTE2,(R2)),(R4)                                   
UGRID14  OI    TWAMODE2,TWAM2CHG                                                
         L     R3,ADISTAB                                                       
         USING DISTABD,R3                                                       
UGRID16  CLI   DISCOL,EOT                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DISCOL,BYTE                                                      
         BE    UGRID18                                                          
         SR    RF,RF                                                            
         IC    RF,DISLN            LENGTH OF COLUMN ENTRY                       
         AR    R3,RF                                                            
         B     UGRID16                                                          
*                                                                               
UGRID18  OC    DISADDR,DISADDR     IF THIS COLUMN A SUBSTITUTE ONE              
         BNZ   UGRID20             NO - SOMETHING PRESENT                       
         GOTO1 ASUBDIS             GO READ FOR SUBSTITUTE COLUMN                
         BE    *+6                                                              
         DC    H'0'                DUMP IF NO SUBSTITUTE FOUND                  
UGRID20  OC    TSARIND3,DISOTSAR   ADD INDICATORS TO TSAR REC                   
         OC    DISDDATA,DISDDATA                                                
         BZ    UGRID22             DATA NOT HELD ON TSARREC                     
         MVC   LAREADDR,DISDDATA                                                
         EX    0,LARE              RE=A(DATA)                                   
         B     UGRID44                                                          
*                                                                               
UGRID22  TM    UPDIND2,UPDIREC     HAVE WE ALREADY GOT THIS RECORD              
         BNZ   UGRID24             YES                                          
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    UPDIND2,UPDIREC                                                  
UGRID24  L     R5,AIOBUFF                                                       
         USING TRNRECD,R5                                                       
         LA    RF,TRNRFST          RF=A(FIRST ELEMENT)                          
         USING NOTELD,RF                                                        
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,CHAR3            R1=ROW NUMBER DATA IS ON                     
*        SHI   R1,1                R1=ELEMENTS TO SKIP                          
UGRID26  CLI   NOTEL,0             END OF RECORD                                
         BE    UGRID36             YES - THEN MUST ADD ELEMENT                  
         CLC   NOTEL,DISELEQ       DOES IT MATCH ELEMENT ON TABLE               
         BNE   UGRID30             NO                                           
UGRID28  TM    DISCIND3,DISI3MUL   DOES COLUMN HAVE MULTIPLE ROWS               
         BZ    UGRID32             NO - PUT OUT EMPTY COLUMN                    
         LTR   R1,R1               HAVE WE REACH CORRECT ELEMENT                
         BZ    UGRID32             YES - PUT OUT DATA FROM THIS ELEMENT         
         SHI   R1,1                SUBTRACT COUNT AND GO TO NEXT EL             
UGRID30  IC    RE,NOTLN            BUMP TO NEXT ELEMENT                         
         AR    RF,RE                                                            
         B     UGRID26                                                          
*                                                                               
UGRID32  SR    R1,R1                                                            
         IC    R1,DISEDISP         R1=DISPLACEMENT ON ELEMENT TO DATA           
         LA    RE,NOTEL            RF=A(CURRENT ELEMENT)                        
         AR    RE,R1               RE=A(DATA TO BE REPLACED)                    
         TM    DISCIND4,DISI4MST   MASTER IN GROUP                              
         BZ    UGRID34             NO                                           
         CLI   BYTE2,0             YES - IF DATA LENGTH ZERO                    
         BNE   UGRID34             NO                                           
         MVI   NOTEL,X'FF'         YES - DELETE ELEMENT FROM RECORD             
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),(X'FF',AIOBUFF),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         B     UGRID02                                                          
UGRID34  TM    DISCIND4,DISI4VAR   IS DATA VARIABLE IN LENGTH                   
         BZ    UGRID44             NO                                           
         MVC   ELEMT,NOTEL      COPY EXISTING ELEMENT                           
         LA    RE,ELEMT                                                         
         AR    RE,R1               RF=A(DATA TO BE REPLACE ON NEW EL)           
         SR    R5,R5                                                            
         CLI   BYTE2,0             ARE WE DELETING DATA                         
         BE    UGRID35             YES                                          
         IC    R5,BYTE2            R5=LENGTH OF DATA INPUT                      
         SHI   R5,1                                                             
         EX    R5,*+4                                                           
         MVC   0(0,RE),TEMP        MOVE DATA INTO ELEMENT                       
         AHI   R5,1                                                             
UGRID35  AR    R5,R1               R5=NEW LENGTH OF ELEMENT                     
         STC   R5,ELEMT+1                                                       
         GOTO1 AREPELEM,DMCB,AIOBUFF,(RF),ELEMT    REPLACE ELEMENT              
         B     UGRID02                                                          
*                                                                               
UGRID36  MVC   ELEMT(L'DISELEQ),DISELEQ  MOVE IN ELEMENT CODE                   
         MVC   ELEMT+1,DISELEN  MOVE IN STANDARD LENGTH                         
         LA    RF,ELEMT                                                         
         SR    R1,R1                                                            
         IC    R1,DISEDISP                                                      
         AR    RF,R1                                                            
         TM    DISCIND2,DISI2DTE                                                
         BZ    UGRID38                                                          
         GOTO1 VDATCON,DMCB,(0,TEMP),(DISDFORM,(RF))                            
         B     UGRID42                                                          
*                                                                               
UGRID38  TM    DISCIND4,DISI4VAR   IS DATA VARIABLE IN LENGTH                   
         BZ    UGRID40             NO                                           
         SR    R5,R5                                                            
         IC    R5,BYTE2            LENGTH OF DATA                               
         SHI   R5,1                                                             
         EX    R5,*+4                                                           
         MVC   0(0,RF),TEMP        MOVE DATA INTO ELEMENT                       
         AHI   R5,1                                                             
         AR    R5,R1               R5=NEW LENGTH OF ELEMENT                     
         STC   R5,ELEMT+1                                                       
*                                                                               
UGRID40  SR    RE,RE                                                            
         IC    RE,DISDLEN                                                       
         SHI   RE,1                SUBTRACT 1 FOR EX                            
         EX    RE,*+4                                                           
         MVC   0(0,RF),TEMP        MOVE COLUMN DATA                             
UGRID42  LA    RF,ELEMT                                                         
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,(RF),=C'ADD=CODE'             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         B     UGRID02                                                          
*                                                                               
UGRID44  LR    RF,RE               RF=A(DATA)                                   
         ST    RF,SAVERF                                                        
         SR    RE,RE                                                            
         IC    RE,DISDLEN          LENGTH OF DATA                               
         TM    DISCIND2,DISI2DTE                                                
         BNZ   UGRID48             DATE DATA                                    
         TM    DISCIND2,DISI2CKB                                                
         BNZ   UGRID54             CHECKBOX DATA                                
         SHI   RE,1                SUBTRACT 1 FOR EX                            
         CLI   BYTE2,0             LENGTH                                       
         BNE   UGRID46                                                          
         EX    RE,*+4                                                           
         XC    0(0,RF),0(RF)                                                    
         B     UGRID02                                                          
UGRID46  EX    RE,*+4                                                           
         MVC   0(0,RF),TEMP        MOVE COLUMN DATA                             
         B     UGRID02                                                          
*                                                                               
UGRID48  CLI   BYTE2,0             LENGTH                                       
         BNE   UGRID52                                                          
         CLI   DISDFORM,DISDFRM1   DATE IS PWOS FORMAT                          
         BNE   UGRID50                                                          
         XC    0(3,RF),0(RF)       CLEAR DATE                                   
         B     UGRID02                                                          
UGRID50  CLI   DISDFORM,DISDFRM2   DATE IS COMPRESSED                           
         BE    *+6                                                              
         DC    H'0'                DIE IS OTHER TYPE OF FORMAT                  
         XC    0(2,RF),0(RF)       CLEAR DATE                                   
         B     UGRID02                                                          
*                                                                               
UGRID52  DS    0H                                                               
         MVI   BYTE,0              USA FORMAT (EX MM/DD/YY)                     
         OI    BYTE,X'60'                                                       
         OC    TEMP(3),=X'404040'    UPPER CASE MMM IN MMMDD/YY FORMAT          
         GOTO1 VPERVAL,DMCB,(BYTE2,TEMP),(BYTE,WORK)                            
         USING PERVALD,R1                                                       
         LA    R1,WORK                                                          
         CLI   DMCB+4,PVRCMISS                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB+4,PVRCINV1                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,SAVERF                                                        
         MVC   0(2,RF),PVALCSTA                                                 
*        GOTO1 VDATCON,DMCB,(0,TEMP),(DISDFORM,(RF))                            
         B     UGRID02                                                          
         DROP  R1                                                               
*                                                                               
UGRID54  CLC   TEMP(2),=C'-1'      HAVE THEY MARKED COLUMN                      
         BNE   UGRID56                                                          
         OI    TSARINDS,TSARMKQ                                                 
UGRID56  CLI   TEMP,C'0'           HAVE THEY UNMARKED COLUMN                    
         BNE   UGRID02                                                          
         NI    TSARINDS,X'FF'-TSARMKQ                                           
         B     UGRID02                                                          
*                                                                               
UGRID100 LA    R2,GRDDAT1H         CLEAR SCREEN AND UNPROTECT FIELDS            
         USING FLDHDRD,R2                                                       
         LA    RF,GRDDATLH         LAST FIELD ON SCREEN                         
         SR    RE,RE                                                            
         SR    R1,R1                                                            
UGRID102 IC    RE,FLDLEN                                                        
         LR    R1,RE                                                            
         SH    R1,=Y(FLDDATA-FLDLEN) R1=INPUT DATA LENGTH                       
         BZ    UGRID104                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    FLDDATA(0),FLDDATA  CLEAR DATA IN FEILDS                         
         NI    FLDATB,X'FF'-FATBPROT  REMOVE PROTECTION                         
         BXLE  R2,RE,UGRID102                                                   
*                                                                               
UGRID104 OI    TWAMODE3,TWAM3INT   SET INITIALISED                              
         LA    R2,MRKMSGH                                                       
         NI    FLDATB,X'FF'-FATBPROT  REMOVE PROTECTION                         
         XC    MRKMSG,MRKMSG                                                    
         MVC   MRKMSG(16),=C'<UNPROTECT=ROW4>'                                  
                                                                                
UGRIDNX  B     ROU4H                                                            
*                                                                               
UGRID200 OI    TWAMODE3,TWAM3UPD   SET UPDATE FINISHED                          
         LA    R2,MRKMSGH                                                       
         NI    TWAMODE3,X'FF'-TWAM3INT  REMOVE THE INITIALISED BIT              
         XC    MRKMSG,MRKMSG                                                    
         MVC   MRKMSG(10),=C'<COMPLETE>'   SET MESSAGE FOR PC                   
         OI    MRKMSGH+(FVOIND-FVIHDR),FVOXMT  SET TRANSMIT                     
         OI    FLDATB,FATBPROT     SET PROTECTED                                
         L     R1,ATSARBLK         SAVE TSAR BUFFER ON DISK                     
         USING TSARD,R1                                                         
         OC    TSABUF,TSABUF       TEST WE HAVE A TSAR BUFFER                   
         BZ    UGRIDYX             NO                                           
         MVI   TSACTN,TSASAV       YES - SAVE IT                                
         GOTO1 VTSAR                                                            
         DROP  R1                                                               
UGRIDYX  B     ROU4E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM SCREEN AND PUT TO TEMP SO IT CAN BE READ FOR THE      *         
* UPDATE PROCESS                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETDATA  NTR1  ,                                                                
         XC    TEMP,TEMP                                                        
         LA    RE,TEMP             R6=A(TEMPORARY STORAGE FOR DATA)             
         SR    R5,R5                                                            
         IC    R5,0(R1)            LENGTH OF DATA                               
         L     R2,0(R1)            ADDRESS OF START OF DATA                     
         L     R4,4(R1)            CURRENT LENGHT OF LINE LEFT                  
         LR    R3,R4               R3=CURRENT LENGTH LEFT                       
         SR    R4,R5               HAVE WE GOT ENOUGH SPACE ON LINE             
         BP    GTDATA20            YES                                          
*                                  NO - GET DATA FROM NEXT LINE                 
GTDATA10 LR    R4,R3               R4=REMAINING LENGTH ON LINE                  
         LA    R3,L'GRDDAT1        R3=MAX LENGTH OF EACH LINE                   
         SHI   R4,1                                                             
         EX    R4,*+4                                                           
         MVC   0(0,RE),0(R2)       MOVE FIRST BIT OF DATA FROM EXISTING         
         AHI   R4,1                                       LINE                  
         LA    R2,0(R4,R2)         R2=A(NEXT LINE OF DATA)                      
         LA    R1,GRDDATL+L'GRDDATL                                             
         CR    R1,R2                                                            
         BNH   GTDATX                                                           
         LA    R2,L'FVIHDR(R2)     R2=A(NEXT LINE OF DATA)                      
         LA    RE,0(RE,R4)         ADJUST DATA STORAGE                          
         SR    R5,R4               R5=REMAINING LENGHT OF DATA TO MOVE          
         LR    R4,R3               R4=RESET LENGTH OF EACH LINE                 
         SR    R4,R5               IS DATA LONGER THAN LINE                     
         BM    GTDATA10            YES                                          
GTDATA20 SHI   R5,1                                                             
         EX    R5,*+4                                                           
         MVC   0(0,RE),0(R2)       MOVE REST OF DATA INTO TEMP                  
         AHI   R5,1                                                             
         AR    R2,R5               R2=A(NEXT BIT OF DATA)                       
GTDATX   XIT1  REGS=(R2,R4)                                                     
         EJECT                                                                  
***********************************************************************         
* REPLACE EXISTING ELEMENTS ON A RECORD                               *         
* PARM1 - ADDRESS OF RECORD                                           *         
* PARM2 - ADDRESS OF ELEMENT TO BE REPLACE                            *         
* PARM3 - ADDRESS OF NEW ELEMENT TO BE ADDED                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R4                                                        
OLD      USING TRNELD,R3                                                        
REPELEM  LM    R2,R4,0(R1)         R2=A(RECORD) R3=A(CURRENT ELEM)              
         LR    R0,R2               R0=A(OLD RECORD)                             
         L     RE,AIOBUFF2         RE=A(NEW RECORD)                             
         LA    RF,OLD.TRNELD                                                    
         SR    RF,R0               RF=LENGTH OF RECORD TO COPY                  
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY PART OF RECORD                          
NEW      USING TRNRECD,RE                                                       
         LR    R1,RE                                                            
         AR    R1,RF               R1=A(NEW ELEMENT ON RECORD)                  
         SR    RF,RF                                                            
         IC    RF,TRNLN            RF=LENGTH OF NEW ELEMENT                     
         SHI   RF,1                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),TRNELD      MOVE IN NEW ELEMENT                          
         LA    R1,1(R1,RF)         NEXT PLACE FOR ELEMENT                       
         IC    RF,OLD.TRNLN                                                     
         AR    R3,RF               R3=A(NEXT ELEMENT ON OLD RECORD)             
         DROP  R4                                                               
*                                                                               
REPEL10  CLI   OLD.TRNEL,0                                                      
         BE    REPEL20                                                          
         SR    RF,RF                                                            
         IC    RF,OLD.TRNLN                                                     
         SHI   RF,1                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),OLD.TRNEL                                                
         AHI   RF,1                                                             
         AR    R1,RF               R1=A(NEXT ELEMENT ON NEW REC)                
         AR    R3,RF               R3=A(NEXT ELEMENT ON OLD REC)                
         B     REPEL10                                                          
         DROP  OLD                                                              
                                                                                
REPEL20  MVI   0(R1),0                                                          
         LR    R0,R2                                                            
         L     RE,AIOBUFF2                                                      
         SR    R1,RE                                                            
         AHI   R1,1                ADD ONE FOR X'00' AT EOR                     
         STCM  R1,3,NEW.TRNRLEN    SET LENGTH ON NEW RECORD                     
         LR    RF,R1               LENGTH OF RECORD TO MOVE                     
         MVCL  R0,RE               MOVE RECORD BACK TO ORIGINAL BUFFER          
REPELX   B     ROU4X                                                            
         DROP  NEW                                                              
***********************************************************************         
*        DISPLAY DUMMY SCREEN LINES ON SCREEN                         *         
*                                                                     *         
* FOR BUILDING GRID: FULL  - A(FIRST SPACE AVAIL. ON CURRENT LINE)    *         
*                    BYTE2 - LENGHT OF SPACES AVAIL. ON CURRENT LINE  *         
*                    WORK  - SAVED DATA FOR CURRENT LINE              *         
***********************************************************************         
         SPACE 1                                                                
DISPDUM  MVI   BYTE2,0                                                          
         NI    PCDRIVEN,X'FF'-PCGSCRQ                                           
         SR    RF,RF                                                            
         ICM   RF,1,LINSUSED       RF=(NUMBER OF LINES NEEDED)                  
         BZ    DISPX                                                            
         MHI   RF,L'DUMLIN1        RF=(NUMBER OF CHARS TO BE DISPLAYED)         
         BCTR  RF,0                                                             
         L     RE,ADUMLINE                                                      
*                                                                               
DISP01   CLI   0(RE),C'^'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          TRANSLATE ALL '^' TO SPACE                   
         LA    RE,1(,RE)                                                        
         BCT   RF,DISP01                                                        
*                                                                               
         CLC   DISSTART,DISLINE    TEST FIRST LINE?                             
         BE    DISP06                                                           
*                                                                               
* FILL UP ALL SPACES AT THE END OF CURRENT GRID SCREEN LINE.                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DISLINE          RF=(NEXT FREE LINE NUMBER)                   
         BCTR  RF,0                RF=(CURRENT LINE NUMBER)                     
         MH    RF,=Y(GRDDAT2H-GRDDAT1H)                                         
         LA    R5,GRDDAT1H(RF)     R5=(CURRENT LINE)                            
         LA    R5,L'GRDDAT1H+L'GRDDAT1-1(,R5)                                   
         LA    RF,1(,R5)           RF=(END OF CURRENT LINE)                     
         CLI   0(R5),C' '          FIND END OF DATA IN CURRENT LINE             
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(,R5)           R5=(END OF DATA)                             
         ST    R5,FULL                                                          
         SR    RF,R5               RF=(NUMBER OF SPACES AT THE END)             
         CHI   RF,2                TEST IF THERE IS ENOUGH SPACES?              
         BL    DISP06              NO - PUT IT INTO NEXT AVAIL. LINE            
         STC   RF,BYTE2            BYTE2=(LENGTH OF EMPTY SPACE)                
         MVC   WORK,SPACES                                                      
         L     RE,ADUMLINE                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   WORK(0),0(RE)       SAVE DATA FOR CURRENT LINE                   
         LA    RE,1(RF,RE)                                                      
         SR    RF,RF                                                            
DISP02   CLC   0(L'DUMLIN1,RE),SPACES                                           
         BE    DISP04                                                           
         LA    RF,1(,RF)                                                        
         LA    RE,L'DUMLIN1(,RE)                                                
         B     DISP02                                                           
DISP04   STC   RF,LINSUSED         RF=(NEW NUMBER OF LINES NEEDED)              
         OI    PCDRIVEN,PCGSCRQ    BUILD GRID SCREEN LINES                      
*                                                                               
DISP06   SR    RF,RF                                                            
         IC    RF,DISLINE          RF=(NEXT FREE LINE NUMBER)                   
         LR    RE,RF               RE=RF                                        
         SR    R0,R0                                                            
         ICM   R0,1,LINSUSED       R0=(NUMBER OF LINES NEEDED)                  
         BNZ   *+12                                                             
         TM    PCDRIVEN,PCGSCRQ    TEST BUILD GRID SCREEN LINES?                
         BZ    DISPX                                                            
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         AR    RE,R1               RE=(PROJECTED SCREEN END LINE)               
         SR    R1,R1                                                            
         IC    R1,DISEND           R1=(MAX SCREEN END LINE)                     
         CR    RE,R1               COULD WE FIT ITEM ON SCREEN?                 
         BH    DISPFULL                                                         
         LA    RE,1(RE)                                                         
         STC   RE,DISLINE          UPDATE NEXT FREE LINE NUMBER                 
         MH    RF,=Y(GRDDAT2H-GRDDAT1H)                                         
         LA    R2,GRDDAT1H(RF)     GRID SCREEN DATA LINE                        
         USING FLDHDRD,R2                                                       
         L     RF,ADUMLINE                                                      
         TM    PCDRIVEN,PCGSCRQ    TEST BUILD GRID SCREEN LINES?                
         BZ    DISP10                                                           
         SR    RE,RE                                                            
         ICM   RE,1,BYTE2          LENGTH OF DATA FOR CURRENT GRID LINE         
         BZ    DISP10                                                           
         L     R5,FULL             A(END OF DATA IN CURRENT LINE)               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R5),WORK        MOVE DATA TO THE END OF CURRENT LINE         
         ICM   R0,1,LINSUSED       R0=(NUMBER OF LINES NEEDED)                  
         BZ    DISPX                                                            
         LA    RF,1(RE,RF)         A(DATA FOR NEXT AVAIL. SCREEN LINE)          
*                                                                               
DISP10   MVC   FLDDATA(L'DUMLIN1),0(RF) PUT DUMMY SCREEN LINE OF SCREEN         
         OI    FLDOIND,FOUTTRN                                                  
         NI    FLDATB,X'FF'-FATBHIGH                                            
         LA    RF,L'DUMLIN1(RF)                                                 
         LA    R2,GRDDAT2H-GRDDAT1H(R2)                                         
         BCT   R0,DISP10                                                        
         B     DISPX                                                            
*                                                                               
DISPX    B     ROU4E                                                            
*                                                                               
DISPFULL B     ROU4H                                                            
         EJECT                                                                  
***********************************************************************         
*        CLEAR DATA LINES ON SCREEN                                   *         
*        ON ENTRY R1=A(LINE NUMBER TO START CLEARING FROM)            *         
***********************************************************************         
         SPACE 1                                                                
         USING FLDHDRD,R2                                                       
SCRNCLR  SR    R2,R2               CLEAR FROM LINE COUNT ONWARDS                
         ICM   R2,1,0(R1)                                                       
         BNZ   SCRNC02                                                          
         LA    R2,GRDDAT1H                                                      
         B     SCRNC08                                                          
*                                                                               
SCRNC02  SHI   R2,1                                                             
         MH    R2,=Y(GRDDAT3H-GRDDAT2H)                                         
         LA    R2,GRDDAT2H(R2)                                                  
         B     SCRNC08                                                          
*                                                                               
SCRNC08  LA    RF,GRDDATLH                                                      
*                                                                               
SCRNC09  CR    R2,RF                                                            
         BH    SCRNCLRX                                                         
         SR    RE,RE                                                            
         SR    R1,R1                                                            
SCRNC10  IC    RE,FLDLEN                                                        
         LR    R1,RE                                                            
         SH    R1,=Y(FLDDATA-FLDLEN) R1=INPUT DATA LENGTH                       
         BZ    SCRNC20                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    FLDDATA(0),FLDDATA                                               
*                                                                               
SCRNC20  OI    FLDOIND,FOUTTRN                                                  
         NI    FLDATB,X'FF'-FATBHIGH                                            
         OI    FLDATB,FATBPROT     SET PROTECTION                               
         BXLE  R2,RE,SCRNC10                                                    
*                                                                               
SCRNCLRX B     ROU4X                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET SCREEN DIMENSIONS                                    *         
* ON NTRY PARAM 1 BYTE 0 NUMBER OF PERMANENT TOTAL LINES              *         
*         PARAM 1 BYTES 1-3 A(FIRST AVAILABLE LINE)                   *         
***********************************************************************         
         SPACE 1                                                                
SCRNDIM  MVC   BYTE,0(R1)          NUMBER OF TOTAL LINES                        
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)          RF=A(NEXT AVAILABLE SCREEN LINE)             
*                                                                               
         LA    RE,GRDDAT1H         GRID SCREEN LINE                             
         SR    RF,RE                                                            
         XR    R1,R1                                                            
         IC    R1,FLDLEN-FLDHDRD(RE) R1=TOTAL SCREEN FIELD LENGTH               
         XR    RE,RE                                                            
         DR    RE,R1                                                            
         STC   RF,DISSTART         START POSITION FOR DATA DISPLAY              
         STC   RF,DISLINE          LAST LINE USED                               
         LA    RF,GRDDATLH         GRID DATA LINES                              
         LA    RE,GRDDAT1H                                                      
         SR    RF,RE                                                            
         XR    RE,RE                                                            
         DR    RE,R1                                                            
         XR    R2,R2                                                            
         IC    R2,BYTE                                                          
         SR    RF,R2               REDUCE BY NUMBER OF TOTAL LINES              
         STC   RF,DISEND           MAXIMUM LINES FOR DATA DISPLAY               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DISEND                                                        
         XR    RE,RE                                                            
         IC    RE,DISSTART                                                      
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,MAXLINES         MAX LINES ON SCREEN                          
*                                                                               
         B     ROU4X                                                            
         EJECT                                                                  
***********************************************************************         
*        TEST GRID COLUMN NEEDED                                      *         
* ON ENTRY     R3=A(CURRENT COLUMN IN THE TABLE)                      *         
***********************************************************************         
         SPACE 1                                                                
         USING DISTABD,R3                                                       
TSTCOL   TM    DISCIND,DISCIGRD    TEST NOT GRIDS                               
         BNZ   TSTCOLN                                                          
         TM    DISIND2,DISMULT     CHECK DEALING WITH MULTIPLE ROWS             
         BZ    TCOL01              NO                                           
         TM    DISCIND3,DISI3MUL   CHECK COLUMN IS A MULTIPLE ONE               
         BZ    TSTCOLN             NO - REJECT COLUMN                           
*                                                                               
TCOL01   MVC   THREE,DISVACM       BUT TEST VALIDITY                            
         NC    THREE,XACTOPEV      TEST ELEMENT VALID FOR THIS ACTION           
         CLC   THREE,XACTOPEV                                                   
         BNE   TSTCOLN                                                          
         TM    DISCIND,DISCI2CQ    TEST SECONDARY CURRENCY COLUMN               
         BZ    TCOL02                                                           
*&&UK*&& OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY SET                  
*&&UK*&& BZ    TSTCOLN                                                          
TCOL02   CLI   DISLN,DISTABL       ANY LEDGERS IN THE LIST?                     
         BNH   TSTCOLY             NO - WE WANT IT THEN                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DISLN            LENGTH TABLE ENTRY                           
         SHI   RF,DISTABL          LENGTH OF LEDGER LIST                        
         SRL   RF,1                NUMBER OF LEDGERS IN THE LIST                
         LA    R1,DISLDG           LIST OF LEDGERS                              
*                                                                               
TCOL06   CLI   1(R1),C' '          TEST UNIT ONLY                               
         BNE   TCOL08                                                           
         CLC   0(1,R1),ACCOUNT+1   YES - COMPARE UNIT                           
*        CLC   0(1,R1),SUPPUL      YES - COMPARE UNIT                           
         BE    TCOL10                                                           
         B     *+14                                                             
TCOL08   CLC   0(L'DISLDG,R1),ACCOUNT+1                                         
*COL08   CLC   0(L'DISLDG,R1),SUPPUL                                            
         BE    TCOL10                                                           
         LA    R1,L'DISLDG(,R1)                                                 
         BCT   RF,TCOL06                                                        
*                                  LEDGER NO FOUND IN THE LIST                  
         TM    DISCIND,DISCIXLG    EXCLUDE THESE UNIT/LEDGER                    
         BO    TSTCOLY                                                          
         B     TSTCOLN                                                          
*                                  LEDGER FOUND                                 
TCOL10   TM    DISCIND,DISCIXLG    EXCLUDE THESE UNIT/LEDGER                    
         BO    TSTCOLN                                                          
*                                                                               
TSTCOLY  B     ROU4E                                                            
*                                                                               
TSTCOLN  B     ROU4H                                                            
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*        FIND SUBSTITUTE VALUES FOR COLUMN                            *         
* ON ENTRY     R3=A(CURRENT COLUMN IN THE TABLE DISTAB)               *         
***********************************************************************         
         SPACE 1                                                                
         USING DISTABD,R4                                                       
SUB      USING DISTABD,R3                                                       
SUBDIS   LR    R4,R3                                                            
         L     R3,ASUDTAB                                                       
SUBD02   CLI   SUB.DISCOL,EOT                                                   
         BE    SUBDISN                                                          
         CLC   SUB.DISCOL,DISCOL                                                
         BNE   SUBD10                                                           
         MVC   THREE,SUB.DISVACM                                                
         NC    THREE,XACTOPEV      TEST ELEMENT VALID FOR THIS ACTION           
         CLC   THREE,XACTOPEV                                                   
         BE    SUBDISY             YES VALID                                    
*                                                                               
SUBD10   SR    RF,RF                                                            
         IC    RF,SUB.DISLN        LENGTH OF COLUMN ENTRY                       
         AR    R3,RF                                                            
         B     SUBD02                                                           
*                                                                               
SUBDISY  B     ROU4ER3                                                          
*                                                                               
SUBDISN  B     ROU4H                                                            
         DROP  R4,SUB                                                           
         EJECT                                                                  
**********************************************************************          
*        GET MARKER ID NUMBER                                        *          
* ON EXIT P1=(ACTION TABLE ENTRY IF FOUND)                           *          
**********************************************************************          
         SPACE 1                                                                
GETMID   L     R3,AMIDTAB          ENQUIRY ID TABLE                             
         USING MIDTABD,R3                                                       
         SR    R0,R0                                                            
GEID02   OC    MIDACT,MIDACT       TEST END OF TABLE                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   THREE,MIDACT                                                     
         NC    THREE,XACTOPEV      TEST ELEMENT VALID FOR THIS ACTION           
         CLC   THREE,XACTOPEV                                                   
         BE    *+14                                                             
         ICM   R0,3,MIDLN                                                       
         AR    R3,R0               BUMP TO NEXT ENTRY                           
         B     GEID02                                                           
         DROP  R3                                                               
*                                                                               
         LA    R3,MIDLN1Q(,R3)     SUB ELEMENTS                                 
         USING MIDSUB,R3                                                        
GEID04   CLI   MIDCTRY,CTRYALLQ    TEST ALL COUNTRIES                           
         BE    GEID06                                                           
         CLC   MIDCTRY,AGYCTRY     MATCH ON COUNTRY?                            
         BNE   GEID14                                                           
*                                                                               
GEID06   SR    RF,RF                                                            
         ICM   RF,1,MIDNLDG        RF=NUMBER OF LEDGERS IN THE LIST             
         BZ    GETMIDX             OK - ALL LEDGERS                             
         LA    RE,MIDLEDG          LIST OF LEDGERS                              
GEID08   CLI   1(RE),C' '          TEST UNIT ONLY?                              
         BH    GEID10                                                           
         CLC   0(1,RE),ACCOUNT+1   YES - COMPARE UNIT                           
         BE    GETMIDX                                                          
         B     *+14                                                             
GEID10   CLC   0(L'MIDLEDG,RE),ACCOUNT+1                                        
         BE    GETMIDX                                                          
         LA    RE,L'MIDLEDG(,RE)   NEXT UNIT/LEDGER                             
         BCT   RF,GEID08                                                        
*                                                                               
GEID14   LA    RF,MIDSLNQ          LENGTH OF SUB-ELEM WITHOUT LDG LIST          
         SR    RE,RE                                                            
         ICM   RE,1,MIDNLDG        NUMBER OF LEDGERS IN SUB-ELEM                
         BZ    GEID16                                                           
         SLL   RE,1                LENGTH OF LEDGER LIST                        
         AR    RF,RE                                                            
GEID16   AR    R3,RF               BUMP TO NEXT SUB-ELEM                        
         B     GEID04                                                           
         DROP  R3                                                               
*                                                                               
GETMIDX  ST    R3,0(,R1)           RETURN A(ENQUIRY ID NUMBER)                  
         B     ROU4E               SET CC TO EQUAL                              
         EJECT ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACMRKWRK                                                       
       ++INCLUDE ACMRKCCD          CREDITOR/CHEQUE DSECT REQUIRED               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACMRK43   03/18/15'                                      
         END                                                                    
