*          DATA SET RESPL98A   AT LEVEL 064 AS OF 05/01/02                      
*PHASE T80898A,*                                                                
*INCLUDE RECUP                                                                  
         TITLE 'T80898 - RESPL98 - OVERNIGHT PROJECTIONS'                       
*                                                                               
***********************************************************************         
*                                                                     *         
*- RESPL98 -- OVERNIGHT PROJECTION CREATION/REPORT                    *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*  11/07/89  PJS  DO NOT USE 'PJVAL' SWITCH FOR 2 THINGS (SAVING      *         
*                 PROJECTION VALUE AND PROJECTION FOUND SWITCH)       *         
*                                                                     *         
*                 PGM WAS NOT CREATING PROJECTIONS IF T.V. HOMES      *         
*                 VALUE WAS 0  (LAST VALUE SAVED IN PJVAL BY 'FORMDEM'*         
*                                                                     *         
*  OCT26/90 (MRR) --- CHANGE DBLOCK AND PRINTING FOR 1 DECIMAL        *         
*                                                                     *         
*  DEC14/90 (MRR) --- SWITCH COMPARE OF PROJECTION FROM HOMES TO      *         
*                      RTG HOMES                                      *         
*                                                                     *         
*  DEC10/92 (BU ) --- CHANGE X'5E' ELEMENT VALUE FROM '5202' TO '520A'*         
*                                                                     *         
*  MAR29/95 (BU ) --- 'RECORD ON FILE' ERROR PROBLEM                  *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                               
T80898   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**OVPR**,R9                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T808FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         STM   R8,RC,OVERR8                                                     
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         MVI   RCSUBPRG,0                                                       
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REP                                                     
         DROP  R4                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REP,RREPPAR                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTODAY)                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         L     R2,=A(STACK)                                                     
         A     R2,RELO                                                          
         ST    R2,ASTACK                                                        
         L     R2,=A(PJBUFF)                                                    
         A     R2,RELO                                                          
         ST    R2,APJBUFF                                                       
         L     R2,=A(LYBUFF)                                                    
         A     R2,RELO                                                          
         ST    R2,ALYBUFF                                                       
         L     R2,=V(RECUP)                                                     
         A     R2,RELO                                                          
         ST    R2,VRECUP                                                        
         EJECT                                                                  
*              INITIALIZATION                                                   
         SPACE 3                                                                
         MVC   LASTBK,TO           SET UP LAST YEARS BOOK                       
         ZIC   R1,LASTBK+1                                                      
         BCTR  R1,0                BACK UP TO LAST YEAR                         
         STC   R1,LASTBK+1                                                      
         EDIT  (R1),(2,LASTCD)                                                  
         ZIC   R1,LASTBK+2                                                      
         BCTR  R1,0                                                             
         LA    R1,MONCOD(R1)       PICK UP MONTH CODE                           
         MVC   LASTCD(1),0(R1)     OVERWRITE FIRST CHARACTER                    
         SPACE 1                                                                
         L     R2,ACOMFACS         COMFACS ODDMENTS                             
         USING COMFACSD,R2                                                      
         SPACE 1                                                                
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         DROP  R1                                                               
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
         ST    R2,DBCOMFCS                                                      
         LA    R2,IO                                                            
         ST    R2,DBAREC                                                        
         LA    R2,34(R2)                                                        
         ST    R2,DBAQUART                                                      
         SPACE 1                                                                
         XC    DMCB(12),DMCB       NEED A (DEMUP)                               
         MVC   DMCB+4(4),=X'D9000A08'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VDEMUP,DMCB                                                      
         EJECT                                                                  
*              CONTROL THE IO ROUTINES                                          
         SPACE 3                                                                
         LA    R2,DPLIST                                                        
         LA    R3,NDPT                                                          
         SPACE 1                                                                
MAB      CLI   0(R2),0             CONTROL FOR EACH DP                          
         BE    MABX                                                             
         ST    R2,SAVER2                                                        
         BAS   RE,NEWDPT                                                        
         BAS   RE,MAD                                                           
         LA    R2,1(R2)                                                         
         BCT   R3,MAB                                                           
         B     MABX                                                             
         SPACE 1                                                                
MABX     CLI   TITDET,C'Y'         PRINT DETAILS OPTION                         
         BE    XIT                                                              
         MVI   RCSUBPRG,1                                                       
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   P5,SPACES                                                        
         MVC   P6,SPACES                                                        
         MVC   P(55),=C'***** OVERNIGHT PROJECTION SUCCESSFULLY COMPLETX        
               ED *****'                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
MAD      NTR1                      BUILD A STACK OF D/A                         
         L     R5,ASTACK                                                        
         SR    R6,R6                                                            
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,REP                                                     
         MVC   RIDPKSTA,ACTSTAT                                                 
         MVC   RIDPKDPT,0(R2)                                                   
         GOTO1 HIGH                                                             
         B     MAD4                                                             
         SPACE 1                                                                
MAD2     GOTO1 SEQ                                                              
         SPACE 1                                                                
MAD4     CLC   KEYSAVE(11),KEY     STATION D/P C/B                              
         BNE   MAD6                                                             
         CLC   RIDPKSTD,STRTOPT                                                 
         BL    MAD2                                                             
         CLC   RIDPKSTD,ENDOPT                                                  
         BH    MAD2                                                             
         MVC   0(4,R5),KEY+28                                                   
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         B     MAD2                                                             
         SPACE 1                                                                
MAD6     LTR   R6,R6                                                            
         BZ    XIT                                                              
         L     R5,ASTACK                                                        
         B     MA3                                                              
         SPACE 1                                                                
MA2      LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)                                                         
         BCT   R6,MA3                                                           
         B     XIT                                                              
         SPACE 1                                                                
MA3      MVC   KEY+28(4),0(R5)                                                  
         STM   R5,R6,SAVESTAK                                                   
         GOTO1 GETREC                                                           
         MVC   KEY(27),IO                                                       
         USING RINVKEY,R4                                                       
         SPACE 2                                                                
MA4      MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         LA    R1,RINVPFLT         FILTER FILTER                                
         LA    R5,TITFILT                                                       
         LA    R0,6                                                             
         SPACE 2                                                                
MA6      CLI   0(R5),C'A'          IF ANYTHING IS SPECIFIED                     
         BL    MA8                                                              
         CLC   0(1,R5),0(R1)       IT MUST MATCH                                
         BNE   MA2                                                              
         SPACE 2                                                                
MA8      LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,MA6                                                           
*                                  SEE IF IT ENDS BEFORE TODAY                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MA12                                                             
         CLC   RINVPEFF+2(2),CTODAY                                             
         BL    MA2                                                              
         SPACE 1                                                                
MA12     BAS   RE,HEADER           FORMAT INVENTORY DETAILS                     
         SPACE 1                                                                
         CLI   TITACT,C'Y'         Y = USE ACTUALS IF HIGHER                    
         BNE   MA16                                                             
         BAS   RE,LAST                                                          
MA16     BAS   RE,PROJECT                                                       
         BAS   RE,CHOOSE                                                        
         SPACE 1                                                                
         CLI   TITDET,C'Y'         Y = PRINT DETAILS                            
         BNE   MA2                                                              
         BAS   RE,SPLAT                                                         
         B     MA2                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT HEADER DETAILS                                 
         SPACE 3                                                                
HEADER   NTR1                                                                   
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   P5,SPACES                                                        
         MVC   P6,SPACES                                                        
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         GOTO1 UNDAY,DMCB,RINVPDAY,P+1                                          
         GOTO1 UNTIME,DMCB,RINVPTIM,P+10                                        
         ZIC   R5,RINVPLEN                                                      
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,DMCB,((R5),RINVPROG),(27,P+22),(C'P',3)                  
         EDIT  (1,RINVKQTR),(2,P+50),FILL=0                                     
         MVC   P+52(2),RINVKDAY                                                 
         CLI   P+53,C'0'                                                        
         BNE   *+8                                                              
         MVI   P+53,C' '                                                        
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,P+55)                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CHECK LAST YEAR'S RECORD                             
         SPACE 3                                                                
LAST     NTR1                                                                   
         XC    LYVAL,LYVAL                                                      
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
LAST3    CLC   FROM(1),3(RE)                                                    
         BE    LAST4                                                            
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   LAST3                                                            
         DC    H'0'                                                             
LAST4    MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
LAST5    MVC   RINVKBK,LASTBK+1    TACK ON LAST YEARS BOOK                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         MVC   KEY(27),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         LA    R4,IO               GOT A HIT LAST YEAR                          
         BAS   RE,GETHPT           MAKE SURE REC HAS OLD AND NEW HPT            
         MVC   RINVKBK,TO+1                                                     
         MVC   P+66(2),LASTCD                                                   
         LA    R3,P                                                             
         BAS   RE,FORMDEM                                                       
******   MVC   LYVAL,THISHMS                                                    
         MVC   LYVAL,THISRTG                                                    
         MVC   THISCD,LASTCD                                                    
         BAS   RE,MAINTCOD                                                      
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
LAST10   CLC   TO(1),3(RE)                                                      
         BE    LAST15                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   LAST10                                                           
         DC    H'0'                                                             
LAST15   MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
LAST20   L     R5,ALYBUFF                                                       
         MOVE  ((R5),1000),IO                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROJECT                                               
         SPACE 3                                                                
PROJECT  NTR1                                                                   
         XC    PJVAL,PJVAL                                                      
         XC    FOUNDPJ,FOUNDPJ     ASSUME NO PROJECTION                         
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
PROJ3    CLC   FROM(1),3(RE)                                                    
         BE    PROJ4                                                            
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   PROJ3                                                            
         DC    H'0'                                                             
PROJ4    MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
PROJ5    MVC   RINVKBK,FROM+1      TRY AND READ FROM BOOK                       
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         MVC   KEY(27),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE                                                                  
         MVI   FOUNDPJ,X'FF'       PROJECTION FOUND                             
         SPACE 1                                                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'PROJ FOUND'                                           
         MVC   P+13(27),KEYSAVE                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST END                                                                    
*                                                                               
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         BAS   RE,GETHPT           MAKE SURE RECORD HAS OLD AND NEW HPT         
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
PROJ10   CLC   TO(1),3(RE)                                                      
         BE    PROJ15                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   PROJ10                                                           
         DC    H'0'                                                             
PROJ15   MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
PROJ20   MVC   RINVKBK,TO+1                                                     
         MVC   P2+66(2),=C'PJ'                                                  
         LA    R3,P2                                                            
         GOTO1 VDEMUP,DMCB,(C'I',RINVPEL),UPEL,ACOMFACS                         
         BAS   RE,FORMDEM                                                       
******   MVC   PJVAL,THISHMS                                                    
         MVC   PJVAL,THISRTG                                                    
         MVC   THISCD,=C'PJ'                                                    
         BAS   RE,MAINTCOD                                                      
         L     R5,APJBUFF                                                       
         MOVE  ((R5),1000),IO                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET OLD HPT'S AND DEAL WITH UNCONVERTED RECS          
         SPACE 1                                                                
GETHPT   NTR1                                                                   
         LR    R6,R4                                                            
         MVI   ELCODE,X'5E'        LOOK FOR A BOOK ELEMENT                      
         BAS   RE,GETEL                                                         
         ST    R6,ABOOKEL          SAVE ELEMENT ADDRESS                         
         BE    GETHPT2             FOUND AN ELEMENT                             
         USING RIBELEM,R6                                                       
         LA    R6,DUB                                                           
         XC    DUB,DUB             CLEAR ELEMENT AREA                           
         MVC   RIBELEM(2),=X'5E07' FORCE AN ELEMENT IN                          
         MVC   RIBFILE(3),=C'PTN'                                               
         MVC   RIBBOOK,=X'520A'                                                 
         GOTO1 VRECUP,DMCB,(2,(R4)),DUB,ABOOKEL                                 
         SPACE 1                                                                
GETHPT2  L     R6,ABOOKEL                                                       
         MVI   IUNSW,C'Y'                                                       
         CLC   RIBFILE(3),=C'IUN'                                               
         BE    *+8                                                              
         MVI   IUNSW,C'N'          NOT IN IUN FORMAT                            
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR                                                
         SPACE 1                                                                
GETHPT4  XC    WORK,WORK           BUILD DUMMY INDEX FF UPGRADE EL              
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVI   RAVLNCAT,C'I'       INVENTORY TO INVENTORY TRANSFER              
         MVC   RAVLNOP1,=X'FFFF'                                                
         MVC   RAVLNOP2,=H'1'      SET WEIGHTING TO ONE                         
         SPACE 1                                                                
         GOTO1 VDEMUP,DMCB,34(R4),WORK,ACOMFACS,DEMODUB,TOTSHR                  
         CLI   IUNSW,C'Y'          TEST FOR ORIGINAL REC IN IUN FORMAT          
         BE    XIT                                                              
         SPACE 1                                                                
GETHPT6  XC    WORK,WORK           ADD NEW HPT'S TO NON-IUN RECORD              
         LA    RE,WORK             BY MEANS OF INDEX 100 UPGRADE                
         MVC   RAVLNCOD(2),=X'050E'   BUILD DUMMY UPGRADE ELEMENT               
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         MVI   ELCODE,X'05'                                                     
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         GOTO1 VRECUP,DMCB,(2,(R4)),WORK,(R6)                                   
         SPACE 1                                                                
         GOTO1 VDEMUP,DMCB,(C'I',34(R4)),WORK,ACOMFACS,DEMODUB                  
         B     XIT                                                              
         DROP  R6,RE                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT DEMO LINE                                      
         SPACE 1                                                                
FORMDEM  NTR1                                                                   
         LA    R2,DEMLIST                                                       
         LA    R3,70(R3)           R3=A(PRINT LINE)                             
         LA    R4,6                                                             
         XC    DEMAREA,DEMAREA                                                  
         LA    R6,DEMAREA          OUTPUT VALUES RETURNED TO DEMAREA            
         SPACE 1                                                                
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCK,DEMAREA                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,8(R6)           GET RTG                                       
         ST    R1,THISRTG         AND STORE IT                                  
         L     R1,20(R6)          GET HOMES                                     
         ST    R1,THISHMS         AND STORE IT                                  
FORMDEM2 EQU   *                                                                
         L     R1,0(R6)                                                         
         EDIT  (R1),(6,(R3)),1                                                  
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R6,4(R6)            POINT TO NEXT OUTPUT VALUE                   
         BCT   R4,FORMDEM2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHOOSE ONE OF THE RECORDS                             
         SPACE 3                                                                
CHOOSE   NTR1                                                                   
         L     R5,ALYBUFF          SELECT RECORD WITH HIGHER HOMES              
         CLC   LYVAL,PJVAL                                                      
         BH    CH2                                                              
         L     R5,APJBUFF                                                       
         MVC   P+66(66),P2+66      IF PJ WINS, PRINT PJ LINE                    
*                                                                               
*- EXIT WITHOUT UPDATE IF NO PROJECTION FOUND (IN 'PROJECT' RTN)                
*                                                                               
*******  OC    PJVAL,PJVAL         NO HITS - NOT TOO INTERESTED                 
*                                                                               
         OC    FOUNDPJ,FOUNDPJ     NO HITS - NOT TOO INTERESTED                 
         BZ    XIT                                                              
         SPACE 1                                                                
CH2      MVC   P2,SPACES           ONLY PRINT 1 LINE, NOT PJ AND ACTUAL         
         GOTO1 DATCON,DMCB,(2,CTODAY),(3,BTODAY)                                
         MVC   KEY,0(R5)           MOVE IN KEY OF NEW RECORD                    
         SPACE 1                                                                
CH3      GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     IS IT THERE ALREADY                          
         MVC   KEY(27),KEYSAVE                                                  
         BE    CH4                 YES-GO TO CHANGE CODE                        
         SPACE 1                                                                
         MOVE  (IO,1000),(R5)                                                   
         SPACE 1                                                                
CH3C     LA    R4,IO                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'        IS THERE AN ACTIVITY ELEMENT YET             
         BAS   RE,GETEL                                                         
         BNE   CH3G                                                             
         GOTO1 VRECUP,DMCB,(2,(R4)),(R6),0     YES - JUNK IT                    
         SPACE 1                                                                
CH3G     XC    WORK,WORK           ADD AN ACTIVITY ELEMENT                      
         LA    RE,WORK                                                          
         USING RINVAEL,RE                                                       
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,BTODAY     TODAY'S DATE - BINARY                        
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'A'                                                    
         DROP  RE                                                               
         SPACE 1                                                                
         GOTO1 VRECUP,DMCB,(2,(R4)),WORK,(R6)                                   
CH3L     GOTO1 ADDREC              NO - ADD NEW ONE                             
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'ADDREC TRY'                                           
         MVC   P+13(27),KEY                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST END                                                                    
*                                                                               
         B     XIT                                                              
         SPACE                                                                  
CH4      GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MOVE  (IO,1000),(R5)                                                   
         LA    R4,IO                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'        UPDATE ACTIVITY ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   CH6                                                              
         LR    RE,R6               ESTABLISH ADDRESSABILITY TO ELEMENT          
         USING RINVAEL,RE                                                       
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'C'                                                    
CH6      GOTO1 PUTREC                                                           
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'PUTREC TRY'                                           
         MVC   P+13(27),KEY                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST END                                                                    
*                                                                               
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO MAINTAIN CODE & UPGRADE ELEMENTS                      
         SPACE 1                                                                
MAINTCOD NTR1                                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'CD'        IS THERE A CD ELEMENT YET                    
         BAS   RE,GETEL                                                         
         BNE   MC2                                                              
         GOTO1 VRECUP,DMCB,(2,IO),(R6),0     YES - JUNK IT                      
         SPACE 1                                                                
MC2      LA    R2,WORK             BUILD A NEW ELEMENT IN WORK                  
         XC    WORK,WORK                                                        
         USING RINVCEL,R2                                                       
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,THISCD                                                  
         OI    RINVCTYP,X'40'       PRODUCED BY OP                              
         SPACE 1                                                                
         TM    TO,X'20'            IS 'TO BOOK' ESTIMATED                       
         BNO   *+8                                                              
         MVI   RINVCSET,C'E'       MOVE IN 'E'                                  
         TM    TO,X'04'            IS 'TO BOOK' PROJECTED                       
         BNO   *+8                                                              
         MVI   RINVCSET,C'P'       MOVE IN 'P'                                  
         TM    TO,X'02'            IS 'TO BOOK' SPECIAL SURVEY                  
         BNO   *+8                                                              
         MVI   RINVCSET,C'S'       MOVE IN 'S'                                  
         SPACE 1                                                                
         GOTO1 VRECUP,DMCB,(2,IO),(R2),(R6)                                     
         DROP  R2                                                               
         LA    R6,IO               LOOK FOR UPGRADE ELEMENT                     
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   MC4                                                              
         GOTO1 VRECUP,DMCB,(2,IO),(R6),0     AND DELETE                         
         SPACE 1                                                                
MC4      CLC   THISCD,=C'PJ'       ADD AN UPGRADE EL. FOR PROJECTION            
         BNE   XIT                                                              
         GOTO1 VRECUP,DMCB,(2,IO),UPEL,(R6)                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT CHUNKS                                         
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         SPACE 2                                                                
SPLAT1   CLC   P5,SPACES                                                        
         BE    SPLAT2                                                           
         MVI   SPACING,1                                                        
         MVI   ALLOWLIN,6                                                       
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   P,P5                                                             
         MVC   P2,P6                                                            
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         SPACE 2                                                                
SPLAT2   MVI   SPACING,2                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   P5,SPACES                                                        
         MVC   P6,SPACES                                                        
         B     XIT                                                              
         SPACE 2                                                                
NEWDPT   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'SPUL'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         L     R2,SAVER2                                                        
         MVC   H4+10(4),TITSTAT                                                 
         MVC   H4+16(24),TITMKT                                                 
         CLI   TITDET,C'Y'         IF NO DETAILS, SKIP DAYPART                  
         BNE   HOOK4E                                                           
         LA    R3,DPTBL            LOOK UP DAYPART                              
         MVC   DPBYTE,0(R2)                                                     
         SPACE 2                                                                
HOOK2    CLC   DPBYTE,0(R3)                                                     
         BE    HOOK4                                                            
         LA    R3,L'DPTBL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   HOOK2                                                            
         SPACE 2                                                                
HOOK4    MVC   H4+50(20),1(R3)     DAYPART                                      
HOOK4E   MVC   H4+74(3),TITSRCE                                                 
         LA    R2,FROM                                                          
         LA    R3,H4+83                                                         
         BAS   RE,BOUT                                                          
         CLI   5(R3),C' '                                                       
         BE    HOOK5                                                            
         MVC   6(4,R3),=C' TO '                                                 
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
HOOK5    LA    R2,TO                                                            
         LA    R3,9(R3)                                                         
         BAS   RE,BOUT                                                          
         CLI   TITUPH+5,0                                                       
         BE    HOOK6                                                            
         MVC   H5+74(7),=C'UPGRADE'                                             
         MVC   H5+82(25),TITUP                                                  
         SPACE 1                                                                
HOOK6    CLI   TITDET,C'Y'         IF NO DETAILS, SKIP BOXES                    
         BNE   XIT                                                              
         SPACE 1                                                                
         L     R4,ABOX             INITIALIZE BOXES                             
         LTR   R4,R4                                                            
         BZ    HOOK7                                                            
         USING BOXD,R4                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+009,C'C'    DAY(S)                                       
         MVI   BOXCOLS+021,C'C'    TIME(S)                                      
         MVI   BOXCOLS+049,C'C'    PROGRAMMING                                  
         MVI   BOXCOLS+054,C'C'    INV NU                                       
         MVI   BOXCOLS+063,C'C'    START DATE                                   
         MVI   BOXCOLS+070,C'C'    CODE                                         
         MVI   BOXCOLS+076,C'C'    HUT                                          
         MVI   BOXCOLS+083,C'C'    SHR                                          
         MVI   BOXCOLS+090,C'C'    RTG                                          
         MVI   BOXCOLS+097,C'C'    W18+                                         
         MVI   BOXCOLS+104,C'C'    M18+                                         
         MVI   BOXCOLS+110,C'R'    TOTAL HOMES                                  
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
HOOK7    EQU   *                                                                
         B     XIT                                                              
         SPACE 1                                                                
BOUT     NTR1                                                                   
         SPACE 1                                                                
         LA    R1,SVCLST                                                        
BOUT4    CLC   0(1,R2),3(R1)       CONVERT BOOKVAL TO PRINTABLE PREFIX          
         BE    BOUT7                                                            
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BOUT4                                                            
         DC    H'0'                                                             
BOUT7    CLI   1(R1),C' '                                                       
         BE    BOUT10                                                           
         MVC   0(1,R3),1(R1)                                                    
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
BOUT10   ZIC   R1,2(R2)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
         EDIT  (1,1(R2)),(2,3(R3))                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         EJECT                                                                  
*              TABLES, LTORG ETC                                                
         SPACE 3                                                                
MONCOD   DC    C'JF3AM6Y8SOND'                                                  
         SPACE 1                                                                
****DEMLIST  DC    X'00',C'P',AL1(01)  HUT                                      
****         DC    X'00',C'S',AL1(01)  SHR                                      
****         DC    X'00',C'R',AL1(02)  MET                                      
****         DC    X'00',C'R',AL1(01)  RTG                                      
****         DC    X'00',C'R',AL1(45)  W18+                                     
****         DC    X'00',C'R',AL1(42)  W1849                                    
****         DC    X'00',C'R',AL1(41)  W1834                                    
****         DC    X'00',C'R',AL1(47)  W2549                                    
****         DC    X'00',C'R',AL1(48)  W2554                                    
****         DC    X'00',C'R',AL1(95)  M18+                                     
****         DC    X'00',C'R',AL1(92)  M1849                                    
****         DC    X'00',C'T',AL1(01)  HOMES                                    
DEMLIST  DC    X'00',C'P',AL1(01)  HUT                                          
         DC    X'00',C'S',AL1(01)  SHR                                          
         DC    X'00',C'R',AL1(01)  RTG                                          
         DC    X'00',C'R',AL1(45)  W18+                                         
         DC    X'00',C'R',AL1(95)  M18+                                         
         DC    X'00',C'T',AL1(01)  HOMES                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
         DS    0H                                                               
DPTBL    DS    0CL21                                                            
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIAL'                                                   
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OSOAPS'                                                     
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
NDPT     EQU   (*-DPTBL)/L'DPTBL                                                
         SPACE 2                                                                
RELOC    DC    A(*)                                                             
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
DBEXTRA1 DS    CL128                                                            
         EJECT                                                                  
         SPACE 2                                                                
STACK    DS    8000C                                                            
         SPACE 1                                                                
PJBUFF   DS    1000C                                                            
         SPACE 1                                                                
LYBUFF   DS    1000C                                                            
         SPACE 2                                                                
DUMMY    DSECT                                                                  
**********RINT OFF                                                              
       ++INCLUDE REGENREP                                                       
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE RESPLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESPLD8D                                                       
**********RINT ON                                                               
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
FROM     DS    CL3                                                              
TO       DS    CL3                                                              
UPEL     DS    CL24                                                             
DPLIST   DS    CL20                                                             
SAVER2   DS    F                                                                
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
RELO     DS    A                                                                
DPBYTE   DS    CL1                                                              
IUNSW    DS    C                                                                
CTODAY   DS    CL2                                                              
BTODAY   DS    CL3                                                              
P5       DS    CL133                                                            
P6       DS    CL133                                                            
DEMAREA  DS    CL60                                                             
VRECUP   DS    V                                                                
VDEMUP   DS    V                                                                
APJBUFF  DS    A                                                                
ALYBUFF  DS    A                                                                
ABOOKEL  DS    A                                                                
TOTSHR   DS    3F                                                               
DEMODUB  DS    D                                                                
LASTBK   DS    CL3                                                              
LASTCD   DS    CL2                                                              
PJVAL    DS    F                                                                
LYVAL    DS    F                                                                
THISHMS  DS    F                                                                
THISRTG  DS    F                                                                
THISCD   DS    CL2                                                              
FOUNDPJ  DS    CL2                 PROJECTION FOUND SWITCH                      
*                                  0 = NOT FOUND  ^0 = FOUND                    
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064RESPL98A  05/01/02'                                      
         END                                                                    
