*          DATA SET NESFM6C    AT LEVEL 038 AS OF 04/21/09                      
*PHASE T31C6CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C6C  -- BILL FORMULA RECORDS MAINTENANCE          *         
*                                                                     *         
*  COMMENTS:     THIS WILL REPLACE SPSFM3B MODULE                     *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30) WHICH CALLS THIS.                  *         
*                                                                     *         
*  INPUTS:       SCREENS NESFM4E(MAINT), AND NESFM4F(LIST)            *         
*                                                                     *         
*  OUTPUTS:      BILL FORMULA RECORDS                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
*         PRD, EST CAN BE 'ALL'. THIS TRANSLATES TO                   *         
*         AAA FOR PRODUCT, NULL FOR EST                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C54 - BILL FORMULA RECORDS MAINTENANCE'                      
T31C6C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T31C6C*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         L     R1,SYSPARMS                                                      
         MVC   ATIOB,0(R1)                                                      
*                                                                               
*****    BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
VK       DS    0H                                                               
*                                                                               
         MVC   BFRESTD,SPACES                                                   
         OI    BFRESTDH+6,X'80'                                                 
*                                                                               
         XC    BFRKEY,BFRKEY       START BUILDING THE KEY X'0D4C'               
         LA    R4,BFRKEY                                                        
         USING BFKEY,R4                                                         
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
*                                                                               
         LA    R2,BFRMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         BAS   RE,VSUBMED          CHECK AND STORE SUB-MEDIA                    
*                                                                               
         XC    SMEDH,SMEDH         CALL VALIMED W/ MED N TO SET BAGYMD          
         MVC   SMEDH(9),=XL9'0900000000010000D5'                                
         LA    R2,SMEDH                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                                                          
         MVC   BFKAGYMD,BAGYMD                                                  
         MVC   BFSUBMED,SUBMED                                                  
*                                                                               
         LA    R2,BFRCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BNE   ERRMIS                                                           
         CLI   BFRPRDH+5,0         BUT MAKE SURE, NO PRD                        
         BNE   ERRMIS                                                           
         CLI   BFRESTH+5,0         AND NO EST THEN                              
         BNE   ERRMIS                                                           
         B     VKX                                                              
*                                                                               
VK10     MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALICLT                                                          
         MVC   BFKCLT,BCLT                                                      
*                                                                               
         XC    WORK,WORK           GET B1X PROFILE                              
         XC    PROFB1X,PROFB1X                                                  
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),BFRMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),BOFFICE   OFFICE                                      
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
         CLI   PROFB1X+11,C'N'     TEST IF BFR'S ALLOWED                        
         BE    B1XERR                                                           
         CLI   PROFB1X+11,0                                                     
         BE    B1XERR                                                           
*                                                                               
         LA    R2,BFRPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK30                                                             
         CLI   ACTNUM,ACTREP       IF REPORT CAN BE BLANK                       
         BE    VK30                                                             
         B     ERRMIS                                                           
*                                                                               
VK20     CLC   =C'ALL',BFRPRD                                                   
         BNE   *+14                                                             
         MVC   BFKPRD,=C'AAA'      PRD ALL => AAA                               
         B     VK30                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIPRD                                                          
         MVC   BFKPRD,QPRD                                                      
*                                                                               
VK30     LA    R2,BFRESTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP       IF REPORT CAN BE BLANK                       
         BE    VKX                                                              
         B     ERRINV                                                           
*                                                                               
VK35     CLC   =C'ALL',BFREST                                                   
         BNE   *+12                                                             
         MVI   BEST,0                                                           
         B     VKX                                                              
*                                                                               
         CLI   BFRPRDH+5,0         PRODUT ENTERED ?                             
         BE    *+14                NO, THIS IS A FILTER                         
         CLC   BFKPRD,=C'AAA'      IF PRD  NEQ ALL OR AAA                       
         BNE   VK37                CAN USE VALIEST                              
*                                                                               
         TM    4(R2),X'08'         IF THIS IS A FILTER, JUST CHECK              
         BZ    ERRINV              THAT EST IS NUMERIC                          
         SR    R1,R1                                                            
         IC    R1,BFRESTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BFREST(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,BFKEST                                                        
         B     VKX                                                              
*                                                                               
VK37     MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIEST                                                          
         L     R6,AIO2                                                          
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(8,BFRESTD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(8,BFRESTD+9)                               
         MVI   BFRESTD+8,C'-'                                                   
         OI    BFRESTDH+6,X'80'                                                 
         MVC   SVESTSDT,ESTART                                                  
         MVC   SVESTNDT,EEND                                                    
         MVC   BFKEST,BEST                                                      
         DROP  R6                                                               
*                                                                               
*                                                                               
VKX      MVC   KEY(13),BFRKEY                                                   
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         B     XIT                                                              
*                                                                               
*                                                                               
*******************************************************************             
*                      VALREC                                                   
*******************************************************************             
*                                                                               
VR       DS    0H                                                               
*                                                                               
         L     R3,AIO                                                           
         USING BFREC,R3                                                         
         MVC   BFKEY,KEY           WILL BE REBUILDING REC                       
         XCEFL 24(R3),1000                                                      
         MVC   BFRLEN,DATADISP                                                  
*                                                                               
         LA    R5,BFRMOS1H                                                      
         USING LINDSECT,R5                                                      
         LHI   R4,12               MAX NUMBER OF ENTRIES ON THE SCREEN          
*                                                                               
VR10     LA    R2,LINMOSH          MONTH OF SERICE                              
         CLI   5(R2),0             TEST ANY DATA  ON THIS LINE                  
         BNE   VR15                                                             
         LA    R2,LINFMLH          NO MOS, CHECK FORMULA                        
         CLI   5(R2),0                                                          
         BE    VRNXT               THIS LINE'S EMPTY, CHK NXT LINE              
         LA    R2,LINMOSH          IF FORMULA INPUT, MAKE MOS A '*'             
         MVI   8(R2),C'*'                                                       
         MVI   5(R2),1                                                          
*                                                                               
VR15     XC    ELEM,ELEM                                                        
         USING BFRCDELD,R6                                                      
         LA    R6,ELEM                                                          
         MVI   BFRCDEL,BFRCDELQ    ELEMENT CODE                                 
         MVI   BFRCDLEN,BFRCDLNQ   ELEMENT LENGTH                               
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   VR20                                                             
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         XC    PVALBSTA,PVALBSTA                                                
         DROP  R1                                                               
         B     VR30                                                             
*                                                                               
VR20     CLI   PROFB1X+12,C'N'     DOES B1X ALLOW FOR MOS?                      
         BE    ERRINV                                                           
         GOTO1 PERVAL,DMCB,(LINMOSH+5,LINMOS),PERVALST,0                        
         TM    DMCB+4,X'03'                                                     
         BNZ   ERRINV                                                           
*                                                                               
VR25     LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
*                         WAS START DAY OR ANY PART OF END DAY ENTERED?         
         TM    PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                         
         BNO   ERRINV                      YES, IT SHOULDN'T BE                 
*                                                                               
VR30     MVC   BFRCDDTE,PVALBSTA                                                
         XC    BFRCDDTE,=2X'FF'                                                 
         CLI   BFKEST,0            IF EST=ALL                                   
         BE    VR40                                                             
         CLC   =C'AAA',BFKPRD      OR PRD=ALL                                   
         BE    VR40                                                             
         OC    PVALBSTA,PVALBSTA   OR NO DATE                                   
         BZ    VR40                                                             
         CLC   PVALESTA(4),SVESTSDT   PERIOD SHOULD BE IN ESTIMATE'S            
         BL    OUTESTR                                                          
         CLC   PVALEEND(4),SVESTNDT                                             
         BH    OUTESTR                                                          
         DROP  R1                                                               
*                                                                               
VR40     GOTO1 FINDMNTH,DMCB,BFRCDDTE                                           
         BE    RECXISTS                                                         
*                                                                               
         LA    R2,LINFMLH          BILL FORMULA                                 
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ERRMIS                                                           
*                                  GO AND VALIDATE FORMULA                      
         GOTO1 VALIFML,DMCB,(R2)                                                
*                                                                               
         MVC   BFRCDFML,BFORMULA                                                
*                                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRNXT    LA    R5,LINNEXTL                                                      
         BCT   R4,VR10                                                          
*                                                                               
VRX      CLC   BFRLEN,DATADISP     ANY ELEMENTS IN REC ?                        
         BH    DR                  IF NOT, ERROR                                
         LA    R2,BFRMOS1H         MONTH OF SERICE                              
         B     ERRMIS                                                           
         DROP  R5,R3                                                            
*                                                                               
*******************************************************************             
*        DISPREC                                                                
*******************************************************************             
DR       BAS   RE,CLR                                                           
*                                                                               
         LA    R2,BFRMOS1H                                                      
         USING LINDSECT,R2                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BFRCDELQ                                                  
         BAS   RE,GETEL                                                         
         BE    DR20                                                             
         B     DRX    NO LONGER DIE - JUST EXIT WHEN NO ELEMENT EXISTS          
*                                                                               
         USING BFRCDELD,R6                                                      
*                                                                               
DR20     MVC   LINMOS,SPACES                                                    
         MVI   LINMOS,C'*'         MARKER FOR NO DATE                           
         MVI   LINMOSH+5,1                                                      
         CLC   BFRCDDTE,=2X'FF'                                                 
         BE    DR40                                                             
*                                                                               
         MVC   FULL,BFRCDDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LINMOS)                                  
         MVI   LINMOSH+5,6                                                      
*                                                                               
DR40     DS    0H                                                               
         OI    LINMOSH+6,X'80'                                                  
         MVC   BFORMULA,BFRCDFML                                                
         GOTO1 DISPFML,DMCB,LINFMLH                                             
*                                                                               
         LA    R2,LINNEXTL         R2 = A(NEXT LINE ON SCREEN)                  
         LA    R1,BFRMOSLH         LAST LINE                                    
         CR    R2,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                SHOULD NEVER BE PAST LAST LINE               
         BAS   RE,NEXTEL           GOT ANOTHER FORMULA?                         
         BE    DR20                YES, TRY TO DISPLAY IT                       
*                                                                               
         DROP  R6,R2                                                            
*                                                                               
DRX      B     XIT                                                              
*******************************************************************             
*        DISPKEY                                                                
*******************************************************************             
*                                                                               
DK       DS     0H                                                              
*                                                                               
         L      R6,AIO                                                          
         USING  BFKEY,R6                                                        
*                                                                               
         MVI   BFRMED,C'N'                                                      
         CLI   BFSUBMED,0                                                       
         BE    *+10                                                             
         MVC   BFRMED,BFSUBMED                                                  
*                                                                               
         OI    BFRMEDH+6,X'80'      TRANSMIT MEDIA CODE TO SCREEN               
         MVI   BFRMEDH+5,1                                                      
*                                                                               
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BFKCLT),BFRCLT                               
         OI    BFRCLTH+6,X'80'                                                  
         MVI   BFRCLTH+5,3          TRANSMIT CLIENT CODE TO SCREEN              
         CLI   BFRCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   BFRCLTH+5,2                                                      
*                                                                               
         MVC   BFRPRD,BFKPRD                                                    
         CLC   BFRPRD(3),=C'AAA'   AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   BFRPRD(3),=C'ALL'                                                
         MVI   BFRPRDH+5,3                                                      
         CLI   BFRPRD+2,C' '                                                    
         BNE   *+8                                                              
         MVI   BFRPRDH+5,2                                                      
         OI    BFRPRDH+6,X'80'                                                  
*                                                                               
         CLI   BFKEST,0                                                         
         BNE   DK40                                                             
         MVC   BFREST(3),=C'ALL'                                                
         MVI   BFRESTH+5,3                                                      
         OI    BFRESTH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK40     EDIT  BFKEST,BFREST,FILL=0                                             
         OI    BFRESTH+4,X'08'      NUMERIC CODE                                
         OI    BFRESTH+6,X'80'                                                  
         MVI   BFRESTH+5,3                                                      
*                                                                               
*                                                                               
DKX      B     VK                                                               
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
*        LISTRECS                                                               
*******************************************************************             
*                                                                               
LR       LA    R4,KEY                                                           
         USING BFKEY,R4                                                         
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
         MVC   KEY,BFRKEY                                                       
*                                                                               
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
LR20     CLC   KEY(3),BFRKEY       ANY MORE BILLFRM RECS FOR THAT MEDIA         
         BNE   LRX                 NO                                           
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   FILTFLAG,0                                                       
         BAS   RE,FILTERS                                                       
         TM    FILTFLAG,FLTSKIP    X'80' RECORD TO BE FILTERED OUT?             
         BO    LR15                      YES                                    
*                                                                               
         GOTO1 GETREC                                                           
         XC    LISTAR,LISTAR                                                    
*                                                                               
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BFKCLT),LSTCLT                               
         MVC   LSTPRD,BFKPRD                                                    
         CLC   LSTPRD,=C'AAA'      AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   LSTPRD,=C'ALL'                                                   
*                                                                               
         CLI   BFKEST,0                                                         
         BNE   *+14                                                             
         MVC   LSTEST,=C'ALL'                                                   
         B     LR30                                                             
*                                                                               
         EDIT  BFKEST,LSTEST,FILL=0                                             
*                                                                               
*                                                                               
LR30     L     R6,AIO                                                           
         MVI   ELCODE,BFRCDELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR15                                                             
*                                                                               
*                                  SEE IF OTHER FORMULAS EXIST                  
         ST    R6,FELEM            SAVE ADDRESS OF FIRST                        
         BAS   RE,NEXTEL                                                        
         BNE   LR45                                                             
         MVC   LSTBFR(16),=C'*** MULTIPLE ***'                                  
         B     LR90                                                             
*                                                                               
LR45     L     R6,FELEM                                                         
         USING BFRCDELD,R6                                                      
         MVC   LSTMOS,SPACES                                                    
         MVI   LSTMOS,C'*'         MARKER FOR NO DATE                           
         CLC   BFRCDDTE,=2X'FF'                                                 
         BE    LR50                                                             
*                                                                               
         MVC   FULL,BFRCDDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LSTMOS)                                  
*                                                                               
LR50     MVC   BFORMULA,BFRCDFML                                                
         GOTO1 DISPFML,DMCB,(C'L',LSTBFR)                                       
         DROP  R6                                                               
*                                                                               
LR90     GOTO1 LISTMON                                                          
*        MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     LR15                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        PRINT REPORT                                                           
*                                                                               
PR       L     R8,ASPOOLD                                          L03          
         USING SPOOLD,R8                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
*                                                                               
         LA    R5,P1                                                            
         USING PLINED,R5                                                        
*                                                                               
PR1      LA    R4,KEY                                                           
         USING BFKEY,R4                                                         
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   PR10                                                             
         MVC   KEY(L'BFRKEY),BFRKEY                                             
*                                                                               
PR10     GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR15     GOTO1 SEQ                                                              
PR20     CLC   KEY(3),BFRKEY       ANY MORE BILLFRM RECS FOR THAT MEDIA         
         BNE   PRX                 NO                                           
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVI   FILTFLAG,0                                                       
         BAS   RE,FILTERS                                                       
         TM    FILTFLAG,FLTSKIP    X'80' RECORD TO BE FILTERED OUT?             
         BO    PR15                      YES                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BFKCLT),PLSTCLT                              
         MVC   PLSTPRD,BFKPRD                                                   
         CLC   PLSTPRD,=C'AAA'     AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   PLSTPRD,=C'ALL'                                                  
*                                                                               
         OC    BFKEST,BFKEST                                                    
         BNZ   *+14                                                             
         MVC   PLSTEST,=C'ALL'                                                  
         B     PR30                                                             
*                                                                               
         EDIT  BFKEST,PLSTEST,FILL=0                                            
*                                                                               
*                                                                               
PR30     L     R6,AIO                                                           
         MVI   ELCODE,BFRCDELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR15                                                             
*                                                                               
PR40     DS    0H                                                               
         USING BFRCDELD,R6                                                      
         MVC   PLSTMOS,SPACES                                                   
         MVI   PLSTMOS,C'*'        MARKER FOR NO DATE                           
         CLC   BFRCDDTE,=2X'FF'                                                 
         BE    PR50                                                             
*                                                                               
         MVC   FULL,BFRCDDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,PLSTMOS)                                 
*                                                                               
PR50     MVC   BFORMULA,BFRCDFML                                                
         GOTO1 DISPFML,DMCB,(C'L',PLSTBFR)                                      
*                                                                               
PR90     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BNE   PR15                NO MORE  - GET NEXT RECORD                   
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,BFKCLT),PLSTCLT                              
         MVC   PLSTPRD,BFKPRD                                                   
         CLC   PLSTPRD,=C'AAA'     AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   PLSTPRD,=C'ALL'                                                  
*                                                                               
         OC    BFKEST,BFKEST                                                    
         BNZ   *+14                                                             
         MVC   PLSTEST,=C'ALL'                                                  
         B     PR95                                                             
*                                                                               
         EDIT  BFKEST,PLSTEST,FILL=0                                            
*                                                                               
PR95     B     PR40                NEXT RECORD ENTRY                            
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
*                                                                               
HOOKX    XIT1                                                                   
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
*                                                                               
*******************************************************************             
*        CHECK THE FILTERS                                        *             
*******************************************************************             
FILTERS  NTR1                                                                   
         LA    R4,BFRKEY                                                        
         USING BFREC,R4                     FILTER BY:                          
*                                                                               
         OC    BFKCLT,BFKCLT                CLIENT?                             
         BZ    FLT10                                                            
         CLC   BFKCLT,KEY+BFKCLT-BFKEY                                          
         BNE   FLT100                                                           
*                                                                               
FLT10    OC    BFKPRD,BFKPRD                PRODUCT?                            
         BZ    FLT20                                                            
         CLC   BFKPRD,KEY+BFKPRD-BFKEY                                          
         BNE   FLT100                                                           
*                                                                               
FLT20    OC    BFKEST,BFKEST                ESTIMATE?                           
         BZ    FLT30                                                            
         CLC   BFKEST,KEY+BFKEST-BFKEY                                          
         BNE   FLT100                                                           
*                                                                               
FLT30    OC    BFKMGR,BFKMGR                MARKET?                             
         BZ    FLT40                                                            
         CLC   BFKMGR,KEY+BFKMGR-BFKEY                                          
         BNE   FLT100                                                           
*                                                                               
FLT40    CLC   SUBMED,KEY+BFSUBMED-BFKEY                                        
         BE    FLTX                                                             
         CLI   KEY+BFSUBMED-BFKEY,0         IN CASE SUB-MED IS ZERO             
         BNE   FLT100                                                           
         CLI   SUBMED,C'N'                  IT CAN ONLY BE 'N'                  
         BE    FLTX                                                             
*                                                                               
FLT100   OI    FILTFLAG,FLTSKIP   TURNED ON TO FILTER THIS RECORD OUT           
*                                                                               
FLTX     B     XIT                                                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*******************************************************************             
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
B1XERR   MVI   ERROR,B1XINCMP      B1X PROFILE INCOMPATIBLITY                   
         B     VSFMERR                                                          
OUTESTR  MVI   ERROR,ESTRNGE       MONTH OUT OF ESTIMATE RANGE                  
         B     VSFMERR                                                          
RECXISTS MVI   ERROR,RECEXIST                                                   
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES A FORMULA                                              
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA'S FIELD HEADER)                    
*                                                                               
* ON EXIT:     BFORMULA            BINARY REPRESENTATION OF THE FORMULA         
***********************************************************************         
VALIFML  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         XC    BLOCK(200),BLOCK                                                 
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'83',BLOCK)                                  
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
*                                                                               
         XC    BFORMULA,BFORMULA   CLEAR THE FORMULA                            
*                                                                               
VFMLBBAS LA    R3,BLOCK            R3 = A(BILL BASIS)                           
         CLI   0(R3),0                                                          
         BE    VFMLCPCT            CHECK THE COMM PCT                           
*                                                                               
         CLI   1(R3),0             SHOULD BE UNDIVIDED                          
         BNE   VFMLINVL                                                         
*                                                                               
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
         LA    R5,12(R3)                                                        
*                                                                               
         CLI   0(R5),C'C'          CHECK FOR COMMISSION ONLY                    
         BNE   VFMLBB10                                                         
*                                                                               
         OI    BBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
*                                                                               
         CLI   0(R3),1                                                          
         BE    VFMLINVL                                                         
         LA    R5,1(R5)                                                         
*                                                                               
VFMLBB10 EX    R4,GROSCOM                                                       
         BE    VFMLCPCT                                                         
         EX    R4,NETCOM                                                        
         BNE   VFMLINVL                                                         
         OI    BBILLBAS,X'10'                                                   
*                                                                               
VFMLCPCT LA    R3,32(R3)           R3 = A(COMMISSION PERCENTAGE)                
*                                                                               
         CLI   0(R3),0             NOT REQUIRED                                 
         BNE   VFMLCP10                                                         
         CLI   32(R3),0            MISSING IF COMM BASE IS PRESENT              
         BNE   VFMLMISS                                                         
         B     VFMLCBAS                                                         
*                                                                               
VFMLCP10 ZIC   R0,0(R3)            VALIDATE THE PERCENTAGE                      
         BCTR  R0,0                                                             
         GOTO1 CASHVAL,DMCB,(4,13(R3)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    VFMLINVL                                                         
*                                                                               
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'      MUST BE A PERCENTAGE (0 > X >=100)           
         BH    VFMLINVL                                                         
         C     R0,=F'0'                                                         
         BNH   VFMLINVL                                                         
*                                                                               
         CLI   12(R3),C'+'                                                      
         BE    VFMLCP20                                                         
         CLI   12(R3),C'-'                                                      
         BNE   VFMLINVL            ERROR IF 1ST BYTE IS NEITHER + NOR -         
         LCR   R0,R0               MAKE NEGATIVE IF %-AGE IS NEGATIVE           
*                                                                               
VFMLCP20 STCM  R0,15,BBILLCOM                                                   
*                                                                               
VFMLCBAS LA    R3,32(R3)           R3 = A(COMMISSION BASIS)                     
*                                                                               
         CLI   0(R3),0             NOT REQUIRED                                 
         BNE   VFMLCB10                                                         
         CLI   BLOCK+32,0          ANY COMMISSION PERCENTAGE                    
         BNE   VFMLMISS            YES, THEN WE NEED COMMISSION BASIS           
         B     VFML10                                                           
*                                                                               
VFMLCB10 ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
         LA    R5,12(R3)                                                        
         EX    R4,GROSCOM                                                       
         BE    VFML10                                                           
         EX    R4,NETCOM                                                        
         BNE   VFMLINVL                                                         
         OI    BBILLBAS,X'01'                                                   
*                                                                               
VFML10   DS    0H                                                               
*                                                                               
VFMLX    B     XIT                                                              
*                                                                               
GROSCOM  CLC   0(0,R5),=C'GROSS'                                                
NETCOM   CLC   0(0,R5),=C'NET  '                                                
*                                                                               
VFMLMISS MVI   ERROR,MISSING                                                    
         B     *+8                                                              
VFMLINVL MVI   ERROR,INVALID                                                    
         L     R1,ATIOB            ERROR THAT SETS THE CURSOR TO WHERE          
         USING TIOBD,R1                THE ERROR IS IN THE SCANNED LINE         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
         B     VSFMERR                                                          
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS A FORMULA                                               
*                                                                               
* ON ENTRY:    PARAMETER 1  BYTE 0     C'L' = POINTING TO LISTAR FIELD          
*                           BYTES 1-3  A(LISTAR FIELD)                          
*                        OR BYTES 0-3  A(FORMULA FIELD HEADER)                  
*              BFORMULA            FORMULA TO BE DISPLAYED                      
***********************************************************************         
DISPFML  NTR1                                                                   
         XC    FMLFLAG,FMLFLAG                                                  
         SR    R4,R4               WILL HAVE ACCUME LEN OF INPUT                
         L     R2,0(R1)                                                         
         LA    R3,8(R2)            R3 = A(1ST BYTE IN FORMULA TEXT)             
         CLI   0(R1),C'L'          IS IT LISTAR FIELD ?                         
         BNE   *+12                NO, NOTHING SPECIAL                          
         MVI   FMLFLAG,C'L'                                                     
         LA    R3,0(R2)                                                         
*                                                                               
         TM    BBILLBAS,X'50'      FIGURE OUT WHICH BILL BASIS WE HAVE          
         BNO   DFML10                                                           
         MVC   0(4,R3),=C'CNET'                                                 
         LA    R3,4(R3)                                                         
         AHI   R4,4                                                             
         B     DFML50                                                           
*                                                                               
DFML10   TM    BBILLBAS,X'10'                                                   
         BNO   DFML20                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         AHI   R4,3                                                             
         B     DFML50                                                           
*                                                                               
DFML20   TM    BBILLBAS,X'40'                                                   
         BNO   DFML30                                                           
         MVC   0(5,R3),=C'CGROS'                                                
         LA    R3,5(R3)                                                         
         AHI   R4,5                                                             
         B     DFML50                                                           
*                                                                               
DFML30   MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
         AHI   R4,5                                                             
*                                                                               
DFML50   ICM   R5,15,BBILLCOM      IF NO COMMISSION                             
         BZ    DFMLX               THEN JUST BILL BASIS                         
*                                                                               
         MVI   0(R3),C','          OTHERWISE SEPARATE FIELD WITH COMMAS         
         LA    R3,1(R3)                                                         
         AHI   R4,1                                                             
*                                                                               
         LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WON'T FIT NICELY                 
         BNE   DFML60                                                           
         MVC   0(5,R3),=C'+100,'                                                
         LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,5(R3)                                                         
         AHI   R4,5                                                             
         B     DFML70                                                           
*                                                                               
DFML60   EDIT  (R5),(8,0(R3)),4,FLOAT=+,ALIGN=LEFT                              
         LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         LA    R1,7(R3)                                                         
         AHI   R4,8                                                             
DFML65   CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCTR  R4,0                                                             
         B     DFML65                                                           
*                                                                               
         LA    R3,1(R1)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         AHI   R4,1                                                             
*                                                                               
DFML70   MVC   0(3,R3),=C'NET'                                                  
         AHI   R4,3                                                             
         TM    BBILLBAS,X'01'                                                   
         BO    DFMLX                                                            
         MVC   0(5,R3),=C'GROSS'                                                
         AHI   R4,2                                                             
*                                                                               
DFMLX    CLI   FMLFLAG,C'L'                                                     
         BE    XIT                                                              
         OI    6(R2),X'80'                                                      
         STC   R4,5(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FIND THE FORMULA ELEMENT IN THE RECORD FOR A SPECIFIC MONTH                   
*                                                                               
* ON ENTRY:    PARAMETER 1         A(MONTH COMPLEMENTED WITH X'FF')             
*              AIO                 A(RECORD)                                    
*                                                                               
* ON EXIT:     PARAMETER 1         A(FORMULA ELEMENT IN THE RECORD)             
***********************************************************************         
FINDMNTH NTR1                                                                   
         L     R1,DMCB                                                          
         MVC   HALF,0(R1)          SAVE MONTH WE LOOKING FOR                    
*                                                                               
         L     R6,AIO              LOOKING FOR THE FORMULA ELEMENT              
         MVI   ELCODE,BFRCDELQ                                                  
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
FMTH10   BAS   RE,NEXTEL                                                        
         BNE   FMTHNO              NO MORE FORMULA ELEMENTS                     
*                                                                               
         USING BFRCDELD,R6                                                      
         CLC   HALF,BFRCDDTE       IF WE FOUND A MATCH                          
         BNE   FMTH10                                                           
         ST    R6,DMCB             THEN SAVE OFF THE ADDRESS                    
         DROP  R6                                                               
*                                                                               
FMTHYES  B     YES                                                              
*                                                                               
FMTHNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
*              CLEAR SCREEN                                                     
***********************************************************************         
CLR     NTR1                                                                    
        LA     R2,BFRMOS1H                                                      
CLR10   CLI    0(R2),0             END OF SCREEN ?                              
        BE     CLRX                DONE                                         
        TM     1(R2),X'20'         DO NOT CLEAR PROTECTED FIELDS                
        BO     CLR50                                                            
        ZIC    RE,0(R2)                                                         
        SH     RE,=H'8'                                                         
        TM     1(R2),X'02'         EXTENDED HEADER ?                            
        BZ     *+8                 YES, SUBTRACT EXTENSION LEN                  
        SH     RE,=H'8'                                                         
        BCTR   RE,0                                                             
        EX     RE,*+8                                                           
        B      *+10                                                             
        XC     8(0,R2),8(R2)                                                    
        OI     6(R2),X'80'                                                      
CLR50   ZIC    RE,0(R2)                                                         
        AR     R2,RE                                                            
        B      CLR10                                                            
CLRX    XIT1                                                                    
***********************************************************************         
* VALIDATE SUB MEDIA TYPE                                                       
***********************************************************************         
VSUBMED  NTR1                                                                   
         LA    R2,BFRMEDH                                                       
         LA    R3,SMEDTAB                                                       
         MVI   SUBMED,C'N'                                                      
*                                                                               
VSM10    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    ERRINV                                                           
*                                                                               
         CLC   0(1,R3),8(R2)       VALID SUBMEDIA                               
         BE    VSUBMEDX                                                         
         LA    R3,1(R3)                                                         
         B     VSM10                                                            
*                                                                               
VSUBMEDX DS    0H                                                               
         MVC   SUBMED,8(R2)                                                     
         B     XIT                                                              
*                                                                               
SMEDTAB  DC    C'N'                                                             
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'D'                RADIO                                        
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FF'                                                            
SMEDTABX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*  GET CPROF+6 (AAN) FOR CLUNPK                                                 
***********************************************************************         
GETAAN   NTR1                                                                   
*                                                                               
         MVI   CLTAAN,C'N'         JUST IN CASE WE DONT FIND CLT REC            
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         MVC   AIO,AIO3            AIO3 IS FREE                                 
*                                                                               
         L     R3,AIO1             UCOMM RECORD                                 
         USING BFKEY,R3                                                         
         MVC   KEY+1(1),BFKAGYMD   AGENCY/MEDIA                                 
         MVC   KEY+2(2),BFKCLT     CLIENT                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     HAVE CLIENT RECORD?                          
         BNE   GAAN10              NO                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTAAN,CPROF+6-CLTHDRD(R6)                                       
*                                                                               
GAAN10   MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   KEY(L'BFKEY),SAVEKEY   RESTORE DIR FOR SEQ READING               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,48,C' NETWORK BFORM REPORT '                                  
         SSPEC H2,48,C' -------------------- '                                  
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CLT  PRD  EST  START MOS  FORMULA'                        
         SSPEC H8,1,C'---  ---  ---  ---------  -------'                        
*                                                                               
         DC    X'00'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM4ED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBFML                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
ATIOB    DS    A                                                                
FELEM    DS    A                   ADDRESS OF FIRST FORMULA ELEMENT             
SVESTPER DS    0CL12               ESTIMATE PERIOD                              
SVESTSDT DS    CL6                 ESTIMATE START DATE (YYMMDD)                 
SVESTNDT DS    CL6                 ESTIMATE END DATE   (YYMMDD)                 
FMLFLAG  DS    X                                                                
FILTFLAG DS    X                                                                
FLTSKIP  EQU   X'80'                                                            
BFRKEY   DS    CL13                                                             
SAVEKEY  DS    CL13                                                             
MYESTART DS    CL6                                                              
MYEEND   DS    CL6                                                              
SUBMED   DS    CL1                 SUB MEDIA TYPE                               
SMEDH    DS    CL9                                                              
PROFB1X  DS    CL16                                                             
*                                                                               
BFORMULA DS    0XL(L'BFRCDFML)     BINARY REPRESENTATION OF FORMULA             
BBILLBAS DS    XL(L'EBILLBAS)      2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000'=GROSS, B'0001'=NET                   
BBILLCOM DS    XL(L'EBILLCOM)      SIGNED COMMISSION RATE (99.9999)             
*                                                                               
ERRNUM   DS    XL2                                                              
*                                                                               
CLTAAN   DS    CL1                 SAVED CPROF+6 FROM CLT RECORD                
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
* --- USED FOR CONVERTING THE DATES (EX. JAN/96 -TO- JAN01/96)                  
NTMPDT   DS    0CL8                                                             
NTMPMON  DS    CL3                                                              
NTMPDAY  DS    CL2                                                              
NTMPYR   DS    CL3                                                              
CHARYMD  DS    CL6                                                              
COMPYMD  DS    CL2                                                              
BINYMD   DS    XL3                                                              
*                                                                               
LINDSECT DSECT                                                                  
LINMOSH  DS    CL(L'BFRMOS1H)                                                   
LINMOS   DS    CL(L'BFRMOS1)                                                    
LINFMLH  DS    CL(L'BFRFML1H)                                                   
LINFML   DS    CL(L'BFRFML1)                                                    
LINNEXTL DS    0C                                                               
*                                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL1                                                              
LSTPRD   DS    CL3                                                              
         DS    CL1                                                              
LSTEST   DS    CL3                                                              
         DS    CL1                                                              
LSTMOS   DS    CL10                                                             
         DS    CL1                                                              
LSTBFR   DS    CL30                                                             
*                                                                               
PLINED   DSECT                                                                  
PLSTCLT  DS    CL3                                                              
         DS    CL2                                                              
PLSTPRD  DS    CL3                                                              
         DS    CL2                                                              
PLSTEST  DS    CL3                                                              
         DS    CL2                                                              
PLSTMOS  DS    CL10                                                             
         DS    CL1                                                              
PLSTBFR  DS    CL30                                                             
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE FATIOB                                                         
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038NESFM6C   04/21/09'                                      
         END                                                                    
