*          DATA SET PRSFM35    AT LEVEL 022 AS OF 04/02/19                      
*PHASE T41C35A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T41C35  -- NEW  BILL FORMULA RECORDS MAINTENANCE     *         
*                                                                     *         
*  COMMENTS:     THIS WILL REPLACE T41C17  MODULE                     *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T41C00), WHICH CALLS                 *         
*                GEGENCON (T00A30) WHICH CALLS THIS.                  *         
*                                                                     *         
*  INPUTS:       SCREENS PRSFMDB(MAINT), AND PRSFMDC(LIST)            *         
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
         TITLE 'T41C35 - BILL FORMULA RECORDS MAINTENANCE'                      
T41C35   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C35*,R7,RR=R3                                              
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
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
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
         XC    BFRKEY,BFRKEY       START BUILDING THE KEY                       
         LA    R4,BFRKEY                                                        
         USING PBFKEY,R4                                                        
         MVI   PBFKTYPE,PBFKTYPQ                                                
         MVC   PBFKAGY,AGENCY                                                   
*                                                                               
         LA    R2,BFRMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   PBFKMED,QMED                                                     
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
VK10     MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   PBFKCLT,QCLT                                                     
*                                                                               
         XC    WORK,WORK           GET B1X PROFILE                              
         XC    PROFB1X,PROFB1X                                                  
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
**       SETTING OFFICE CODE HAS ITS DANGERS                                    
**       THIS CONTROL SHOULD BE SET BY CLIENT                                   
**       DANGEROUS TO ALLOW BY OFFICE (OR HIGHER)                               
**       AS BILLING WILL USE ITS DEFAULT FORMULA                                
**       SHOULD ONE FROM AN SFM BILLING FORMULA RECORD NOT BE FOUND             
**       CODE BELOW WAS COPIED FROM PRSFM17 (BILLFORM)                          
**       WHERE IT HAD BEEN NO-OPED BACK IN 1999                                 
**                                                                              
         L     RE,AIO2           CLIENT HEADER SHOULD BE THERE                  
         USING PCLTRECD,RE                                                      
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         DROP  RE                                                               
**                                                                              
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
         CLI   ACTNUM,ACTREP       OR REPORT CAN BE BLANK                       
         BE    VK30                                                             
         B     ERRMIS                                                           
*                                                                               
VK20     CLC   =C'ALL',BFRPRD                                                   
         BNE   *+14                                                             
         MVC   PBFKPRD,=C'AAA'     PRD ALL => AAA                               
         B     VK30                                                             
*                                                                               
         CLC   =C'ZZZ',BFRPRD      ZZZ  IS INVALID PRODUCT                      
         BE    ERRINV                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   PBFKPRD,QPRD                                                     
*                                                                               
VK30     LA    R2,BFRESTH                                                       
         XC    BEST,BEST                                                        
         CLI   5(R2),0                                                          
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP       OR REPORT CAN BE BLANK                       
         BE    VKX                                                              
         B     ERRINV                                                           
*                                                                               
VK35     CLC   =C'ALL',BFREST                                                   
         BE    VKX                                                              
*                                                                               
         CLI   BFRPRDH+5,0         PRODUT ENTERED ?                             
         BE    *+14                NO, THIS IS A FILTER                         
         CLC   PBFKPRD,=C'AAA'     IF PRD  NEQ ALL OR AAA                       
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
         STH   R1,PBFKEST                                                       
         B     VKX                                                              
*                                                                               
VK37     MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
         L     R6,AIO2                                                          
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,(0,PESTST),(8,BFRESTD)                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(8,BFRESTD+9)                            
         MVI   BFRESTD+8,C'-'                                                   
         OI    BFRESTDH+6,X'80'                                                 
         MVC   SVESTSDT,PESTST                                                  
         MVC   SVESTNDT,PESTEND                                                 
         MVC   PBFKEST,BEST                                                     
         DROP  R6                                                               
*                                                                               
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   KEY(L'PBFKEY),BFRKEY                                             
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
         USING PBFKEY,R3                                                        
         MVC   PBFKEY,KEY          WILL BE REBUILDING REC                       
         XCEFL 33(R3),1000                                                      
         MVC   PBFRLEN,DATADISP                                                 
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
         USING PBFRCELD,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PBFRCEL,PBFRCELQ    ELEMENT CODE                                 
         MVI   PBFRCLEN,PBFRCLNQ   ELEMENT LENGTH                               
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   VR20                                                             
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         XC    PVALBSTA,PVALBSTA                                                
         DROP  R1                                                               
         B     VR30                                                             
*                                                                               
VR20     CLI   PROFB1X+12,C'Y'     DOES B1X ALLOW FOR MOS?                      
         BNE   ERRINV                                                           
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
VR30     MVC   PBFRCDTE,PVALBSTA                                                
         XC    PBFRCDTE,=2X'FF'                                                 
         OC    PBFKEST,PBFKEST     IF EST=ALL                                   
         BZ    VR40                                                             
         CLC   =C'AAA',PBFKPRD     OR PRD=ALL                                   
         BE    VR40                                                             
         OC    PVALBSTA,PVALBSTA   OR NO DATE                                   
         BZ    VR40                                                             
         DROP  R1                                                               
*                                                                               
VR40     GOTO1 FINDMNTH,DMCB,PBFRCDTE                                           
         BE    RECXISTS                                                         
*                                                                               
         LA    R2,LINFMLH          BILL FORMULA                                 
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ERRMIS                                                           
*                                  GO AND VALIDATE FORMULA                      
         GOTO1 VALIFML,DMCB,(R2)                                                
*                                                                               
         MVC   PBFRCFML,BFORMULA                                                
*                                                                               
         XC    BACPCT,BACPCT        MUST CLEAR THESE HERE                       
         XC    BACOF,BACOF                                                      
*                                                                               
         CLI   BFORMULA+1,X'08'     SEE IF BASE B IS AC                         
         BNE   VR55                 DISALLOW ENTRIES IN THOSE FIELDS            
*                                                                               
         LA    R2,LINACPH                                                       
         CLI   5(R2),0         ANY DATA IN PCT. FIELD?                          
         BNE   VR50            YES                                              
         LA    R2,LINACOH      IF NOT THEN NONE CAN BE IN 'OF' FIELD            
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
         B     VR90            NO DATA IN EITHER                                
*                                                                               
VR50     DS    0H              EDIT AC PCT.                                     
*                                                                               
         GOTO1 VALIACP,DMCB,(R2)                                                
*                                                                               
         MVC   PBFRCACP,BACPCT                                                  
         LA    R2,LINACOH                                                       
         CLI   5(R2),0        ANY INPUT IN 'OF' FIELD?                          
         BE    ERRMIS             NEEDED IF % WAS ENTERED                       
*                                                                               
*                             EDIT AC PCT. OF FIELD                             
         GOTO1 VALIACO,DMCB,(R2)                                                
         MVC   PBFRCACO,BACOF                                                   
         B     VR90                                                             
*                                                                               
VR55     DS    0H          NO INPUT ALLOWED UNLESS BASE B IS AC                 
         LA    R2,LINACPH                                                       
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
*                                                                               
         LA    R2,LINACOH                                                       
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
*                                                                               
VR90     GOTO1 ADDELEM                                                          
*                                                                               
VRNXT    LA    R5,LINNEXTL                                                      
         BCT   R4,VR10                                                          
*                                                                               
VRX      CLC   PBFRLEN,DATADISP     ANY ELEMENTS IN REC ?                       
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
         MVI   ELCODE,PBFRCELQ                                                  
         BAS   RE,GETEL                                                         
         BE    DR20                FORMULA ELEMENT FOUND                        
         B     DRX          IF NOT, JUST EXIT - DON'T DUMP ANY MORE             
*                                                                               
         USING PBFRCELD,R6                                                      
*                                                                               
DR20     MVC   LINMOS,SPACES                                                    
         MVI   LINMOS,C'*'         MARKER FOR NO DATE                           
         MVI   LINMOSH+5,1                                                      
         CLC   PBFRCDTE,=2X'FF'                                                 
         BE    DR40                                                             
*                                                                               
         MVC   FULL,PBFRCDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LINMOS)                                  
         MVI   LINMOSH+5,6                                                      
*                                                                               
DR40     DS    0H                                                               
         OI    LINMOSH+6,X'80'                                                  
         MVC   BFORMULA,PBFRCFML                                                
         GOTO1 DISPFML,DMCB,LINFML                                              
         OI    LINFMLH+6,X'80'                                                  
*                                                                               
         CLI   PBFRCACO,X'01'    GROSS?                                         
         BE    DR50                                                             
         CLI   PBFRCACO,X'02'    NET?                                           
         BE    DR50                                                             
         CLI   PBFRCACO,X'05'    GROSS-CD                                       
         BE    DR50                                                             
         CLI   PBFRCACO,X'06'    NET-CD                                         
         BE    DR50                                                             
         XC    PBFRCACP,PBFRCACP    THEY WERE BAD-SO CLEAR                      
         XC    PBFRCACO,PBFRCACO    (COMMENT FROM OLD BILLFORM)                 
*                                                                               
DR50     MVC   BACPCT,PBFRCACP                                                  
         GOTO1 DISPACP,DMCB,LINACP                                              
         OI    LINACPH+6,X'80'                                                  
*                                                                               
         MVC   BACOF,PBFRCACO                                                   
         GOTO1 DISPACO,DMCB,LINACO                                              
         OI    LINACOH+6,X'80'                                                  
*                                                                               
         LA    R2,LINNEXTL         R2 = A(NEXT LINE ON SCREEN)                  
         LA    R1,BFRLAST                                                       
         CR    R2,R1               PASSED LAST LINE?                            
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
         USING  PBFKEY,R6                                                       
*                                                                               
         MVC   BFRMED,PBFKMED                                                   
*                                                                               
         OI    BFRMEDH+6,X'80'      TRANSMIT MEDIA CODE TO SCREEN               
         MVI   BFRMEDH+5,1                                                      
*                                                                               
         MVC   BFRCLT,PBFKCLT                                                   
         OI    BFRCLTH+6,X'80'                                                  
         MVI   BFRCLTH+5,3          TRANSMIT CLIENT CODE TO SCREEN              
         CLI   BFRCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   BFRCLTH+5,2                                                      
*                                                                               
         MVC   BFRPRD,PBFKPRD                                                   
         CLC   BFRPRD(3),=C'AAA'   AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   BFRPRD(3),=C'ALL'                                                
         MVI   BFRPRDH+5,3                                                      
         CLI   BFRPRD+2,C' '                                                    
         BNE   *+8                                                              
         MVI   BFRPRDH+5,2                                                      
         OI    BFRPRDH+6,X'80'                                                  
*                                                                               
         OC    PBFKEST,PBFKEST                                                  
         BNZ   DK40                                                             
         MVC   BFREST(3),=C'ALL'                                                
         MVI   BFRESTH+5,3                                                      
         OI    BFRESTH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK40     EDIT  (B2,PBFKEST),BFREST,FILL=0                                       
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
         USING PBFKEY,R4                                                        
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
         MVC   KEY(L'BFRKEY),BFRKEY                                             
*                                                                               
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
LR20     CLC   KEY(4),BFRKEY       ANY MORE BILLFRM RECS FOR THAT MEDIA         
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
         MVC   LSTCLT,PBFKCLT                                                   
         MVC   LSTPRD,PBFKPRD                                                   
         CLC   LSTPRD,=C'AAA'      AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   LSTPRD,=C'ALL'                                                   
*                                                                               
         OC    PBFKEST,PBFKEST                                                  
         BNZ   *+14                                                             
         MVC   LSTEST,=C'ALL'                                                   
         B     LR30                                                             
*                                                                               
         EDIT  PBFKEST,LSTEST,FILL=0                                            
*                                                                               
*                                                                               
LR30     L     R6,AIO                                                           
         MVI   ELCODE,PBFRCELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR15                                                             
*                                  SEE IF OTHER FORMULAS EXIST                  
         ST    R6,FELEM            SAVE ADDRESS OF FIRST                        
         BAS   RE,NEXTEL                                                        
         BNE   LR40                                                             
         MVC   LSTBFR(16),=C'*** MULTIPLE ***'                                  
         B     LR90                                                             
*                                                                               
LR40     L     R6,FELEM                                                         
         USING PBFRCELD,R6                                                      
         MVC   LSTMOS,SPACES                                                    
         MVI   LSTMOS,C'*'         MARKER FOR NO DATE                           
         CLC   PBFRCDTE,=2X'FF'                                                 
         BE    LR50                                                             
*                                                                               
         MVC   FULL,PBFRCDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LSTMOS)                                  
*                                                                               
LR50     MVC   BFORMULA,PBFRCFML                                                
         GOTO1 DISPFML,DMCB,LSTBFR                                              
*                                                                               
         CLI   PBFRCACO,X'01'    GROSS?                                         
         BE    LR60                                                             
         CLI   PBFRCACO,X'02'    NET?                                           
         BE    LR60                                                             
         CLI   PBFRCACO,X'05'    GROSS-CD                                       
         BE    LR60                                                             
         CLI   PBFRCACO,X'06'    NET-CD                                         
         BE    LR60                                                             
         XC    PBFRCACP,PBFRCACP    THEY WERE BAD-SO CLEAR                      
         XC    PBFRCACO,PBFRCACO    (COMMENT FROM OLD BILLFORM)                 
                                                                                
*                                                                               
LR60     MVC   BACPCT,PBFRCACP                                                  
         GOTO1 DISPACP,DMCB,LSTACP                                              
*                                                                               
         MVC   BACOF,PBFRCACO                                                   
         GOTO1 DISPACO,DMCB,LSTACO                                              
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
         USING PBFKEY,R4                                                        
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   PR10                                                             
         MVC   KEY(L'BFRKEY),BFRKEY                                             
*                                                                               
PR10     GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR15     GOTO1 SEQ                                                              
PR20     CLC   KEY(4),BFRKEY       ANY MORE BILLFRM RECS FOR THAT MEDIA         
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
         MVC   PLSTCLT,PBFKCLT                                                  
         MVC   PLSTPRD,PBFKPRD                                                  
         CLC   PLSTPRD,=C'AAA'     AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   PLSTPRD,=C'ALL'                                                  
*                                                                               
         OC    PBFKEST,PBFKEST                                                  
         BNZ   *+14                                                             
         MVC   PLSTEST,=C'ALL'                                                  
         B     PR30                                                             
*                                                                               
         EDIT  PBFKEST,PLSTEST,FILL=0                                           
*                                                                               
*                                                                               
PR30     L     R6,AIO                                                           
         MVI   ELCODE,PBFRCELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR15                                                             
*                                                                               
PR40     DS    0H                                                               
         USING PBFRCELD,R6                                                      
         MVC   PLSTMOS,SPACES                                                   
         MVI   PLSTMOS,C'*'        MARKER FOR NO DATE                           
         CLC   PBFRCDTE,=2X'FF'                                                 
         BE    PR50                                                             
*                                                                               
         MVC   FULL,PBFRCDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,PLSTMOS)                                 
*                                                                               
PR50     MVC   BFORMULA,PBFRCFML                                                
         GOTO1 DISPFML,DMCB,PLSTBFR                                             
*                                                                               
         CLI   PBFRCACO,X'01'    GROSS?                                         
         BE    PR60                                                             
         CLI   PBFRCACO,X'02'    NET?                                           
         BE    PR60                                                             
         CLI   PBFRCACO,X'05'    GROSS-CD                                       
         BE    PR60                                                             
         CLI   PBFRCACO,X'06'    NET-CD                                         
         BE    PR60                                                             
         XC    PBFRCACP,PBFRCACP    THEY WERE BAD-SO CLEAR                      
         XC    PBFRCACO,PBFRCACO    (COMMENT FROM OLD BILLFORM)                 
                                                                                
*                                                                               
PR60     MVC   BACPCT,PBFRCACP                                                  
         GOTO1 DISPACP,DMCB,PLSTACP                                             
*                                                                               
         MVC   BACOF,PBFRCACO                                                   
         GOTO1 DISPACO,DMCB,PLSTACO                                             
*                                                                               
*                                                                               
PR90     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BNE   PR15                NO MORE  - GET NEXT RECORD                   
         MVC   PLSTCLT,PBFKCLT                                                  
         MVC   PLSTPRD,PBFKPRD                                                  
         CLC   PLSTPRD,=C'AAA'     AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   PLSTPRD,=C'ALL'                                                  
*                                                                               
         OC    PBFKEST,PBFKEST                                                  
         BNZ   *+14                                                             
         MVC   PLSTEST,=C'ALL'                                                  
         B     PR95                                                             
*                                                                               
         EDIT  PBFKEST,PLSTEST,FILL=0                                           
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
         USING PBFREC,R4                    FILTER BY:                          
*                                                                               
         OC    PBFKCLT,PBFKCLT              CLIENT?                             
         BZ    FLT10                                                            
         CLC   PBFKCLT,KEY+PBFKCLT-PBFKEY                                       
         BNE   FLT100                                                           
*                                                                               
FLT10    OC    PBFKPRD,PBFKPRD              PRODUCT?                            
         BZ    FLT20                                                            
         CLC   PBFKPRD,KEY+PBFKPRD-PBFKEY                                       
         BNE   FLT100                                                           
*                                                                               
FLT20    OC    PBFKEST,PBFKEST              ESTIMATE?                           
         BE    FLTX                                                             
         CLC   PBFKEST,KEY+PBFKEST-PBFKEY                                       
         BE    *+8                                                              
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
B1XINCMP EQU   86                                                               
ESTRNGE  EQU   97                                                               
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FIND THE FORMULA ELEMENT IN THE RECORD FOR A SPECIFIC MONTH                   
*                                                                               
* ON ENTRY:    PARAMETER 1         A(MONTH COMPLEMENTED WITH X'FF')             
*                                                                               
***********************************************************************         
FINDMNTH NTR1                                                                   
         L     R1,DMCB                                                          
         MVC   HALF,0(R1)          SAVE MONTH WE LOOKING FOR                    
*                                                                               
         L     R6,AIO              LOOKING FOR THE FORMULA ELEMENT              
         MVI   ELCODE,PBFRCELQ                                                  
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
FMTH10   BAS   RE,NEXTEL                                                        
         BNE   FMTHNO              NO MORE FORMULA ELEMENTS                     
*                                                                               
         USING PBFRCELD,R6                                                      
         CLC   HALF,PBFRCDTE       IF WE FOUND A MATCH                          
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
         EJECT                                                                  
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
         BNE   VFMLBB12                                                         
         OI    BBILLBAS,X'01'        GROSS                                      
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB12 DS    0H                                                               
         EX    R4,NETCOM                                                        
         BNE   VFMLBB14                                                         
         OI    BBILLBAS,X'02'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB14 DS    0H                                                               
         EX    R4,GRCDCOM         GROSS-CD                                      
         BNE   VFMLBB15                                                         
         OI    BBILLBAS,X'05'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB15 DS    0H                                                               
         EX    R4,GLCDCOM        GLCD                                           
         BNE   VFMLBB16                                                         
         OI    BBILLBAS,X'05'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB16 DS    0H                                                               
         EX    R4,GLCDCOM2       G-CD                                           
         BNE   VFMLBB17                                                         
         OI    BBILLBAS,X'05'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB17 DS    0H                                                               
         EX    R4,NETCDCOM         NET-CD                                       
         BNE   VFMLBB18                                                         
         OI    BBILLBAS,X'06'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB18 DS    0H                                                               
         EX    R4,NLCDCOM          NLCD                                         
         BNE   VFMLBB19                                                         
         OI    BBILLBAS,X'06'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB19 DS    0H                                                               
         EX    R4,NLCDCOM2          N-CD                                        
         BNE   VFMLBB20                                                         
         OI    BBILLBAS,X'06'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB20 DS    0H                                                               
         EX    R4,ACCOM            AGYCOM                                       
         BNE   VFMLBB21                                                         
         OI    BBILLBAS,X'08'                                                   
         B     VFMLCPCT                                                         
*                                                                               
VFMLBB21 DS    0H                  AC                                           
         EX    R4,ACCOM2                                                        
         BNE   VFMLINVL                                                         
         OI    BBILLBAS,X'08'                                                   
         B     VFMLCPCT                                                         
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
VFMLCP20 DS    0H                                                               
         ST    R0,FULL                                                          
         MVC   BBILLCOM(3),FULL+1                                               
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
         BNE   VFMLCB12                                                         
         OI    BBILLBAS+1,X'01'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB12 DS    0H                                                               
         EX    R4,NETCOM                                                        
         BNE   VFMLCB14                                                         
         OI    BBILLBAS+1,X'02'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB14 DS    0H                                                               
         EX    R4,GRCDCOM                                                       
         BNE   VFMLCB15                                                         
         OI    BBILLBAS+1,X'05'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB15 DS    0H                                                               
         EX    R4,GLCDCOM          GLCD                                         
         BNE   VFMLCB16                                                         
         OI    BBILLBAS+1,X'05'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB16 DS    0H                                                               
         EX    R4,GLCDCOM2         G-CD                                         
         BNE   VFMLCB17                                                         
         OI    BBILLBAS+1,X'05'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB17 DS    0H                                                               
         EX    R4,NETCDCOM                                                      
         BNE   VFMLCB18                                                         
         OI    BBILLBAS+1,X'06'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB18 DS    0H                                                               
         EX    R4,NLCDCOM        NLCD                                           
         BNE   VFMLCB19                                                         
         OI    BBILLBAS+1,X'06'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB19 DS    0H                                                               
         EX    R4,NLCDCOM2       N-CD                                           
         BNE   VFMLCB20                                                         
         OI    BBILLBAS+1,X'06'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB20 DS    0H                                                               
         EX    R4,ACCOM          AGYCOM                                         
         BNE   VFMLCB21                                                         
         OI    BBILLBAS+1,X'08'                                                 
         B     VFML10                                                           
*                                                                               
VFMLCB21 DS    0H                                                               
         EX    R4,ACCOM2          AC                                            
         BNE   VFMLINVL                                                         
         OI    BBILLBAS+1,X'08'                                                 
         B     VFML10                                                           
*                                                                               
VFML10   DS    0H                                                               
*                                                                               
VFMLX    B     XIT                                                              
*                                                                               
GROSCOM  CLC   0(0,R5),=C'GROSS'                                                
NETCOM   CLC   0(0,R5),=C'NET  '                                                
GRCDCOM  CLC   0(0,R5),=C'GROSS-CD'                                             
GLCDCOM  CLC   0(0,R5),=C'GLCD    '                                             
GLCDCOM2 CLC   0(0,R5),=C'G-CD    '                                             
NETCDCOM CLC   0(0,R5),=C'NET-CD'                                               
NLCDCOM  CLC   0(0,R5),=C'NLCD  '                                               
NLCDCOM2 CLC   0(0,R5),=C'N-CD  '                                               
ACCOM    CLC   0(0,R5),=C'AGYCOM'                                               
ACCOM2   CLC   0(0,R5),=C'AC    '                                               
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
VALIACP  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         XC    BACPCT,BACPCT       CLEAR AC PCT FIELD                           
         OC    BFORMULA+2(3),BFORMULA+2  ADJUSTMENT MUST BE PRESENT             
         BZ    VACPINVL                                                         
*                                                                               
*                                                                               
VACP10   ZIC   R0,5(R2)            VALIDATE THE PERCENTAGE                      
         BCTR  R0,0                                                             
         GOTO1 CASHVAL,DMCB,(4,9(R2)),(R0)                                      
         CLI   DMCB,X'FF'                                                       
         BE    VACPINVL                                                         
*                                                                               
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'      MUST BE A PERCENTAGE (0 > X >=100)           
         BH    VACPINVL                                                         
         C     R0,=F'0'                                                         
         BNH   VACPINVL                                                         
*                                                                               
         CLI   8(R2),C'+'                                                       
         BE    VACP20                                                           
         CLI   8(R3),C'-'                                                       
         BNE   VACPINVL            ERROR IF 1ST BYTE IS NEITHER + NOR -         
         LCR   R0,R0               MAKE NEGATIVE IF %-AGE IS NEGATIVE           
*                                                                               
VACP20   DS    0H                                                               
         ST    R0,FULL                                                          
         MVC   BACPCT(3),FULL+1                                                 
*                                                                               
         MVC   WORK(1),BFORMULA+2    SIGNS MUST MATCH                           
         MVC   WORK+1(1),BACPCT                                                 
         NI    WORK,X'80'            SET OFF ALL BUT SIGN                       
         NI    WORK+1,X'80'          SET OFF ALL BUT SIGN                       
         CLC   WORK(1),WORK+1                                                   
         BNE   VACPINVL                                                         
*                                                                               
*                                                                               
VACPX    B     XIT                                                              
*                                                                               
VACPMISS MVI   ERROR,MISSING                                                    
         B     *+8                                                              
VACPINVL MVI   ERROR,INVALID        R2 SHOULD BE AT FIELD                       
         B     VSFMERR                                                          
         EJECT                                                                  
***********************************************************************         
VALIACO  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         XC    BACOF,BACOF        CLEAR AC PCT OF FIELD                         
*                                                                               
VACO10   ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BNE   VACO12                                                           
         OI    BACOF,X'01'                                                      
         B     VACOX                                                            
*                                                                               
VACO12   DS    0H                                                               
         EX    R4,NETCOM                                                        
         BNE   VACO14                                                           
         OI    BACOF,X'02'                                                      
         B     VACOX                                                            
*                                                                               
VACO14   DS    0H                                                               
         EX    R4,GRCDCOM                                                       
         BNE   VACO15                                                           
         OI    BACOF,X'05'                                                      
         B     VACOX                                                            
*                                                                               
VACO15   DS    0H                                                               
         EX    R4,GLCDCOM          GLCD                                         
         BNE   VACO16                                                           
         OI    BACOF,X'05'                                                      
         B     VACOX                                                            
*                                                                               
VACO16   DS    0H                                                               
         EX    R4,GLCDCOM2         G-CD                                         
         BNE   VACO17                                                           
         OI    BACOF,X'05'                                                      
         B     VACOX                                                            
*                                                                               
VACO17   DS    0H                                                               
         EX    R4,NETCDCOM                                                      
         BNE   VACO18                                                           
         OI    BACOF,X'06'                                                      
         B     VACOX                                                            
*                                                                               
VACO18   DS    0H                                                               
         EX    R4,NLCDCOM        NLCD                                           
         BNE   VACO19                                                           
         OI    BACOF,X'06'                                                      
         B     VACOX                                                            
*                                                                               
VACO19   DS    0H                                                               
         EX    R4,NLCDCOM2       N-CD                                           
         BNE   VACO20                                                           
         OI    BACOF,X'06'                                                      
         B     VACOX                                                            
*                                                                               
VACO20   B     VACOINVL          INVALID                                        
*                                                                               
VACOX    B     XIT                                                              
*                                                                               
VACOMISS MVI   ERROR,MISSING                                                    
         B     *+8                                                              
VACOINVL MVI   ERROR,INVALID       R2 SHOULD BE AT FIELD                        
         B     VSFMERR                                                          
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS A FORMULA                                               
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD)                             
*              BFORMULA            FORMULA TO BE DISPLAYED                      
***********************************************************************         
DISPFML  NTR1                                                                   
         L     R3,0(R1)            R3 = A(1ST BYTE IN FORMULA TEXT)             
         XC    0(L'BFRFML1,R3),0(R3)                                            
*                                                                               
*                                                                               
         TM    BBILLBAS,X'41'      FIGURE OUT WHICH BILL BASIS WE HAVE          
         BNO   DFML10                                                           
         MVC   0(6,R3),=C'CGROSS'                                               
         LA    R3,6(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML10   TM    BBILLBAS,X'42'                                                   
         BNO   DFML20                                                           
         MVC   0(4,R3),=C'CNET'                                                 
         LA    R3,4(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML20   TM    BBILLBAS,X'45'                                                   
         BNO   DFML25                                                           
         MVC   0(9,R3),=C'CGROSS-CD'                                            
         LA    R3,9(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML25   TM    BBILLBAS,X'46'                                                   
         BNO   DFML27                                                           
         MVC   0(7,R3),=C'CNET-CD'                                              
         LA    R3,7(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML27   TM    BBILLBAS,X'48'                                                   
         BNO   DFML29                                                           
         MVC   0(7,R3),=C'CAGYCOM'                                              
         LA    R3,7(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML29   TM    BBILLBAS,X'05'                                                   
         BNO   DFML31                                                           
         MVC   0(8,R3),=C'GROSS-CD'                                             
         LA    R3,8(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML31   TM    BBILLBAS,X'06'                                                   
         BNO   DFML33                                                           
         MVC   0(6,R3),=C'NET-CD'                                               
         LA    R3,6(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML33   TM    BBILLBAS,X'01'                                                   
         BNO   DFML35                                                           
         MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML35   TM    BBILLBAS,X'02'                                                   
         BNO   DFML37                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML37   DC    H'0'                BAD BASIS                                    
*                                                                               
DFML50   OC    BBILLCOM,BBILLCOM   IF NO COMMISSION                             
         BZ    DFMLX               THEN JUST BILL BASIS                         
*                                                                               
         MVI   0(R3),C','          OTHERWISE SEPARATE FIELD WITH COMMAS         
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (B3,BBILLCOM),(8,1(R3)),4,ALIGN=LEFT                             
         MVI   0(R3),C'+'                                                       
         TM    BBILLCOM,X'80'                                                   
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         LA    R1,8(R3)                                                         
DFML65   CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     DFML65                                                           
*                                                                               
         LA    R3,1(R1)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DFML66   TM    BBILLBAS+1,X'08'                                                 
         BNO   DFML67                                                           
         MVC   0(6,R3),=C'AGYCOM'                                               
         LA    R3,6(R3)                                                         
         B     DFML80                                                           
*                                                                               
*                                                                               
DFML67   TM    BBILLBAS+1,X'05'                                                 
         BNO   DFML69                                                           
         MVC   0(8,R3),=C'GROSS-CD'                                             
         LA    R3,8(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML69   TM    BBILLBAS+1,X'06'                                                 
         BNO   DFML71                                                           
         MVC   0(6,R3),=C'NET-CD'                                               
         LA    R3,6(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML71   TM    BBILLBAS+1,X'01'                                                 
         BNO   DFML73                                                           
         MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML73   TM    BBILLBAS+1,X'02'                                                 
         BNO   DFML75                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML75   DC    H'0'                BAD BASIS                                    
*                                                                               
DFML80   DS    0H                                                               
*                                                                               
DFMLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS AC PCT                                                  
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD)                             
*              BACPCT              AC PCT TO BE DISPLAYED                       
***********************************************************************         
DISPACP  NTR1                                                                   
         L     R3,0(R1)            R3 = A(1ST BYTE IN AC PCT)                   
         XC    0(L'BFRACP1,R3),0(R3)                                            
*                                                                               
DACP50   OC    BACPCT,BACPCT       SEE IF I HAVE                                
         BZ    DACPX               THEN JUST BILL BASIS                         
*                                                                               
         EDIT  (B3,BACPCT),(8,1(R3)),4,ALIGN=LEFT                               
         MVI   0(R3),C'+'                                                       
         TM    BACPCT,X'80'                                                     
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
DACPX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS AC PCT OF                                               
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD)                             
*              BACOF               AC PCT OF TO BE DISPLAYED                    
***********************************************************************         
DISPACO  NTR1                                                                   
         L     R3,0(R1)            R3 = A(1ST BYTE IN AC PCT)                   
         XC    0(L'BFRACO1,R3),0(R3)                                            
*                                                                               
DACO50   OC    BACOF,BACOF      SEE IF I HAVE                                   
         BZ    DACOX               THEN JUST BILL BASIS                         
*                                                                               
*                                                                               
DACO66   TM    BACOF,X'08'                                                      
         BNO   DACO67                                                           
         MVC   0(6,R3),=C'AGYCOM'                                               
         B     DACO80                                                           
*                                                                               
*                                                                               
DACO67   TM    BACOF,X'05'                                                      
         BNO   DACO69                                                           
         MVC   0(8,R3),=C'GROSS-CD'                                             
         B     DACO80                                                           
*                                                                               
DACO69   TM    BACOF,X'06'                                                      
         BNO   DACO71                                                           
         MVC   0(6,R3),=C'NET-CD'                                               
         B     DACO80                                                           
*                                                                               
DACO71   TM    BACOF,X'01'                                                      
         BNO   DACO73                                                           
         MVC   0(5,R3),=C'GROSS'                                                
         B     DACO80                                                           
*                                                                               
DACO73   TM    BACOF,X'02'                                                      
         BNO   DACO75                                                           
         MVC   0(3,R3),=C'NET'                                                  
         B     DACO80                                                           
*                                                                               
DACO75   DC    H'0'                BAD ACT PCT OF BASIS                         
*                                                                               
DACO80   DS    0H                                                               
*                                                                               
DACOX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,52,C' PRINT BFORM REPORT '                                    
         SSPEC H2,52,C' ------------------ '                                    
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CLT  PRD  EST  START MOS  FORMULA'                        
         SSPEC H8,1,C'---  ---  ---  ---------  -------'                        
         SSPEC H7,57,C'SHOW AC AS % OF'                                         
         SSPEC H8,57,C'---------------'                                         
*                                                                               
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMDBD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE PPBILLFML                                                      
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
ATIOB    DS    A                                                                
FELEM    DS    A                   ADDRESS OF FIRST FORMULA ELEMENT             
*                                  WHEN LISTING                                 
*                                                                               
SVESTPER DS    0CL12               ESTIMATE PERIOD                              
SVESTSDT DS    CL6                 ESTIMATE START DATE (YYMMDD)                 
SVESTNDT DS    CL6                 ESTIMATE END DATE   (YYMMDD)                 
FMLFLAG  DS    X                                                                
FILTFLAG DS    X                                                                
FLTSKIP  EQU   X'80'                                                            
BFRKEY   DS    CL25                                                             
SAVEKEY  DS    CL25                                                             
MYESTART DS    CL6                                                              
MYEEND   DS    CL6                                                              
SUBMED   DS    CL1                 SUB MEDIA TYPE                               
SMEDH    DS    CL9                                                              
PROFB1X  DS    CL16                                                             
*                                                                               
BFORMULA DS    0XL(L'PBFRCFML)     BINARY REPRESENTATION OF FORMULA             
BBILLBAS DS    XL(L'BILBASA+L'BILBASB)                                          
*                                                                               
BBILLCOM DS    XL(L'BILADJ)        SIGNED COMMISSION RATE (99.9999)             
*                                                                               
BACPCT   DS    XL(L'PBFRCACP)       'SHOW AC AS PCT'                            
BACOF    DS    XL(L'PBFRCACO)       'SHOW AC AS PCT. OF'                        
*                                                                               
ERRNUM   DS    XL2                                                              
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
LINACPH  DS    CL(L'BFRACP1H)                                                   
LINACP   DS    CL(L'BFRACP1)                                                    
LINACOH  DS    CL(L'BFRACO1H)                                                   
LINACO   DS    CL(L'BFRACO1)                                                    
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
LSTBFR   DS    CL29                                                             
         DS    CL1                                                              
LSTACP   DS    CL10                                                             
         DS    CL1                                                              
LSTACO   DS    CL10                                                             
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
PLSTBFR  DS    CL29                                                             
         DS    CL1                                                              
PLSTACP  DS    CL10                                                             
         DS    CL1                                                              
PLSTACO  DS    CL10                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DEDBLOCK                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPERVALD                                                      
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PRSFM35   04/02/19'                                      
         END                                                                    
