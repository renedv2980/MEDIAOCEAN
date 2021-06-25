*          DATA SET PRSFM17    AT LEVEL 023 AS OF 06/22/05                      
*PHASE T41C17A                                                                  
***********************************************************************         
*                                                                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*  BPLA   12/04   BE SURE ELEM IS CLEARED                                       
*                                                                               
*  BPLA   06/04   ADD CODE FOR 'SHOW AC AS %...'                                
*                                                                               
*  BPLA   11/03   NO-OP ESTIMATE DATE CHECK                                     
*                 (MOS MAY BE OUTSIDE OF ESTIMATE DATES)                        
*                 CHECK MAY HAVE BEEN NEEDED FOR SPOT/NET                       
*                                                                               
*  BPLA   1/99    NO-OP OFFICE IN GETPROF CALL                                  
*                                                                               
*  BPLA   1/99    SET OFFICE IN CALL TO GETPROF                                 
*                                                                               
*  BPLA  11/98    CHANGE TO ALLOW ALTERNATE SPELLINGS                           
*                 G-CD AND GLCD FOR GROSS LESS CD                               
*                 N-CD AND NLCD FOR NET LESS CD                                 
*                                                                               
*  BPLA 2/21/95   FOR PRINT CHECK FOR ZZZ NOT POL                               
*                                                                               
*  TITLE: PRSFM17 - T41C17 BILL FORMULA RECORDS MAINTENANCE                     
*                                                                               
*  CALLED FROM: SFM CONTROLLER (T41C00), WHICH CALLS                            
*               GEGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  INPUTS: SCREENS PRSFMD7 (T41CD7) -- MAINTENANCE SCREEN                       
*                                                                               
*  OUTPUTS: A LIST OF BILL FORMULA RECORDS                                      
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - WORK                                                            
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
*                                                                               
*         PRD, EST, CAN BE 'ALL'. THIS TRANSLATES TO                            
*         AAA FOR PRODUCT, NULL FOR EST                                         
*                                                                               
***********************************************************************         
         TITLE 'PRSFM17 - BILL FORMULA RECORDS MAINTENANCE OVERLAY'             
T41C17   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C17*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R1,SYSPARMS                                                      
         MVC   ATIOB,0(R1)                                                      
*                                                                               
         OI    CONSERVH+6,X'81'    CHANGE THIS FIELD TO MODIFIED                
         MVI   IOOPT,C'Y'          WE'LL DO OUR OWN I/O'S                       
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY ROUTINE                                                          
***********************************************************************         
VKEY     DS    0H                                                               
         NI    BITFLAG1,X'FF'-B1KEYCHG                                          
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  LA    R2,BILMEDH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    BITFLAG1,B1KEYCHG   YES                                          
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
VKMEDX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKCLT00  LA    R2,BILCLTH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    BITFLAG1,B1KEYCHG   YES                                          
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         XC    WORK,WORK           GET B1X PROFILE                              
         XC    PROFB1X,PROFB1X                                                  
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
**                                                                              
**       SETTING OFFICE CODE NO-OPED                                            
**       THIS CONTROL SHOULD BE SET BY CLIENT                                   
**       VERY DANGEROUS TO ALLOW BY OFFICE (OR HIGHER)                          
**                                                                              
**       L     RE,AIO1           CLIENT HEADER SHOULD BE THERE                  
**       USING PCLTRECD,RE                                                      
**       CLI   PCLTOFF,C' '                                                     
**       BNH   *+14                                                             
**       MVI   WORK+10,C'*'                                                     
**       MVC   WORK+11(1),PCLTOFF                                               
**       DROP  RE                                                               
**                                                                              
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
*                                                                               
VKCLTX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
*****                                                                           
* VALIDATE THE PRODUCT IF ANY                                                   
*****                                                                           
VKPRD00  LA    R2,BILPRDH                                                       
         NI    FLTRFLG1,X'FF'-FLTR1PRD                                          
*                                                                               
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    BITFLAG1,B1KEYCHG   YES                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKPRDX                                                           
*                                                                               
         CLC   =C'ALL',BILPRD      PRD ALL => AAA                               
         BNE   *+16                                                             
         MVC   BILPRD(3),=C'AAA'                                                
         MVC   QPRD,=C'AAA'                                                     
         CLC   BILPRD(3),=C'AAA'   FOR AAA SKIP VALIPRD                         
         BE    VKPRD06                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
VKPRD06  DS    0H                                                               
         MVC   FILTRPRD,QPRD                                                    
         OI    FLTRFLG1,FLTR1PRD                                                
*                                                                               
VKPRDX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
*****                                                                           
* VALIDATE THE ESTIMATE IF ANY                                                  
*****                                                                           
VKEST00  LA    R2,BILESTH                                                       
         NI    FLTRFLG1,X'FF'-FLTR1EST                                          
*                                                                               
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    BITFLAG1,B1KEYCHG   YES                                          
*                                                                               
         XC    BILESTD,BILESTD     CLEAR ESTIMATE PERIOD ON SCREEN              
         XC    BEST,BEST                                                        
         CLI   5(R2),0                                                          
         BE    VKESTX                                                           
*                                                                               
         CLC   =C'ALL',BILEST                                                   
         BE    VKEST02                                                          
         CLI   BILPRDH+5,0         NEED PRODUCT BEFORE WE CHECK EST             
         BNE   *+12                                                             
         LA    R2,BILPRDH                                                       
         B     MISSFLD                                                          
*                                                                               
VKEST02  DS    0H                                                               
         MVC   QEST,=C'ALL'                                                     
         XC    BEST,BEST                                                        
         CLC   BILEST(3),=C'ALL'                                                
         BE    VKEST04                                                          
         CLC   BILPRD(3),=C'AAA'   FOR PRD AAA WONT DO VALIEST                  
         BNE   VKEST03             SO NEED TO SET ESTIMATE                      
*                                                                               
         LA    R2,BILESTH                                                       
         ICM   RE,1,5(R2)          IF NO INPUT                                  
         BZ    VKEST03             LET VALIEST HAVE IT                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STH   RE,BEST                                                          
         B     VKEST04                                                          
*                                                                               
VKEST03  DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO              SAVE THE ESTIMATE PERIOD                     
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,(X'10',PESTST),(8,BILESTD)                           
         DROP  R6                                                               
*                                                                               
VKEST04  DS    0H                                                               
         MVC   FILTREST,BEST                                                    
         OI    FLTRFLG1,FLTR1EST                                                
*                                                                               
VKESTX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
         OI    BILESTDH+6,X'80'                                                 
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY ROUTINE                                                          
***********************************************************************         
VREC     DS    0H                                                               
         CLI   PROFB1X+11,C'N'     TEST BFR'S ALLOWED                           
         BE    *+12                                                             
         CLI   PROFB1X+11,0                                                     
         BNE   VR02                                                             
*                                                                               
         LA    R2,CONACTH                                                       
         B     B1XERR                                                           
*                                                                               
VR02     DS    0H                                                               
         TM    BITFLAG1,B1KEYCHG                                                
         BNZ   LREC                                                             
*****                                                                           
* VALIDATE THE FIRST LIST LINE                                                  
*****                                                                           
         LA    R3,BILPRD1H                                                      
         USING LINDSECT,R3                                                      
         TM    LINPRDH+4,X'20'     VALIDATED PREVIOUSLY?                        
         BZ    VR10                                                             
         TM    LINESTH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINMOSH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINFMLH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINACPH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINACOH+4,X'20'                                                  
         BNZ   VR100               TOP LINE HAS NOT BEEN CHANGED                
*                                                                               
VR10     CLI   LINPRDH+5,0         ANY DATA ON THE TOP LINE?                    
         BNE   VR20                                                             
         CLI   LINESTH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINMOSH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINFMLH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINACPH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINACOH+5,0                                                      
         BE    VR100               NONE, CHECK OTHER LINES                      
*                                                                               
VR20     LA    R2,LINPRDH          ALL FIELDS ON LINE ARE REQUIRED              
         CLI   5(R2),0             EXCEPT BILACPH AND BILACOH                   
         BNE   VR25                                                             
         CLI   BILPRDH+5,0                                                      
         BE    MISSFLD                                                          
*                                                                               
         MVC   4(4,R2),BILPRDH+4                                                
         MVC   8(L'BILPRD,R2),BILPRD                                            
*                                                                               
VR25     DS    0H                                                               
         CLC   =C'ZZZ',LINPRD      ZZZ NOT ALLOWED                              
         BE    INVLFLD                                                          
         CLC   =C'ALL',LINPRD      PRD ALL => AAA                               
         BNE   *+16                                                             
         MVC   LINPRD(3),=C'AAA'                                                
         MVC   QPRD,=C'AAA'                                                     
         CLC   LINPRD(3),=C'AAA'   FOR AAA SKIP VALIPRD                         
         BNE   VR27                                                             
         MVC   QPRD,=C'AAA'        STILL MUST SET IN QPRD                       
         B     VR30                                                             
*                                                                               
VR27     GOTO1 VALIPRD                                                          
*                                                                               
VR30     LA    R2,LINESTH                                                       
         CLI   5(R2),0                                                          
         BNE   VR35                                                             
         CLI   BILESTH+5,0                                                      
         BE    MISSFLD                                                          
*                                                                               
         MVC   4(4,R2),BILESTH+4                                                
         MVC   8(L'BILEST,R2),BILEST                                            
*                                                                               
VR35     DS    0H                                                               
         XC    BEST,BEST                                                        
         MVC   QEST,=C'ALL'                                                     
         CLC   =C'ALL',LINEST                                                   
         BE    VR40                                                             
         CLC   LINPRD(3),=C'AAA'   FOR PRD AAA WONT DO VALIEST                  
         BNE   VR38                SO NEED TO SET ESTIMATE                      
*                                                                               
         ICM   RE,1,5(R2)          IF NO INPUT                                  
         BZ    VR38                LET VALIEST HAVE IT                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STH   RE,BEST                                                          
         B     VR40                                                             
*                                                                               
VR38     DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO              SAVE THE ESTIMATE PERIOD                     
         USING ESTHDRD,R6                                                       
         MVC   SVESTSDT,PESTST                                                  
         MVC   SVESTNDT,PESTEND                                                 
         DROP  R6                                                               
*                                                                               
VR40     DS    0H                                                               
         B     VR46            SPOT HAD MARKET HERE                             
*                                                                               
VR46     DS    0H                                                               
         LA    R2,LINMOSH                                                       
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   8(R2),C'*'                                                       
         CLI   8(R2),C'*'                                                       
         BNE   VR46D                                                            
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         XC    PVALBSTA,PVALBSTA                                                
         B     VR46G                                                            
*                                                                               
VR46D    DS    0H                                                               
         CLI   PROFB1X+12,C'Y'     DOES B1X ALLOW FOR MOS?                      
         BNE   INVLFLD                                                          
         GOTO1 PERVAL,DMCB,(LINMOSH+5,LINMOS),PERVALST,0                        
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLFLD                                                          
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
* WAS THE START DAY OR ANY PART OF THE END DAY ENTERED?                         
         TM    PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                         
         BNO   INVLFLD                     YES, IT SHOULDN'T BE                 
*                                                                               
VR46G    DS    0H                                                               
         OC    BEST,BEST           IF EST=ALL                                   
         BZ    VR48                                                             
         CLC   LINPRD(3),=C'AAA'   OR PRD=ALL                                   
         BE    VR48                                                             
         OC    PVALBSTA,PVALBSTA   OR NO DATE                                   
         BZ    VR48                                                             
**NO-OP  CLC   PVALESTA(4),SVESTSDT   PERIOD SHOULD BE IN ESTIMATE'S            
**NO-OP  BL    OUTESTR                                                          
**NO-OP  CLC   PVALEEND(4),SVESTNDT                                             
**NO-OP  BH    OUTESTR                                                          
         DROP  R1                                                               
*                                                                               
VR48     DS    0H                                                               
         LA    R2,LINFMLH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLC   =C'DELETE',8(R2)                                                 
         BE    INVLFLD                                                          
*                                                                               
         GOTO1 VALIFML,DMCB,(R2)                                                
*                                                                               
*        IF BASE B WAS AC ALLOW DATA IN THE 'SHOW AC AS %..' FIELDS             
*                                                                               
VR50     XC    BACPCT,BACPCT        MUST CLEAR THESE HERE                       
         XC    BACOF,BACOF                                                      
*                                                                               
         CLI   BFORMULA+1,X'08'     SEE IF BASE B IS AC                         
         BNE   VR55       DISALLOW ENTRIES IN THOSE FIELDS                      
*                                                                               
         LA    R2,LINACPH                                                       
         CLI   5(R2),0         ANY DATA IN PCT. FIELD?                          
         BNE   VR50P           YES                                              
         LA    R2,LINACOH      IF NOT THEN NONE CAN BE IN 'OF' FIELD            
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
         B     VR55X           NO DATA IN EITHER                                
*                                                                               
VR50P    DS    0H              EDIT AC PCT.                                     
*                                                                               
         GOTO1 VALIACP,DMCB,(R2)                                                
*                                                                               
         LA    R2,LINACOH                                                       
         CLI   5(R2),0        ANY INPUT IN 'OF' FIELD?                          
         BE    MISSFLD            NEEDED IF % WAS ENTERED                       
*                                                                               
*                             EDIT AC PCT. OF FIELD                             
         GOTO1 VALIACO,DMCB,(R2)                                                
         B     VR55X                                                            
*                                                                               
VR55     DS    0H          NO INPUT ALLOWED UNLESS BASE B IS AC                 
         LA    R2,LINACPH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,LINACOH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
VR55X    LA    R4,ELEM             BUILD THE FORMULA ELEMENT                    
         XC    ELEM(30),ELEM       BE SURE IT'S CLEAR                           
         USING PBFRCELD,R4                                                      
         MVI   PBFRCEL,PBFRCELQ                                                 
         MVI   PBFRCLEN,PBFRCLNQ                                                
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   PBFRCDTE,PVALBSTA                                                
         XC    PBFRCDTE,=2X'FF'                                                 
         DROP  R1                                                               
         MVC   PBFRCFML,BFORMULA                                                
         MVC   PBFRCACP,BACPCT                                                  
         MVC   PBFRCACO,BACOF                                                   
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY             SEE IF THE RECORD EXISTED BEFORE             
         LA    R6,KEY                                                           
         USING PBFKEY,R6                                                        
         MVI   PBFKTYPE,PBFKTYPQ                                                
         MVC   PBFKAGY,AGENCY                                                   
         MVC   PBFKMED,QMED                                                     
         MVC   PBFKCLT,QCLT                                                     
         MVC   PBFKPRD,QPRD                                                     
         MVC   PBFKEST,BEST                                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PBFKEY),KEYSAVE   RECORD EXISTED BEFORE?                   
         BE    VR65                    YES, ADD THE ELEMENT TO RECORD           
*                                                                               
         L     R6,AIO                  NO, CREATE THE RECORD                    
         XC    0(50,R6),0(R6)                                                   
         USING PBFREC,R6                                                        
         MVC   0(L'PBFKEY,R6),KEYSAVE                                           
*                                                                               
         GOTO1 ADDELEM             ADD THE ELEMENT TO THE RECORD                
*                                                                               
         GOTO1 ADDREC              FINALLY ADD THE NEW RECORD OUT               
         B     VR70                                                             
*                                                                               
VR65     GOTO1 GETREC                                                           
*                                                                               
         LA    R4,ELEM             R4 = A(FORMULA ELEMENT)                      
         USING PBFRCELD,R4                                                      
*                                                                               
         GOTO1 FINDMNTH,DMCB,PBFRCDTE                                           
         BE    RECXISTS                                                         
*                                                                               
         GOTO1 ADDELEM             NO, ADD IT TO THE RECORD                     
*                                                                               
         GOTO1 PUTREC              FINALLY WRITE RECORD OUT                     
*                                                                               
VR70     XC    BILPRD,BILPRD       SO 1ST LINE DATA WILL GO TO FILTERS          
         MVC   BILPRD(L'QPRD),QPRD                                              
         OI    BILPRDH+6,X'80'                                                  
         OI    FLTRFLG1,FLTR1PRD                                                
         MVC   FILTRPRD,QPRD                                                    
*                                                                               
         XC    BILEST,BILEST                                                    
         MVC   BILEST(L'QEST),QEST                                              
         OI    BILESTH+6,X'80'                                                  
         OI    FLTRFLG1,FLTR1EST                                                
         MVC   FILTREST,BEST                                                    
         XC    BILESTD,BILESTD                                                  
         OC    BEST,BEST           FOR EST=ALL, SKIP DATES                      
         BZ    VR72                                                             
         CLC   QPRD,=C'AAA'        ALSO FOR PRODUCT ALL                         
         BE    VR72                                                             
         OC    QPRD,QPRD           OR PRODUCT NOT REQUESTED                     
         BZ    VR72                                                             
         GOTO1 DATCON,DMCB,(X'10',SVESTSDT),(8,BILESTD)                         
         OI    BILESTDH+6,X'80'                                                 
*                                                                               
VR72     DS    0H                                                               
         B     LREC                                                             
         DROP  R4                                                               
         EJECT                                                                  
*****                                                                           
* VALIDATE THE LINES BELOW FIRST LIST LINE                                      
*****                                                                           
VR100    LA    R3,BILPRD2H                                                      
         USING LINDSECT,R3                                                      
         LA    R4,TBLNTRYS                                                      
         USING TABLDSCT,R4                                                      
*                                                                               
VR100LP  LA    R1,BILPRDLH                                                      
         CR    R3,R1                                                            
         BH    VRECX                                                            
*                                                                               
         OC    TABLPRD,TABLPRD     ANYTHING LEFT TO VALIDATE?                   
         BE    VRECX               NO                                           
*                                                                               
         TM    LINMOSH+4,X'20'     DID THIS LINE CHANGE?                        
         BZ    VR102                                                            
         TM    LINFMLH+4,X'20'                                                  
         BZ    VR102                                                            
         TM    LINACPH+4,X'20'                                                  
         BZ    VR102                                                            
         TM    LINACOH+4,X'20'                                                  
         BNZ   VR100NX                                                          
*                                                                               
VR102    LA    R2,LINPRDH                                                       
         CLC   =C'ZZZ',LINPRD                                                   
         BE    INVLFLD                                                          
         CLC   =C'ALL',LINPRD      PRD ALL => AAA                               
         BNE   *+10                                                             
         MVC   LINPRD(3),=C'AAA'                                                
         CLC   LINPRD(3),=C'AAA'   FOR AAA SKIP VALIPRD                         
         BE    VR105                                                            
         GOTO1 VALIPRD                                                          
*                                                                               
VR105    DS    0H                                                               
         LA    R2,LINESTH                                                       
         XC    BEST,BEST                                                        
         CLC   =C'ALL',LINEST                                                   
         BE    VR106                                                            
         CLC   LINPRD(3),=C'AAA'   FOR PRD AAA WONT DO VALIEST                  
         BNE   VR105D              SO NEED TO SET ESTIMATE                      
*                                                                               
         ICM   RE,1,5(R2)          IF NO INPUT                                  
         BZ    VR105D              LET VALIEST HAVE IT                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STH   RE,BEST                                                          
         B     VR106                                                            
*                                                                               
VR105D   DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         MVC   SVESTSDT,PESTST                                                  
         MVC   SVESTNDT,PESTEND                                                 
         DROP  R6                                                               
*                                                                               
VR106    DS    0H                                                               
*                                                                               
VR107    DS    0H                                                               
         LA    R2,LINMOSH                                                       
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   8(R2),C'*'                                                       
         CLI   8(R2),C'*'          NO DATE                                      
         BNE   VR107D                                                           
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         XC    PVALBSTA,PVALBSTA                                                
         B     VR107G                                                           
*                                                                               
VR107D   DS    0H                                                               
         CLI   PROFB1X+12,C'Y'     DOES B1X ALLOW FOR MOS?                      
         BNE   INVLFLD                                                          
*                                                                               
         GOTO1 PERVAL,DMCB,(LINMOSH+5,LINMOS),PERVALST,0                        
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLFLD                                                          
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
* WAS THE START DAY OR ANY PART OF THE END DAY ENTERED?                         
         TM    PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                         
         BNO   INVLFLD                     YES, IT SHOULDN'T BE                 
*                                                                               
VR107G   DS    0H                                                               
         OC    BEST,BEST           IF EST=ALL                                   
         BZ    VR108               SKIP EST PERIOD CHECK                        
         CLC   LINPRD(3),=C'AAA'   OR IF PRD=ALL                                
         BE    VR108                                                            
         OC    PVALBSTA,PVALBSTA   OR IF NO DATE                                
         BE    VR108                                                            
**NO-OP  CLC   PVALESTA(4),SVESTSDT   ELSE, PERIOD SHOULD BE IN EST             
**NO-OP  BL    OUTESTR                                                          
**NO-OP  CLC   PVALEEND(4),SVESTNDT                                             
**NO-OP  BH    OUTESTR                                                          
         DROP  R1                                                               
*                                                                               
VR108    DS    0H                                                               
         LA    R2,LINFMLH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         NI    BITFLAG1,X'FF'-B1DELFML                                          
*                                                                               
         CLC   =C'DELETE',8(R2)    DELETE THE FORMULA ELEMENT?                  
         BNE   *+12                                                             
         OI    BITFLAG1,B1DELFML   YES                                          
         B     VR110                                                            
*                                                                               
         GOTO1 VALIFML,DMCB,(R2)                                                
*                                                                               
*        IF BASE B WAS AC ALLOW DATA IN THE 'SHOW AC AS %..' FIELDS             
*                                                                               
VR109    XC    BACPCT,BACPCT        MUST CLEAR THESE HERE                       
         XC    BACOF,BACOF                                                      
*                                                                               
         CLI   BFORMULA+1,X'08'     SEE IF BASE B IS AC                         
         BNE   VR109E     DISALLOW ENTRIES IN THOSE FIELDS                      
*                                                                               
         LA    R2,LINACPH                                                       
         CLI   5(R2),0         ANY DATA IN PCT. FIELD?                          
         BNE   VR109P          YES                                              
         LA    R2,LINACOH      IF NOT THEN NONE CAN BE IN 'OF' FIELD            
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
         B     VR109X          NO DATA IN EITHER                                
*                                                                               
VR109P   DS    0H              EDIT AC PCT.                                     
*                                                                               
         GOTO1 VALIACP,DMCB,(R2)                                                
*                                                                               
         LA    R2,LINACOH                                                       
         CLI   5(R2),0        ANY INPUT IN 'OF' FIELD?                          
         BE    MISSFLD            NEEDED IF % WAS ENTERED                       
*                                                                               
*                             EDIT AC PCT. OF FIELD                             
         GOTO1 VALIACO,DMCB,(R2)                                                
         B     VR109X                                                           
*                                                                               
VR109E   DS    0H          NO INPUT ALLOWED UNLESS BASE B IS AC                 
         LA    R2,LINACPH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,LINACOH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
VR109X   DS    0H                                                               
*                                                                               
VR110    LA    R6,ELEM             BUILD THE FORMULA ELEMENT                    
         XC    ELEM(30),ELEM       BE SURE IT'S CLEARED                         
         USING PBFRCELD,R6                                                      
         MVI   PBFRCEL,PBFRCELQ                                                 
         MVI   PBFRCLEN,PBFRCLNQ                                                
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   PBFRCDTE,PVALBSTA                                                
         XC    PBFRCDTE,=2X'FF'                                                 
         DROP  R1                                                               
         MVC   PBFRCFML,BFORMULA                                                
         MVC   PBFRCACP,BACPCT                                                  
         MVC   PBFRCACO,BACOF                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             RECORD HAD BETTER EXIST                      
         LA    R6,KEY                                                           
         USING PBFKEY,R6                                                        
         MVI   PBFKTYPE,PBFKTYPQ                                                
         MVC   PBFKAGY,AGENCY                                                   
         MVC   PBFKMED,QMED                                                     
         MVC   PBFKCLT,QCLT                                                     
         MVC   PBFKPRD,TABLPRD                                                  
         MVC   PBFKEST,TABLEST                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PBFKEY),KEYSAVE   RECORD EXISTED BEFORE?                   
         BE    *+6                     YES, ADD THE ELEMENT TO RECORD           
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,ELEM             R5 = A(FORMULA ELEMENT)                      
         USING PBFRCELD,R5                                                      
*                                                                               
         GOTO1 FINDMNTH,DMCB,PBFRCDTE   DOES FORMULA FOR MONTH EXIST?           
         BE    VR150                    YES                                     
*                                                                               
         TM    BITFLAG1,B1DELFML        NO, IF TRYING TO DELETE                 
         BNZ   RECNTFND                 THEN RECORD NOT FOUND                   
*                                                                               
*                                  DELETE OLD ELEMENT FIRST                     
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('PBFRCELQ',AIO),(L'TABLMOS,   X        
               TABLMOS),0                                                       
         CLI   DMCB+12,0           ELEMENT EXISTS                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADDELEM             NO, ADD IT TO THE RECORD                     
*                                                                               
         GOTO1 PUTREC              FINALLY WRITE RECORD OUT                     
         B     VR100NX                                                          
         DROP  R5                                                               
*                                                                               
VR150    DS    0H                                                               
         L     R6,DMCB             R6 = A(ELEMENT FOUND)                        
         USING PBFRCELD,R6                                                      
*                                                                               
         CLC   PBFRCDTE,TABLMOS    FORMULA FOR THE MONTH EXISTS                 
         BE    VR160               OKAY IF MONTH IS SAME AS LISTED ONE          
*                                                                               
         TM    BITFLAG1,B1DELFML   TRYING TO DELETE?                            
         BZ    RECXISTS            NO, RECORD EXISTS ALREADY                    
         LA    R2,LINFMLH          ERROR IF TRYING TO DELETE                    
         B     INVLFLD                                                          
*                                                                               
VR160    TM    BITFLAG1,B1DELFML                                                
         BZ    VR170                                                            
*                                  DELETE OLD ELEMENT FIRST                     
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('PBFRCELQ',AIO),(L'TABLMOS,   X        
               TABLMOS),0                                                       
         CLI   DMCB+12,0           ELEMENT EXISTS                               
         BE    VR180                                                            
         DC    H'0'                                                             
*                                                                               
VR170    MVC   PBFRCDTE,ELEM+PBFRCDTE-PBFRCELD                                  
         MVC   PBFRCFML,ELEM+PBFRCFML-PBFRCELD                                  
         MVC   PBFRCACP,ELEM+PBFRCACP-PBFRCELD                                  
         MVC   PBFRCACO,ELEM+PBFRCACO-PBFRCELD                                  
         DROP  R6                                                               
*                                                                               
VR180    GOTO1 PUTREC              FINALLY WRITE RECORD OUT                     
*                                                                               
VR100NX  OI    LINMOSH+4,X'20'     VALIDATE LINE SO WE DON'T                    
         OI    LINFMLH+4,X'20'         DO IT AGAIN                              
         OI    LINACPH+4,X'20'                                                  
         OI    LINACOH+4,X'20'                                                  
*                                                                               
         LA    R3,LINNEXTL                                                      
         LA    R4,TABLNEXT                                                      
         B     VR100LP                                                          
         DROP  R3,R4                                                            
*                                                                               
VRECX    B     LREC                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORDS                                                              
***********************************************************************         
LREC     DS    0H                                                               
         OI    BILPRD1H+6,X'40'    CURSOR GOES HERE NOW                         
*                                                                               
         LA    R2,BILPRDLH         CLEAR LIST PORTION OF SCREEN                 
         USING LINDSECT,R2                                                      
         SR    RE,RE               *** TWAXC CODE FOR BILPRD1H,LINFMLH          
         LA    R1,BILPRD1H               THAT ALSO VALIDATES THE FIELDS         
         LA    RF,LINACOH                                                       
LRTWAXC1 IC    RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         LTR   RE,RE                                                            
         BM    LRTWAXCX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         OI    4(R1),X'20'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,LRTWAXC1                                                   
LRTWAXCX DS    0H                                                               
*                                                                               
         XC    TBLNTRYS,TBLNTRYS   CLEAR ENTRY TABLE                            
*                                                                               
         LA    R2,BILPRD2H         R2 = A(1ST DISPLAY LINE)                     
         LA    R3,TBLNTRYS         R3 = A(TABLE ENTRIES)                        
         USING TABLDSCT,R3                                                      
*                                                                               
         TM    BITFLAG1,B1KEYCHG   KEY CHANGED?                                 
         BNZ   LR10                YES                                          
         OC    LASTNTRY,LASTNTRY   NO, RESTART LIST FROM BEGINNING?             
         BNZ   LR50                    NO                                       
*                                                                               
LR10     XC    LASTNTRY,LASTNTRY                                                
*                                                                               
         LA    R6,KEY              DISPLAY BASED ON KEY FIELDS                  
         USING PBFKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   PBFKTYPE,PBFKTYPQ                                                
         MVC   PBFKAGY,AGENCY                                                   
         MVC   PBFKMED,QMED                                                     
         MVC   PBFKCLT,QCLT                                                     
*                                                                               
         TM    FLTRFLG1,FLTR1PRD                                                
         BZ    LR20                                                             
         MVC   PBFKPRD,QPRD                                                     
*                                                                               
         TM    FLTRFLG1,FLTR1EST                                                
         BZ    LR20                                                             
         MVC   PBFKEST,BEST                                                     
*                                                                               
*                                                                               
LR20     B     LR100HI                                                          
         DROP  R6                                                               
*                                                                               
LR50     LA    R6,KEY              DISPLAY BASED ON KEY FIELDS                  
         USING PBFKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   PBFKTYPE,PBFKTYPQ                                                
         MVC   PBFKAGY,AGENCY                                                   
         MVC   PBFKMED,QMED                                                     
         MVC   PBFKCLT,QCLT                                                     
         MVC   PBFKPRD(L'PBFKPRD+L'PBFKEST),LASTNTRY                            
         DROP  R6                                                               
*                                                                               
LR100HI  GOTO1 HIGH                                                             
         B     LR110                                                            
LR100SQ  GOTO1 SEQ                                                              
*                                                                               
LR110    LA    R6,KEY                                                           
         USING PBFKEY,R6                                                        
*                                                                               
         CLC   PBFKEY(PBFKPRD-PBFKEY),KEYSAVE BILL FORMULA RECORD FOR           
         BE    *+14                             SPECIFIED MEDIA/CLIENT?         
         XC    LASTNTRY,LASTNTRY   NO, RESTART LIST FROM THE BEGINNING          
         B     LRECX                   NEXT TIME IN                             
*                                                                               
         OC    LASTNTRY,LASTNTRY                                                
         BZ    LR120                                                            
         CLC   PBFKEY,KEYSAVE                                                   
         BE    LR150                                                            
         DC    H'0'                IT BETTER BE THE SAME                        
*                                                                               
LR120    TM    FLTRFLG1,FLTR1PRD   MAKE SURE FILTERS ARE SATISFIED IF           
         BZ    LR122                   FROM BEGINNING OR KEY CHANGED            
         CLC   FILTRPRD,PBFKPRD                                                 
         BNE   LR100SQ                                                          
*                                                                               
LR122    DS    0H                                                               
         TM    FLTRFLG1,FLTR1EST                                                
         BZ    LR124                                                            
         CLC   FILTREST,PBFKEST                                                 
         BNE   LR100SQ                                                          
*                                                                               
LR124    DS    0H                                                               
         DROP  R6                                                               
*                                                                               
LR150    GOTO1 GETREC              FETCH THE BILL FORMULA RECORD                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PBFRCELQ     THE ONLY ELEMENT IN RECORD                   
*                                                                               
         OC    LASTNTRY,LASTNTRY   FROM A PREVIOUS ONE?                         
         BZ    LR160               NO                                           
*                                                                               
         GOTO1 FINDMNTH,DMCB,LASTNTRY+TABLMOS-TABLDSCT                          
         BE    *+6                                                              
         DC    H'0'                DIE, IT SUPPOSED TO BE HERE                  
*                                                                               
         L     R6,DMCB             R6 = A(ELEMENT)                              
         B     LR200                                                            
*                                                                               
         USING PBFRCELD,R6                                                      
LR160    BAS   RE,GETEL                                                         
         BNE   LR100SQ             GET NEXT RECORD IF NOT FORMULA               
*                                                                               
LR200    LA    R1,BILPRDLH         DO WE HAVE ANYMORE ROOM ON SCREEN?           
         CR    R2,R1                                                            
         BNH   LR210               YES                                          
*                                                                               
         L     R1,AIO                                                           
         USING PBFREC,R1                                                        
         MVC   LASTNTRY(TABLMOS-TABLPRD),PBFKPRD                                
         DROP  R1                                                               
         MVC   LASTNTRY+TABLMOS-TABLDSCT(L'TABLMOS),PBFRCDTE                    
         B     LRECX                                                            
*                                                                               
LR210    L     R4,AIO                                                           
         USING PBFREC,R4                                                        
         MVC   LINPRD,PBFKPRD                                                   
         CLC   LINPRD(3),=C'AAA'   AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   LINPRD(3),=C'ALL'                                                
         MVI   LINPRDH+5,3                                                      
         CLI   LINPRD+2,C' '                                                    
         BNE   *+8                                                              
         MVI   LINPRDH+5,2                                                      
*                                                                               
         OC    PBFKEST,PBFKEST                                                  
         BNZ   LR214                                                            
         MVC   LINEST(3),=C'ALL'                                                
         MVI   LINESTH+5,3                                                      
         B     LR216                                                            
*                                                                               
LR214    DS    0H                                                               
         EDIT  (B2,PBFKEST),(3,LINEST),FILL=0                                   
         MVI   LINESTH+5,3                                                      
         OI    LINESTH+4,X'08'                                                  
*                                                                               
LR216    DS    0H                                                               
*                                                                               
LR220    DS    0H                                                               
         MVC   TABLPRD(TABLMOS-TABLPRD),PBFKPRD                                 
         DROP  R4                                                               
*                                                                               
         MVC   TABLMOS,PBFRCDTE                                                 
         MVC   LINMOS,SPACES                                                    
         MVI   LINMOS,C'*'         MARKER FOR NO DATE                           
         MVI   LINMOSH+5,1                                                      
         CLC   PBFRCDTE,=2X'FF'                                                 
         BE    LR222                                                            
*                                                                               
         MVC   FULL,PBFRCDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LINMOS)                                  
         MVI   LINMOSH+5,6                                                      
*                                                                               
LR222    DS    0H                                                               
         MVC   BFORMULA,PBFRCFML                                                
         GOTO1 DISPFML,DMCB,LINFMLH                                             
*                                                                               
         CLI   PBFRCACO,X'01'    GROSS?                                         
         BE    LR222C                                                           
         CLI   PBFRCACO,X'02'    NET?                                           
         BE    LR222C                                                           
         CLI   PBFRCACO,X'05'    GROSS-CD                                       
         BE    LR222C                                                           
         CLI   PBFRCACO,X'06'    NET-CD                                         
         BE    LR222C                                                           
         XC    PBFRCACP,PBFRCACP    THEY WERE BAD-SO CLEAR                      
         XC    PBFRCACO,PBFRCACO                                                
*                                                                               
LR222C   MVC   BACPCT,PBFRCACP                                                  
         GOTO1 DISPACP,DMCB,LINACPH                                             
*                                                                               
         MVC   BACOF,PBFRCACO                                                   
         GOTO1 DISPACO,DMCB,LINACOH                                             
*                                                                               
         XC    LASTNTRY,LASTNTRY                                                
*                                                                               
         LA    R2,LINNEXTL         R2 = A(NEXT LINE ON SCREEN)                  
         LA    R3,TABLNEXT         R3 = A(NEXT ENTRY IN TABLE)                  
*                                                                               
         BAS   RE,NEXTEL           GOT ANOTHER FORMULA?                         
         BE    LR200               YES, TRY TO DISPLAY IT                       
         B     LR100SQ             NO, GET NEXT RECORD                          
*                                                                               
LRECX    B     XIT                                                              
         EJECT                                                                  
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
* THIS ROUTINE VALIDATES A FORMULA                                              
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA'S FIELD HEADER)                    
*                                                                               
* ON EXIT:     BFORMULA            BINARY REPRESENTATION OF THE FORMULA         
***********************************************************************         
VALIFML  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'83',BLOCK)                                  
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
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
         B     ERREXIT                                                          
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
         B     ERREXIT                                                          
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
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS A FORMULA                                               
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD HEADER)                      
*              BFORMULA            FORMULA TO BE DISPLAYED                      
***********************************************************************         
DISPFML  NTR1                                                                   
         L     R2,0(R1)            R2 = A(FORMULA FIELD HEADER)                 
         XC    8(L'BILFML1,R2),8(R2)                                            
         LA    R3,8(R2)            R3 = A(1ST BYTE IN FORMULA TEXT)             
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
DFMLX    OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS AC PCT                                                  
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD HEADER)                      
*              BACPCT              AC PCT TO BE DISPLAYED                       
***********************************************************************         
DISPACP  NTR1                                                                   
         L     R2,0(R1)            R2 = A(FORMULA FIELD HEADER)                 
         XC    8(L'BILACP1,R2),8(R2)                                            
         LA    R3,8(R2)            R3 = A(1ST BYTE IN AC PCT)                   
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
DACPX    OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE DISPLAYS AC PCT OF                                               
*                                                                               
* ON ENTRY:    PARAMETER 1         A(FORMULA FIELD HEADER)                      
*              BACOF               AC PCT OF TO BE DISPLAYED                    
***********************************************************************         
DISPACO  NTR1                                                                   
         L     R2,0(R1)            R2 = A(FORMULA FIELD HEADER)                 
         XC    8(L'BILACO1,R2),8(R2)                                            
         LA    R3,8(R2)            R3 = A(1ST BYTE IN AC PCT)                   
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
DACOX    OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
RECXISTS MVI   ERROR,RECEXIST                                                   
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
*                                                                               
OUTESTR  MVI   ERROR,ESTRNGE  MONTH OUT OF ESTIMATE RANGE                       
         B     ERREXIT                                                          
*                                                                               
B1XERR   MVI   ERROR,B1XINCMP B1X PROFILE INCOMPATIBLITY                        
         B     ERREXIT                                                          
*&&DO                                                                           
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
*&&                                                                             
ERREXIT  GOTO1 ERREX                                                            
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
ESTRNGE  EQU   97                                                               
B1XINCMP EQU   86                                                               
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD7D                                                       
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
       ++INCLUDE PPBILLFML                                                      
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
ATIOB    DS    A                                                                
*                                                                               
BITFLAG1 DS    X                                                                
B1KEYCHG EQU   X'80'               A KEY FIELD HAS BEEN CHANGED                 
B1DELFML EQU   X'40'               DELETE THE FORMULA ELEMENT                   
*                                                                               
FLTRFLG1 DS    X                                                                
FLTR1PRD EQU   X'80'               START DISPLAY AT THIS PRODUCT                
FLTR1EST EQU   X'40'                                     ESTIMATE               
*                                                                               
BFORMULA DS    0XL(L'PBFRCFML)     BINARY REPRESENTATION OF FORMULA             
BBILLBAS DS    XL(L'BILBASA+L'BILBASB)                                          
*                                                                               
BBILLCOM DS    XL(L'BILADJ)      SIGNED COMMISSION RATE (99.9999)               
*                                                                               
BACPCT   DS    XL(L'PBFRCACP)       'SHOW AC AS PCT'                            
BACOF    DS    XL(L'PBFRCACO)       'SHOW AC AS PCT. OF'                        
*                                                                               
FILTRPRD DS    CL(L'PBFKPRD)       PRODUCT FILTER VALUE                         
FILTREST DS    XL(L'PBFKEST)       ESTIMATE FILTER VALUE                        
*                                                                               
SVESTPER DS    0CL12               ESTIMATE PERIOD                              
SVESTSDT DS    CL6                 ESTIMATE START DATE (YYMMDD)                 
SVESTNDT DS    CL6                 ESTIMATE END DATE   (YYMMDD)                 
PERVALST DS    XL56                PERVAL STORAGE AREA                          
PROFB1X  DS    XL16                B1X PROFILE                                  
*                                                                               
LASTNTRY DS    XL(TABLNEXT-TABLDSCT)   LAST ENTRY USED                          
TBLNTRYS DS    0XL((NODSPLNS+1)*(TABLNEXT-TABLDSCT))                            
         DS    (NODSPLNS+1)XL(TABLNEXT-TABLDSCT)                                
         EJECT                                                                  
***********************************************************************         
* DSECT THAT COVERS THE SAVED TABLE ENTRIES                                     
***********************************************************************         
TABLDSCT DSECT                                                                  
TABLPRD  DS    XL(L'PBFKPRD)       PRODUCT                                      
TABLEST  DS    XL(L'PBFKEST)       ESTIMATE                                     
TABLMOS  DS    XL(L'PBFRCDTE)      MONTH OF SERVICE                             
TABLNEXT DS    0X                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT THAT COVERS THE DISPLAY LINE                                            
***********************************************************************         
LINDSECT DSECT                                                                  
LINPRDH  DS    CL(L'BILPRD1H)                                                   
LINPRD   DS    CL(L'BILPRD1)                                                    
LINESTH  DS    CL(L'BILEST1H)                                                   
LINEST   DS    CL(L'BILEST1)                                                    
LINMOSH  DS    CL(L'BILMOS1H)                                                   
LINMOS   DS    CL(L'BILMOS1)                                                    
LINFMLH  DS    CL(L'BILFML1H)                                                   
LINFML   DS    CL(L'BILFML1)                                                    
LINACPH  DS    CL(L'BILACP1H)                                                   
LINACP   DS    CL(L'BILACP1)                                                    
LINACOH  DS    CL(L'BILACO1H)                                                   
LINACO   DS    CL(L'BILACO1)                                                    
LINNEXTL DS    0C                                                               
*                                                                               
LNDSPLNS EQU   BILPRD2H-BILPRD1H                                                
NODSPLNS EQU   ((BILPRDLH-BILPRD1H)/LNDSPLNS)   NOT INCL 1ST INPUT LINE         
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PRSFM17   06/22/05'                                      
         END                                                                    
