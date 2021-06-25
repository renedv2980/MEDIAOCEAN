*          DATA SET SPSFM3B    AT LEVEL 014 AS OF 05/19/08                      
*PHASE T2173BA                                                                  
***********************************************************************         
*                                                                               
*  TITLE: SPSFM3B - T2173B BILL FORMULA RECORDS MAINTENANCE                     
*                                                                               
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                            
*               GEGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  INPUTS: SCREENS SPSFM9F (T2179F) -- MAINTENANCE SCREEN                       
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
*   NOTE- THERE IS SOME OBSCURE STUFF IN HERE RELATING TO MARKET                
*         GROUPS. THE KEY WAS EXPANDED TO ALLOW FOR MGR'S OR MKTS               
*         BUT THE CODE TO HANDLE THEM HAS NOT BEEN WRITTEN. IN                  
*         RECORD KEY AND IN VARIOUS TABLE THE MGR FIELD IS 3 LONG               
*         AND THE MARKET FIELD OVERLAYS THE LAST 2 BYTES AND IS                 
*         PRECEDED BY A NULL.                                                   
*                                                                               
*         PRD, EST, AND MGR/MKT CAN BE 'ALL'. THIS TRANSLATES TO                
*         AAA FOR PRODUCT, NULL FOR EST, AND NULLS FOR MGR/MKT.                 
*                                                                               
***********************************************************************         
T2173B   TITLE 'SPSFM3B - BILL FORMULA RECORDS MAINTENANCE OVERLAY'             
T2173B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2173B*,R7                                                    
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
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         XC    WORK,WORK           GET B1X PROFILE                              
         XC    PROFB1X,PROFB1X                                                  
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE   OFFICE                                      
         DROP  R6                                                               
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
         MVI   BEST,0                                                           
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
         MVI   BEST,0                                                           
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
         STC   RE,BEST                                                          
         B     VKEST04                                                          
*                                                                               
VKEST03  DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO              SAVE THE ESTIMATE PERIOD                     
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,(X'10',ESTART),(8,BILESTD)                           
         DROP  R6                                                               
*                                                                               
VKEST04  DS    0H                                                               
         MVC   FILTREST,BEST                                                    
         OI    FLTRFLG1,FLTR1EST                                                
*                                                                               
VKESTX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
         OI    BILESTDH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE MARKET IF ANY                                                    
*****                                                                           
VKMKT00  LA    R2,BILMKTH                                                       
         NI    FLTRFLG1,X'FF'-FLTR1MKT                                          
*                                                                               
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY?                  
         BNZ   *+8                                                              
         OI    BITFLAG1,B1KEYCHG   YES                                          
*                                                                               
         XC    BILMKNM,BILMKNM                                                  
         OI    BILMKNMH+6,X'80'                                                 
*                                                                               
         CLI   BILMKTH+5,0                                                      
         BE    VKMKTX                                                           
*                                                                               
         XC    FILTRMM,FILTRMM     MGR/MKT FIELD                                
         XC    BMKT,BMKT                                                        
         MVC   QMKT,=C'ALL '                                                    
         CLC   =C'ALL',BILMKT                                                   
         BE    VKMKT04                                                          
*                                                                               
         GOTO1 VALIMKT                                                          
VKMKT04  DS    0H                                                               
         MVC   FILTRMM+1(2),BMKT        (ONLY MARKET USED NOW)                  
         OI    FLTRFLG1,FLTR1MKT                                                
         MVC   BILMKNM,MKTNM                                                    
*                                                                               
VKMKTX   OI    4(R2),X'20'         VALIDATED THIS FIELD                         
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
         TM    LINMKTH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINMOSH+4,X'20'                                                  
         BZ    VR10                                                             
         TM    LINFMLH+4,X'20'                                                  
         BNZ   VR100               TOP LINE HAS NOT BEEN CHANGED                
*                                                                               
VR10     CLI   LINPRDH+5,0         ANY DATA ON THE TOP LINE?                    
         BNE   VR20                                                             
         CLI   LINESTH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINMKTH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINMOSH+5,0                                                      
         BNE   VR20                                                             
         CLI   LINFMLH+5,0                                                      
         BE    VR100               NONE, CHECK OTHER LINES                      
*                                                                               
VR20     LA    R2,LINPRDH          ALL FIELDS ON LINE ARE REQUIRED              
         CLI   5(R2),0                                                          
         BNE   VR25                                                             
         CLI   BILPRDH+5,0                                                      
         BE    MISSFLD                                                          
*                                                                               
         MVC   4(4,R2),BILPRDH+4                                                
         MVC   8(L'BILPRD,R2),BILPRD                                            
*                                                                               
VR25     DS    0H                                                               
         CLC   =C'POL',LINPRD      POL NOT ALLOWED                              
         BE    INVLFLD                                                          
         CLC   =C'ALL',LINPRD      PRD ALL => AAA                               
         BNE   *+16                                                             
         MVC   LINPRD(3),=C'AAA'                                                
         MVC   QPRD,=C'AAA'                                                     
         CLC   LINPRD(3),=C'AAA'   FOR AAA SKIP VALIPRD                         
         BE    VR30                                                             
         GOTO1 VALIPRD                                                          
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
         MVI   BEST,0                                                           
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
         STC   RE,BEST                                                          
         B     VR40                                                             
*                                                                               
VR38     DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO              SAVE THE ESTIMATE PERIOD                     
         USING ESTHDRD,R6                                                       
         MVC   SVESTSDT,ESTART                                                  
         MVC   SVESTNDT,EEND                                                    
         DROP  R6                                                               
*                                                                               
VR40     LA    R2,LINMKTH                                                       
         CLI   5(R2),0                                                          
         BNE   VR45                                                             
         CLI   BILMKTH+5,0                                                      
         BE    MISSFLD                                                          
*                                                                               
         MVC   4(4,R2),BILMKTH+4                                                
         MVC   8(L'BILMKT,R2),BILMKT                                            
*                                                                               
VR45     DS    0H                                                               
         MVC   QMKT,=C'ALL '                                                    
         XC    BMKT,BMKT                                                        
         CLC   =C'ALL',LINMKT                                                   
         BE    VR46                                                             
         GOTO1 VALIMKT                                                          
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
         BE    VR46E                                                            
         CLI   PROFB1X+12,C'S'                                                  
         BNE   INVLFLD                                                          
*                                                                               
VR46E    DS    0H                                                               
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
         CLI   BEST,0              IF EST=ALL                                   
         BE    VR48                                                             
         CLC   LINPRD(3),=C'AAA'   OR PRD=ALL                                   
         BE    VR48                                                             
         OC    PVALBSTA,PVALBSTA   OR NO DATE                                   
         BZ    VR48                                                             
         CLC   PVALESTA(4),SVESTSDT   PERIOD SHOULD BE IN ESTIMATE'S            
         BL    OUTESTR                                                          
         CLC   PVALEEND(4),SVESTNDT                                             
         BH    OUTESTR                                                          
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
         DROP  R3                                                               
*                                                                               
         LA    R4,ELEM             BUILD THE FORMULA ELEMENT                    
         USING BFRCDELD,R4                                                      
         MVI   BFRCDEL,BFRCDELQ                                                 
         MVI   BFRCDLEN,BFRCDLNQ                                                
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   BFRCDDTE,PVALBSTA                                                
         XC    BFRCDDTE,=2X'FF'                                                 
         DROP  R1                                                               
         MVC   BFRCDFML,BFORMULA                                                
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY             SEE IF THE RECORD EXISTED BEFORE             
         LA    R6,KEY                                                           
         USING BFKEY,R6                                                         
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,BAGYMD                                                  
         MVC   BFKCLT,BCLT                                                      
         MVC   BFKPRD,QPRD                                                      
         MVC   BFKEST,BEST                                                      
         MVC   BFKMKT,BMKT                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BFKEY),KEYSAVE    RECORD EXISTED BEFORE?                   
         BE    VR50                    YES, ADD THE ELEMENT TO RECORD           
*                                                                               
         L     R6,AIO                  NO, CREATE THE RECORD                    
         XC    0(50,R6),0(R6)                                                   
         USING BFREC,R6                                                         
         MVC   0(L'BFKEY,R6),KEYSAVE                                            
*                                                                               
         GOTO1 ADDELEM             ADD THE ELEMENT TO THE RECORD                
*                                                                               
         GOTO1 ADDREC              FINALLY ADD THE NEW RECORD OUT               
         B     VR70                                                             
*                                                                               
VR50     GOTO1 GETREC                                                           
*                                                                               
         LA    R4,ELEM             R4 = A(FORMULA ELEMENT)                      
         USING BFRCDELD,R4                                                      
*                                                                               
         GOTO1 FINDMNTH,DMCB,BFRCDDTE                                           
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
         CLI   BEST,0              FOR EST=ALL, SKIP DATES                      
         BE    VR72                                                             
         CLC   QPRD,=C'AAA'        ALSO FOR PRODUCT ALL                         
         BE    VR72                                                             
         OC    SVESTSDT,SVESTSDT   EST DATE SET?                                
         BZ    VR72                NO, NOT ALWAYS SET!                          
         GOTO1 DATCON,DMCB,(X'10',SVESTSDT),(8,BILESTD)                         
         OI    BILESTDH+6,X'80'                                                 
*                                                                               
VR72     DS    0H                                                               
         XC    BILMKT,BILMKT                                                    
         MVC   BILMKT(L'QMKT),QMKT                                              
         OI    BILMKTH+6,X'80'                                                  
         XC    BILMKNM,BILMKNM                                                  
         MVC   BILMKNM,MKTNM                                                    
         OI    BILMKNMH+6,X'80'                                                 
         OI    FLTRFLG1,FLTR1MKT                                                
         XC    FILTRMM,FILTRMM     MGR/MKT FIELD                                
         MVC   FILTRMM+1(2),BMKT        (ONLY MARKET USED NOW)                  
*                                                                               
         CLI   BMKT,X'FF'          GOT A MARKET GROUP INSTEAD?                  
         BNE   *+8                                                              
         MVI   FILTRMM,X'FF'       YES                                          
*                                                                               
         OI    BITFLAG1,B1KEYCHG   MAKE APPL THINK KEY WAS CHANGED              
*                                                                               
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
         BZ    *+12                                                             
         TM    LINFMLH+4,X'20'                                                  
         BNZ   VR100NX                                                          
*                                                                               
         LA    R2,LINPRDH                                                       
         CLC   =C'POL',LINPRD                                                   
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
         MVI   BEST,0                                                           
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
         STC   RE,BEST                                                          
         B     VR106                                                            
*                                                                               
VR105D   DS    0H                                                               
         GOTO1 VALIEST                                                          
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         MVC   SVESTSDT,ESTART                                                  
         MVC   SVESTNDT,EEND                                                    
         DROP  R6                                                               
*                                                                               
VR106    DS    0H                                                               
         XC    BMKT,BMKT                                                        
         CLC   =C'ALL',LINMKT                                                   
         BE    VR107                                                            
         LA    R2,LINMKTH                                                       
         GOTO1 VALIMKT                                                          
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
         BE    VR107E                                                           
         CLI   PROFB1X+12,C'S'                                                  
         BNE   INVLFLD                                                          
*                                                                               
VR107E   DS    0H                                                               
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
         CLI   BEST,0              IF EST=ALL                                   
         BE    VR108               SKIP EST PERIOD CHECK                        
         CLC   LINPRD(3),=C'AAA'   OR IF PRD=ALL                                
         BE    VR108                                                            
         OC    PVALBSTA,PVALBSTA   OR IF NO DATE                                
         BE    VR108                                                            
         CLC   PVALESTA(4),SVESTSDT   ELSE, PERIOD SHOULD BE IN EST             
         BL    OUTESTR                                                          
         CLC   PVALEEND(4),SVESTNDT                                             
         BH    OUTESTR                                                          
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
VR110    LA    R6,ELEM             BUILD THE FORMULA ELEMENT                    
         USING BFRCDELD,R6                                                      
         MVI   BFRCDEL,BFRCDELQ                                                 
         MVI   BFRCDLEN,BFRCDLNQ                                                
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   BFRCDDTE,PVALBSTA                                                
         XC    BFRCDDTE,=2X'FF'                                                 
         DROP  R1                                                               
         MVC   BFRCDFML,BFORMULA                                                
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             RECORD HAD BETTER EXIST                      
         LA    R6,KEY                                                           
         USING BFKEY,R6                                                         
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,BAGYMD                                                  
         MVC   BFKCLT,BCLT                                                      
         MVC   BFKPRD,TABLPRD                                                   
         MVC   BFKEST,TABLEST                                                   
         MVC   BFKMGR,TABLMGR      MGR/MKT                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BFKEY),KEYSAVE    RECORD EXISTED BEFORE?                   
         BE    *+6                     YES, ADD THE ELEMENT TO RECORD           
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,ELEM             R5 = A(FORMULA ELEMENT)                      
         USING BFRCDELD,R5                                                      
*                                                                               
         GOTO1 FINDMNTH,DMCB,BFRCDDTE   DOES FORMULA FOR MONTH EXIST?           
         BE    VR150                    YES                                     
*                                                                               
         TM    BITFLAG1,B1DELFML        NO, IF TRYING TO DELETE                 
         BNZ   RECNTFND                 THEN RECORD NOT FOUND                   
*                                                                               
*                                  DELETE OLD ELEMENT FIRST                     
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('BFRCDELQ',AIO),(L'TABLMOS,   X        
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
         USING BFRCDELD,R6                                                      
*                                                                               
         CLC   BFRCDDTE,TABLMOS    FORMULA FOR THE MONTH EXISTS                 
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
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('BFRCDELQ',AIO),(L'TABLMOS,   X        
               TABLMOS),0                                                       
         CLI   DMCB+12,0           ELEMENT EXISTS                               
         BE    VR180                                                            
         DC    H'0'                                                             
*                                                                               
VR170    MVC   BFRCDDTE,ELEM+BFRCDDTE-BFRCDELD                                  
         MVC   BFRCDFML,ELEM+BFRCDFML-BFRCDELD                                  
         DROP  R6                                                               
*                                                                               
VR180    GOTO1 PUTREC              FINALLY WRITE RECORD OUT                     
*                                                                               
VR100NX  OI    LINMOSH+4,X'20'     VALIDATE LINE SO WE DON'T                    
         OI    LINFMLH+4,X'20'         DO IT AGAIN                              
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
         LA    RF,LINFMLH                                                       
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
         USING BFKEY,R6                                                         
         XC    KEY,KEY                                                          
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,BAGYMD                                                  
         MVC   BFKCLT,BCLT                                                      
*                                                                               
         TM    FLTRFLG1,FLTR1PRD                                                
         BZ    LR20                                                             
         MVC   BFKPRD,QPRD                                                      
*                                                                               
         TM    FLTRFLG1,FLTR1EST                                                
         BZ    LR20                                                             
         MVC   BFKEST,BEST                                                      
*                                                                               
         TM    FLTRFLG1,FLTR1MKT                                                
         BZ    LR20                                                             
         MVC   BFKMKT,BMKT                                                      
*                                                                               
LR20     B     LR100HI                                                          
         DROP  R6                                                               
*                                                                               
LR50     LA    R6,KEY              DISPLAY BASED ON KEY FIELDS                  
         USING BFKEY,R6                                                         
         XC    KEY,KEY                                                          
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,BAGYMD                                                  
         MVC   BFKCLT,BCLT                                                      
         MVC   BFKPRD(L'BFKPRD+L'BFKEST+L'BFKMGR),LASTNTRY                      
         DROP  R6                                                               
*                                                                               
LR100HI  GOTO1 HIGH                                                             
         B     LR110                                                            
LR100SQ  GOTO1 SEQ                                                              
*                                                                               
LR110    LA    R6,KEY                                                           
         USING BFKEY,R6                                                         
*                                                                               
         CLC   BFKEY(BFKPRD-BFKEY),KEYSAVE    BILL FORMULA RECORD FOR           
         BE    *+14                             SPECIFIED MEDIA/CLIENT?         
         XC    LASTNTRY,LASTNTRY   NO, RESTART LIST FROM THE BEGINNING          
         B     LRECX                   NEXT TIME IN                             
*                                                                               
         OC    LASTNTRY,LASTNTRY                                                
         BZ    LR120                                                            
         CLC   BFKEY,KEYSAVE                                                    
         BE    LR150                                                            
         DC    H'0'                IT BETTER BE THE SAME                        
*                                                                               
LR120    TM    FLTRFLG1,FLTR1PRD   MAKE SURE FILTERS ARE SATISFIED IF           
         BZ    LR122                   FROM BEGINNING OR KEY CHANGED            
         CLC   FILTRPRD,BFKPRD                                                  
         BNE   LR100SQ                                                          
*                                                                               
LR122    DS    0H                                                               
         TM    FLTRFLG1,FLTR1EST                                                
         BZ    LR124                                                            
         CLC   FILTREST,BFKEST                                                  
         BNE   LR100SQ                                                          
*                                                                               
LR124    DS    0H                                                               
         TM    FLTRFLG1,FLTR1MKT                                                
         BZ    LR150                                                            
         CLC   FILTRMM,BFKMGR                                                   
         BNE   LR100SQ                                                          
         DROP  R6                                                               
*                                                                               
LR150    GOTO1 GETREC              FETCH THE BILL FORMULA RECORD                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BFRCDELQ     THE ONLY ELEMENT IN RECORD                   
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
         USING BFRCDELD,R6                                                      
LR160    BAS   RE,GETEL                                                         
         BNE   LR100SQ             GET NEXT RECORD IF NOT FORMULA               
*                                                                               
LR200    LA    R1,BILPRDLH         DO WE HAVE ANYMORE ROOM ON SCREEN?           
         CR    R2,R1                                                            
         BNH   LR210               YES                                          
*                                                                               
         L     R1,AIO                                                           
         USING BFREC,R1                                                         
         MVC   LASTNTRY(TABLMOS-TABLPRD),BFKPRD                                 
         DROP  R1                                                               
         MVC   LASTNTRY+TABLMOS-TABLDSCT(L'TABLMOS),BFRCDDTE                    
         B     LRECX                                                            
*                                                                               
LR210    L     R4,AIO                                                           
         USING BFREC,R4                                                         
         MVC   LINPRD,BFKPRD                                                    
         CLC   LINPRD(3),=C'AAA'   AAA -> ALL                                   
         BNE   *+10                                                             
         MVC   LINPRD(3),=C'ALL'                                                
         MVI   LINPRDH+5,3                                                      
         CLI   LINPRD+2,C' '                                                    
         BNE   *+8                                                              
         MVI   LINPRDH+5,2                                                      
*                                                                               
         CLI   BFKEST,0                                                         
         BNE   LR214                                                            
         MVC   LINEST(3),=C'ALL'                                                
         MVI   LINESTH+5,3                                                      
         B     LR216                                                            
*                                                                               
LR214    DS    0H                                                               
         EDIT  (B1,BFKEST),(3,LINEST),FILL=0                                    
         MVI   LINESTH+5,3                                                      
         OI    LINESTH+4,X'08'                                                  
*                                                                               
LR216    DS    0H                                                               
         OC    BFKMGR,BFKMGR                                                    
         BNZ   LR218                                                            
         MVC   LINMKT(3),=C'ALL'                                                
         MVI   LINMKTH+5,3                                                      
         B     LR220                                                            
*                                                                               
LR218    DS    0H                                                               
         EDIT  (B2,BFKMKT),(4,LINMKT),FILL=0                                    
         MVI   LINMKTH+5,4                                                      
         OI    LINMKTH+4,X'08'                                                  
*                                                                               
LR220    DS    0H                                                               
         MVC   TABLPRD(TABLMOS-TABLPRD),BFKPRD                                  
         DROP  R4                                                               
*                                                                               
         MVC   TABLMOS,BFRCDDTE                                                 
         MVC   LINMOS,SPACES                                                    
         MVI   LINMOS,C'*'         MARKER FOR NO DATE                           
         MVI   LINMOSH+5,1                                                      
         CLC   BFRCDDTE,=2X'FF'                                                 
         BE    LR222                                                            
*                                                                               
         MVC   FULL,BFRCDDTE                                                    
         XC    FULL(2),=2X'FF'                                                  
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(9,LINMOS)                                  
         MVI   LINMOSH+5,6                                                      
*                                                                               
LR222    DS    0H                                                               
         MVC   BFORMULA,BFRCDFML                                                
         GOTO1 DISPFML,DMCB,LINFMLH                                             
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
         TM    BBILLBAS,X'50'      FIGURE OUT WHICH BILL BASIS WE HAVE          
         BNO   DFML10                                                           
         MVC   0(4,R3),=C'CNET'                                                 
         LA    R3,4(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML10   TM    BBILLBAS,X'10'                                                   
         BNO   DFML20                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML20   TM    BBILLBAS,X'40'                                                   
         BNO   DFML30                                                           
         MVC   0(5,R3),=C'CGROS'                                                
         LA    R3,5(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML30   MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
*                                                                               
DFML50   ICM   R5,15,BBILLCOM      IF NO COMMISSION                             
         BZ    DFMLX               THEN JUST BILL BASIS                         
*                                                                               
         MVI   0(R3),C','          OTHERWISE SEPARATE FIELD WITH COMMAS         
         LA    R3,1(R3)                                                         
*                                                                               
         LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WON'T FIT NICELY                 
         BNE   DFML60                                                           
         MVC   0(5,R3),=C'+100,'                                                
         LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,5(R3)                                                         
         B     DFML70                                                           
*                                                                               
DFML60   EDIT  (R5),(8,0(R3)),4,FLOAT=+,ALIGN=LEFT                              
         LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         LA    R1,7(R3)                                                         
DFML65   CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     DFML65                                                           
*                                                                               
         LA    R3,1(R1)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DFML70   MVC   0(3,R3),=C'NET'                                                  
         TM    BBILLBAS,X'01'                                                   
         BO    *+10                                                             
         MVC   0(5,R3),=C'GROSS'                                                
*                                                                               
DFMLX    OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
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
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM9FD                                                       
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
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBFML                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
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
FLTR1MKT EQU   X'20'                                     MARKET                 
*                                                                               
BFORMULA DS    0XL(L'BFRCDFML)     BINARY REPRESENTATION OF FORMULA             
BBILLBAS DS    XL(L'EBILLBAS)      2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000'=GROSS, B'0001'=NET                   
BBILLCOM DS    XL(L'EBILLCOM)      SIGNED COMMISSION RATE (99.9999)             
*                                                                               
FILTRPRD DS    CL(L'BFKPRD)        PRODUCT FILTER VALUE                         
FILTREST DS    XL(L'BFKEST)        ESTIMATE FILTER VALUE                        
FILTRMM  DS    XL(L'BFKMGR)        MGR/MKT FILTER VALUE                         
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
TABLPRD  DS    XL(L'BFKPRD)        PRODUCT                                      
TABLEST  DS    XL(L'BFKEST)        ESTIMATE                                     
TABLMGR  DS    XL(L'BFKMGR)        MARKET GROUP                                 
         ORG   TABLMGR                                                          
         DS    XL1                 SPARE                                        
TABLMKT  DS    XL(L'BFKMKT)        MARKET                                       
TABLMOS  DS    XL(L'BFRCDDTE)      MONTH OF SERVICE                             
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
LINMKTH  DS    CL(L'BILMKT1H)                                                   
LINMKT   DS    CL(L'BILMKT1)                                                    
LINMOSH  DS    CL(L'BILMOS1H)                                                   
LINMOS   DS    CL(L'BILMOS1)                                                    
LINFMLH  DS    CL(L'BILFML1H)                                                   
LINFML   DS    CL(L'BILFML1)                                                    
LINNEXTL DS    0C                                                               
*                                                                               
LNDSPLNS EQU   BILPRD2H-BILPRD1H                                                
NODSPLNS EQU   ((BILPRDLH-BILPRD1H)/LNDSPLNS)   NOT INCL 1ST INPUT LINE         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPSFM3B   05/19/08'                                      
         END                                                                    
