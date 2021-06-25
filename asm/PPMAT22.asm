*          DATA SET PPMAT22    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T40222A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T40222 - CHECKING OF PRINT INVOICES/VALKEY ONLY                       
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200) WHICH CALLS DDGENCON          
*               (T00A30) WHICH CALLS PRINT INVOICE CHECK (T40202)               
*               WHICH CALLS THIS                                                
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     PPMATFB (T402FB) -- CHECK/UPDATE SCREEN BEFORE LIST             
*               PPMATEE (T402EE) -- SUPER CHECK SCREEN FOR NEWSPAPERS           
*               PPMATED (T402ED) -- SUPER CHECK SCREEN FOR MAGAZINES            
*               PPMATEB (T402EB) -- CHECK SCREEN FOR NEWSPAPERS                 
*               PPMATEC (T402EC) -- UPDATE SCREEN FOR NEWSPAPERS                
*               PPMATDB (T402DB) -- CHECK SCREEN FOR MAGAZINES                  
*               PPMATDC (T402DC) -- UPDATE SCREEN FOR MAGAZINES                 
*               PPMATCB (T402CB) -- SCREEN FOR COMMENTS OF DETAILS              
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - WORK (MINELEM, A(REC))                                          
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
*****NOTE*****                                                                  
* IF YOU ARE MAKING CHANGES IN THIS PROGRAM THAT USE THE FIELD 'QPRD'           
* OR THAT REQUIRE ADDITIONAL CALLS TO EXISTING PRINT MATCH SUBROUTINES          
* BE AWARE THAT WITH PRODUCT VARIOUS, QPRD WILL BE *** AND THE SPECIFIC         
* PRODUCT WILL ACTUALLY BE IN THE MINIO DETAIL ELEMENT (PIMSPRD IS THE          
* FIELD).  THIS MAY AFFECT YOUR CHANGES SO YOU SHOULD LOOK AT THE               
* SUBROUTINE AND AT ALREADY EXISTING CALLS TO IT. THANKS, ABBEY                 
*                                                                               
***********************************************************************         
         EJECT                                                                  
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
* SMYE 02/00     ADDED CODE AT VKEY53 LABEL TO MAKE SURE THE SPECIAL            
*                REP FIELD (SPCLREP) HAD THE SPECIAL REP CODE WHERE             
*                APPROPRIATE (WAS FAILING TO FIND INVOICE HEADER WHEN           
*                RETURNING FROM REPORT IF SPECIAL REP CODE WAS IN               
*                HEADER - WOULD PUT OUT ERROR MESSAGE INSTEAD OF THE            
*                "REPORT ABC,NN ..SPOOLED" MESSAGE)                             
*                                                                               
*                                                                               
T40222   TITLE 'PPMAT22 - PRINT INVOICE CHECKING OVERLAY-VKEY ONLY'             
T40222   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40222*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
MAIN     B     VKEY                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
*                                                                               
         CLC   QPRD2,=C'REP'       RETURN FROM REPORT                           
         BE    VKEY05                                                           
*                                                                               
         MVI   PREVSEL,0           NO PREVIOUS SELECT ACTION                    
*                                                                               
*                                  DID WE GET CONTROL BACK FROM PAY?            
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    VKEY02                                                           
*                                                                               
         CLI   PAYFLAG,C'Y'        NO, SHOWED A MESSAGE AFTER PAY?              
         BNE   *+12                                                             
*                                                                               
         CLI   CKALREDY,C'Y'       YES, DID WE CHECK BUYS AND DETAILS?          
         BE    *+8                 YES, DON'T RESET CKALREDY FLAG               
         MVI   CKALREDY,C'N'                                                    
*                                                                               
         MVI   PAYFLAG,C'N'                                                     
         B     VKEY04                                                           
*                                                                               
VKEY02   GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVXCTL                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY,=C'PRI'    FROM THE PRINT SYSTEM?                       
         BNE   VKEY04                                                           
         CLC   GLVXFRPR,=C'PAY'    PAY PROGRAM?                                 
         BNE   VKEY04                                                           
         TM    GLVXFLG1,GLV1RETN   RETURN CALL?                                 
         BZ    VKEY04                                                           
         DROP  R1                                                               
*                                                                               
         SR    RF,RF               RETRANSMIT ALL FIELDS AFTER                  
         LA    RE,CHKOPTNH             GETTTING CONTROL BACK FROM PAY           
         CLI   0(RE),0                                                          
         BE    *+14                                                             
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         B     *-14                                                             
         MVC   1(2,RE),=X'0101'                                                 
*                                                                               
         MVI   PAYFLAG,C'Y'                                                     
*                                                                               
VKEY04   CLI   PFKEY,11            CHECK REPORT?                                
         BNE   VKEY06                                                           
*                                                                               
         B     XIT                                                              
*                                                                               
VKEY05   XC    QPRD2,QPRD2                                                      
         MVI   BITFLAG,0                                                        
         MVI   GERROR1,RPTGEND     CHECK REPORT SENT                            
         MVI   BLOCK,10                                                         
         MVC   BLOCK+1(3),SPOOLID  DISPLAY REPORT ID                            
         MVI   BLOCK+4,C','                                                     
         EDIT  (2,SPOOLRPN),(5,BLOCK+5),ALIGN=LEFT                              
         MVI   BLOCK+10,0                                                       
         L     R2,EFHREC           CURSOR TO RECORD FIELD                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
*                                                                               
         NI    CHKMEDH+4,X'FF'-X'20'   TAKE OFF VALIDATION BITS BECAUSE         
         NI    CHKCLTH+4,X'FF'-X'20'      THE REPORT OVERLAY WIPES OUT          
         NI    CHKPRDH+4,X'FF'-X'20'      OUR STORAGE AREA                      
         NI    CHKESTMH+4,X'FF'-X'20'                                           
         NI    CHKPERH+4,X'FF'-X'20'                                            
         NI    CHKPUBH+4,X'FF'-X'20'                                            
         NI    CHKINVNH+4,X'FF'-X'20'                                           
         NI    CHKREPNH+4,X'FF'-X'20'                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
* VALIDATE THE MEDIA                                                            
VKEY06   LA    R2,CHKMEDH                                                       
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VKEY08                                                           
         MVC   QMED,MYQMED                                                      
         MVC   SCRTYPE,QMED                                                     
*                                                                               
*        CLI   SCRTYPE,C'S'        SUPPLEMENTS?                                 
*        BNE   *+8                                                              
*        MVI   SCRTYPE,C'N'        YES, THEY ARE LIKE NEWSPAPER                 
*                                                                               
         CLI   SCRTYPE,C'N'        SCREEN FOR NEWSPAPER TYPES?                  
         BE    *+8                                                              
         MVI   SCRTYPE,C'M'        NO, FOR MAGAZINE TYPES                       
*                                                                               
         CLI   PFKEY,12            RETURNING FROM THE COMMENT SCREEN?           
         BE    VKEY08A             *** NEED TO VALIMED, FOR SOME REASON         
         B     VKEY20                  OTHERWISE THE CALLOV WON'T WORK          
**       B     VKEY08A             NEED TO VALIMED FOR CALLOV TO WORK           
*                                                                               
VKEY08   OI    BITFLAG,X'80'       YES, RESTART LIST                            
VKEY08A  GOTO1 VALIMED                                                          
         MVC   MYQMED,QMED                                                      
         MVC   SCRTYPE,QMED                                                     
*                                                                               
*        CLI   SCRTYPE,C'S'        SUPPLEMENTS?                                 
*        BNE   *+8                                                              
*        MVI   SCRTYPE,C'N'        YES, THEY ARE LIKE NEWSPAPER                 
*                                                                               
         CLI   SCRTYPE,C'N'        SCREEN FOR NEWSPAPER TYPES?                  
         BE    *+8                                                              
         MVI   SCRTYPE,C'M'        NO, FOR MAGAZINE TYPES                       
*                                                                               
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
         LA    R1,CHKTAGH          POINT TO WHERE SCREEN WILL BE LOADED         
         ST    R1,DMCB             USED IN 1ST PARAM FOR CALLOV                 
*                                                                               
         CLI   ACTNUM,ACTCHECK     ACTION CHECK?                                
         BNE   VKEY10                                                           
         MVI   DMCB,X'EB'          NEWSPAPER SCREEN IS DEFAULT                  
         CLI   SCRTYPE,C'N'                                                     
         BE    VKEY15                                                           
         MVI   DMCB,X'DB'          MAGAZINE SCREEN WANTED                       
         B     VKEY15                                                           
*                                                                               
VKEY10   CLI   ACTNUM,ACTUPDTE     ACTION UPDATE?                               
         BNE   VKEY10A                                                          
         MVI   DMCB,X'EC'          ACTION UPDATE, NEWSPAPER DEFAULT             
         CLI   SCRTYPE,C'N'                                                     
         BE    VKEY15                                                           
         MVI   DMCB,X'DC'          MAGAZINE SCREEN WANTED                       
         B     VKEY15                                                           
*                                                                               
VKEY10A  MVI   DMCB,X'EE'          ACTION SUPERCK, NEWSPAPER DEFAULT            
         CLI   SCRTYPE,C'N'                                                     
         BE    VKEY15                                                           
         MVI   DMCB,X'ED'          MAGAZINE SCREEN WANTED                       
*                                                                               
VKEY15   DS    0H                                                               
         GOTO1 CALLOV,DMCB         LOAD UP BOTTOM PART OF SCREEN                
         CLI   4(R1),X'FF'                                                      
         BNE   VKEY20                                                           
         DC    H'0'                                                             
*                                                                               
VKEY20   TM    BITFLAG,X'40'       SWITCHED TO ANOTHER SCREEN?                  
         BNZ   VKEY60A                                                          
*                                                                               
* VALIDATE THE CLIENT                                                           
VKEY20A  LA    R2,CHKCLTH                                                       
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VKEY25                                                           
         MVC   QCLT,MYQCLT                                                      
         MVC   SVPROF,MYPROF                                                    
         B     VKEY30                                                           
*                                                                               
VKEY25   GOTO1 VALICLT                                                          
         MVC   MYQCLT,QCLT                                                      
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   CHKCLXP,C'='                                                     
         MVC   CHKCLXP+1(L'CHKCLXP-1),CLTNM       SHOW CLIENT NAME              
         OI    CHKCLXPH+6,X'80'                                                 
         OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
         XC    WORK,WORK                                                        
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'P0IM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   CLTOFICE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFICE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   MYPROF,SVPROF       SAVE OUR PROFILE                             
*                                                                               
* VALIDATE THE PRODUCT                                                          
VKEY30   LA    R2,CHKPRDH                                                       
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VKEY31                                                           
         MVC   QPRD,MYQPRD                                                      
         B     VKEY36                                                           
*                                                                               
VKEY31   CLC   =C'***',8(R2)       PRODUCT VARIOUS?                             
         BNE   VKEY31A                                                          
         MVC   PRDNM,=C'VARIOUS              '                                  
         MVC   QPRD,=C'***'                                                     
         B     VKEY32                                                           
VKEY31A  GOTO1 VALIPRD                                                          
VKEY32   MVC   MYQPRD,QPRD                                                      
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVI   CHKPDXP,C'='                                                     
         MVC   CHKPDXP+1(L'CHKPDXP-1),PRDNM       SHOW PRODUCT NAME             
         OI    CHKPDXPH+6,X'80'                                                 
         OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
* VALIDATE THE ESTIMATE                                                         
VKEY36   LA    R2,CHKESTMH                                                      
         OI    6(R2),X'20'                                                      
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    *+14                                                             
         MVC   BEST,MYBEST         YES, COPY WHAT WE VALIDATED                  
         B     VKEY38                                                           
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKEY36A                                                          
         XC    BEST,BEST                                                        
         MVC   MYBEST,BEST                                                      
         B     VKEY38                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VKEY36A  CLC   =C'***',CHKPRD                                                   
         BNE   VKEY37A                                                          
         PACK  DUB,CHKESTM                                                      
         CVB   R1,DUB                                                           
         STH   R1,MYBEST                                                        
         B     VKEY37                                                           
VKEY37A  GOTO1 VALIEST                                                          
         MVC   MYBEST,BEST                                                      
VKEY37   OI    4(R2),X'20'                                                      
         MVI   CHKESXP,C'='                                                     
         MVC   CHKESXP+1(L'CHKESXP-1),ESTNM                                     
         OI    CHKESXPH+6,X'80'                                                 
         OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
* VALIDATE THE PERIOD                                                           
VKEY38   LA    R2,CHKPERH                                                       
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'                                                      
         BZ    VKEY38A                                                          
         MVC   INVSTDT,MYSTDT                                                   
         MVC   INVENDDT,MYENDDT                                                 
         B     VKEY39                                                           
*                                                                               
VKEY38A  LA    R3,CHKPER                                                        
         ICM   R3,8,CHKPERH+5                                                   
         GOTO1 PERVAL,DMCB,(R3),(X'20',PERVALST)                                
         CLI   DMCB+4,X'04'                                                     
         BE    VKEY38ER                                                         
*                                                                               
         CLI   DMCB+4,0                                                         
         BE    *+12                                                             
VKEY38ER LA    R2,CHKPERH                                                       
         B     INVPER                                                           
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   INVSTDT,PVALBSTA                                                 
         MVC   INVENDDT,PVALBEND                                                
         CLC   INVSTDT,INVENDDT                                                 
         BH    VKEY38ER                                                         
         MVC   MYSTDT,INVSTDT                                                   
         MVC   MYENDDT,INVENDDT                                                 
*                                                                               
* VALIDATE THE SPECIAL REP                                                      
VKEY39   LA    R2,CHKREPNH                                                      
         OI    6(R2),X'20'                                                      
         TM    4(R2),X'20'                                                      
         BZ    *+14                                                             
         MVC   SPCLREP,MYSREP                                                   
         B     VKEY40                                                           
*                                                                               
         XC    SPCLREP,SPCLREP                                                  
         CLI   5(R2),0                                                          
         BE    VKEY40                                                           
         CLI   CHKREPX,0           DID WE HAVE SOMETHING THERE ALREADY?         
         BNE   VKEY40              YES, NOT A SPECIAL REP                       
*                                                                               
         MVI   CHKREPX,C'*'        PUT A '*' IF SPECIAL REP                     
         MVC   SPCLREP,CHKREPN                                                  
         MVC   MYSREP,SPCLREP                                                   
*                                                                               
* VALIDATE THE PUB                                                              
VKEY40   LA    R2,CHKPUBH                                                       
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VKEY45                                                           
******   BZ    VKEY45A                                                          
         MVC   BPUB,MYBPUB                                                      
*                                                                               
         CLC   =X'FFFF',BPUB+4     ALL ZONES AND EDITIONS?                      
         BNE   *+8                                                              
         OI    GLOBFLG1,X'80'      YES, SET THAT BIT ON                         
*                                                                               
         ZAP   PUBCASHD,MYPUBCD                                                 
         ZAP   PUBAGYCM,MYPUBAC                                                 
         MVC   MAGFREQ,MYMAGFRQ                                                 
         MVC   PUBTAXES,MYTAXES                                                 
         MVC   PUBGSTAX,MYPUBGST                                                
         B     VKEY50                                                           
*                                                                               
VKEY45   OI    6(R2),X'80'         CHANGE ALL '.'S TO ','S                      
         LA    R0,L'CHKPUB                                                      
         LA    R1,CHKPUB                                                        
VKEY45LP CLI   0(R1),C'.'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VKEY45LP                                                      
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         CLI   DMCB+4,0                                                         
         BE    MISSFLD                                                          
*                                                                               
         CLI   DMCB+4,2            2 LINES?                                     
         BL    VKEY45A             NO                                           
*                                                                               
         LA    R3,BLOCK+32         LOOK AT THE SECOND LINE                      
         ZIC   R0,5(R2)            SAVE LENGTH OF PUB                           
         CLC   =C'ALL',12(R3)      ALL ZONES AND EDITIONS?                      
         BNE   VKEY45A                                                          
*                                                                               
         LA    R3,BLOCK                                                         
         TM    2(R3),X'80'         VALID NUMERIC?                               
         BZ    INVLNUM                                                          
         OI    4(R2),X'08'       YES, JUST VALIDATE THE NUMERIC PART            
         MVC   5(1,R2),0(R3)                                                    
         OI    GLOBFLG1,X'80'      SET ALL ZONES & EDITIONS BIT                 
*                                                                               
VKEY45A  GOTO1 VALIPUB                                                          
         STC   R0,5(R2)            RESTORE LENGTH OF PUB                        
         MVC   SVPROF,MYPROF       SVPROF GETS CLOBBERED FOR SOME               
*                                      REASON FOR PUB 11335520                  
*                                                                               
         TM    GLOBFLG1,X'80'      IF ALL ZONES AND EDITIONS                    
         BZ    *+10                                                             
         MVC   BPUB+4(2),=X'FFFF'  THEN X'FF' FILL ZONE AND EDITION             
*                                                                               
         MVC   MYBPUB,BPUB                                                      
         ZAP   MYPUBCD,PUBCASHD                                                 
         ZAP   MYPUBAC,PUBAGYCM                                                 
         MVC   MYMAGFRQ,MAGFREQ                                                 
         MVC   MYTAXES,PUBTAXES                                                 
         MVC   MYPUBGST,PUBGSTAX                                                
         OI    4(R2),X'20'         HAS BEEN VALIDATED                           
         MVC   CHKPBXP,PUBNM       SHOW PUB NAME                                
         OI    CHKPBXPH+6,X'80'                                                 
         OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
* VALIDATE THE YEAR                                                             
VKEY50   LA    R2,CHKYEARH                                                      
         OI    6(R2),X'2C'         DON'T ALLOW USER TO CHANGE FIELD AND         
*                                      HIDE THE FIELD                           
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    *+14                                                             
         MVC   QYEAR,MYQYEAR                                                    
         B     VKEY52                                                           
*                                                                               
         MVC   QYEAR,CHKYEAR                                                    
         MVC   MYQYEAR,QYEAR                                                    
*                                                                               
VKEY52   GOTO1 MNIOINIT            INITIALIZE MINIO                             
*                                                                               
*                                                                               
* VALIDATE THE INVOICE NUM                                                      
         LA    R2,CHKINVNH                                                      
         OI    6(R2),X'20'         DON'T ALLOW USER TO CHANGE FIELD             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BZ    VKEY53                                                           
         MVC   LSTHDRSQ,MYHDRSQ    RESTORE OUR SAVED VALUES                     
         XC    PUBPYREP,PUBPYREP                                                
         CLI   CHKREPX,C'*'                                                     
         BE    *+10                                                             
         MVC   PUBPYREP,CHKREPN                                                 
         B     VKEY58                                                           
*                                                                               
VKEY53   DS    0H                                                               
         CLI   CHKREPX,C'*'        SPECIAL REP ?                                
         BNE   *+10                NO                                           
         MVC   SPCLREP,CHKREPN                                                  
         GOTO1 GTINVHDR,DMCB,CHKINVN,CHKESTM                                    
         BE    *+12                GET INVOICE HEADER INFORMATION               
         LA    R2,CHKINVNH                 BASED ON INVOICE NUMBER              
         B     NOHDR                       EST, SREP, AND PERIOD                
*                                                                               
VKEY54   OI    CHKOPTNH+4,X'80'                                                 
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
         MVC   MYHDRSQ,LSTHDRSQ                                                 
*                                                                               
         CLC   PIMSTDT,INVSTDT     MAKE SURE THE PERIOD IS THE SAME             
         BNE   DIFPER                                                           
         CLC   PIMENDDT,INVENDDT                                                
         BNE   DIFPER                                                           
*                                                                               
         MVI   MYGRSNET,C'G'       ASSUME GROSS AMOUNT FIRST                    
         TM    PIMSTAT,X'40'                                                    
         BNZ   VKEY56                                                           
         MVI   MYGRSNET,C'N'       NO, NET AMOUNT                               
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VKEY54A                                                          
         LA    R2,CK1GROSH         R2 = WHERE '---GROSS---' IS                  
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2GROSH                                                      
         B     VKEY54C                                                          
*                                                                               
VKEY54A  CLI   ACTNUM,ACTUPDTE                                                  
         BNE   VKEY54B                                                          
         LA    R2,UP1GROSH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2GROSH                                                      
         B     VKEY54C                                                          
*                                                                               
VKEY54B  LA    R2,SC1GROSH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2GROSH                                                      
*                                                                               
VKEY54C  MVC   8(L'CK1GROS,R2),=C'----NET----'   PUT NET OUT INSTEAD            
*                                                                               
VKEY56   MVI   CASHDISC,C'Y'       ASSUME CASH DISCOUNT FIRST                   
         TM    PIMSTAT,X'20'                                                    
         BZ    *+8                                                              
         MVI   CASHDISC,C'N'       NO CASH DISCOUNT                             
*                                                                               
         ZAP   INVHDRAM,PIMAMT     COPY INVOICE HEADER AMOUNT                   
*                                                                               
         OI    CHKINVNH+4,X'20'    VALIDATE THE INVOICE NUMBER                  
*                                                                               
VKEY58   OC    CHKREPN,CHKREPN     IF THERE IS A SPECIAL REP?                   
         BNZ   VKEY58A                                                          
         OC    PUBPYREP,PUBPYREP   NONE, ANY PAYING REP?                        
         BZ    VKEY60              NO, NONE                                     
         MVC   CHKREPN,PUBPYREP                                                 
*                                                                               
VKEY58A  LA    R2,CHKREPNH                                                      
*                                                                               
         TM    4(R2),X'20'         VALIDATED ALREADY?                           
         BNZ   VKEY60              YES                                          
*                                                                               
         MVI   5(R2),4                                                          
         OI    4(R2),X'08'                                                      
         GOTO1 VALIREP                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PREPELEM,R6                                                      
*                                                                               
         CLI   CHKREPX,C'*'        IF SPECIAL REP FROM INVOICE                  
         BE    *+14                                                             
         MVC   CHKREPX,PREPNAME                                                 
         B     *+10                                                             
         MVC   CHKREPX+1(L'CHKREPX-1),PREPNAME    THEN LEAVE '*' THERE          
*                                                                               
         OI    CHKREPNH+4,X'20'    VALIDATE THE REP NUMBER                      
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
VKEY60   TM    OVLYFLAG,X'88'      COMMENTS SCREEN OR TOTAL SCREEN?             
         BNZ   VKEYX               YES, NO OPTIONS FOR COMMENTS SCREEN          
*                                                                               
* VALIDATE THE OPTIONS                                                          
         TM    CHKOPTNH+4,X'80'    OPTIONS FIELD CHANGED?                       
         BZ    VKEYX                                                            
         OI    BITFLAG,X'80'       YES, RESTART LIST                            
*                                                                               
VKEY60A  CLI   CHKOPTNH+5,0        NOTHING IN OPTIONS FIELD?                    
         BNE   VKEY65                                                           
*                                                                               
         MVC   CHKOPTN(2),SVPROF+1   YES, GET OPTION FROM PROFILE               
         MVI   CHKOPTNH+5,2                                                     
         OI    CHKOPTNH+6,X'80'                                                 
         CLI   SVPROF+2,0          ONE BYTE FOR OPTION?                         
         BE    *+12                                                             
         CLI   SVPROF+2,C'.'                                                    
         BNE   VKEY65                                                           
         MVI   CHKOPTN+1,0         YES                                          
         MVI   CHKOPTNH+5,1                                                     
*                                                                               
VKEY65   LA    R3,OPTNTABL         R3 = A(OPTIONS TABLE)                        
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VKEY65A                                                          
         LA    R2,CK1NETH          R2 = WHERE '----NET----' IS                  
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2NETH                                                       
         B     VKEY70                                                           
*                                                                               
VKEY65A  CLI   ACTNUM,ACTUPDTE                                                  
         BNE   VKEY65B                                                          
         LA    R2,UP1NETH                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2NETH                                                       
         B     VKEY70                                                           
*                                                                               
VKEY65B  LA    R2,SC1NETH                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2NETH                                                       
*                                                                               
VKEY70   CLI   0(R3),X'FF'                                                      
         BNE   *+12                                                             
         LA    R2,CHKOPTNH                                                      
         B     INVLOPT                                                          
*                                                                               
         CLI   CHKOPTN,C'Z'        IF OPTION IS ZONE/EDITION                    
         BNE   VKEY70Z                                                          
         CLC   =X'FFFF',BPUB+4     ALL ZONES AND EDITIONS?                      
         BE    VK70UNP1                                                         
         LA    R2,CHKOPTNH                                                      
         B     INVLOPT             ELSE, INVALID OPTION ERROR                   
*                                                                               
VKEY70Z  CLC   =C'CT',CHKOPTN                                                   
         BNE   VK70PROT                                                         
         CLC   =C'***',QPRD        MUST BE FOR PRODUCT VARIOUS                  
         BE    *+12                UNP PRD FLD ONLY                             
         LA    R2,CHKOPTNH                                                      
         B     INVLOPT             ELSE INVALID                                 
*                                                                               
VK70UNP1 CLI   ACTNUM,ACTUPDTE     UNPROTECT CTPBI ON UPPER SCREEN              
         BE    VK70UNP                                                          
*                                                                               
         LA    R0,CK1PFLNH        ** FOR NEWSPAPERS                             
         LA    RE,CK1SEL0H         BOTTOM HALF OF SCREEN                        
         USING SCRLIN1D,RE                                                      
         LA    R1,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    VK70UNP2                                                         
         LA    R0,CK2PFLNH         ** FOR MAGAZINES                             
         LA    RE,CK2SEL0H                                                      
         USING SCRLIN2D,RE                                                      
         LA    R1,SLN2NETH                                                      
*                                                                               
VK70UNP2 OI    4(R1),X'20'         VALIDATE AND                                 
         NI    1(R1),X'FF'-X'20'   UNPROTECT NET FLD                            
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SLN1LEN(R1)                                                   
         B     *+8                                                              
         LA    R1,SLN2LEN(R1)                                                   
         CR    R1,R0                                                            
         BL    VK70UNP2                                                         
         B     VKEY70E                                                          
*                                                                               
VK70UNP  CLI   SCRTYPE,C'N'                                                     
         BNE   VKEY70A                                                          
************************                                                        
*  FOR  UPDATE SCREENS *                                                        
************************                                                        
         LA    R0,UP1PFLNH         **NEWSPAPER                                  
         LA    RE,UP1SEL1H                                                      
         USING SCRLIN1D,RE                                                      
         LA    R1,SLN1SELH                                                      
         B     VKEY70B                                                          
*                                                                               
VKEY70A  LA    R0,UP2PFLNH         **MAGAZINE                                   
         LA    RE,UP2SEL1H                                                      
         USING SCRLIN2D,RE                                                      
         LA    R1,SLN2SELH                                                      
*                                                                               
VKEY70B  CLI   8(R1),C'*'          IF IT IS ALREADY MATCHED                     
         BNE   VKEY70B1                                                         
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SLN1LEN(R1)                                                   
         B     *+8                                                              
         LA    R1,SLN2LEN(R1)                                                   
         B     VKEY70B2                                                         
VKEY70B1 CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SLN1NETH-SLN1SELH(R1)                                         
         B     *+8                                                              
         LA    R1,SLN2NETH-SLN2SELH(R1)                                         
         OI    4(R1),X'20'         VALIDATE AND                                 
         NI    1(R1),X'FF'-X'20'   UNPROTECT FLD                                
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SLN1NXTL-SLN1NETH(R1)                                         
         B     *+8                                                              
         LA    R1,SLN2NXTL-SLN2NETH(R1)                                         
VKEY70B2 CR    R1,R0               END OF SCREEN?                               
         BL    VKEY70B                                                          
         DROP  RE                                                               
         B     VKEY70E                                                          
*                                                                               
VK70PROT DS    0H                  FIELDS WERE UNPROTECTED                      
         LA    R0,CK1PFLNH         AND MUST NOW BE PROTECTED                    
         LA    RE,CK1SEL0H                                                      
         CLI   ACTNUM,ACTUPDTE                                                  
         BNE   *+8                                                              
         LA    RE,UP1SEL1H                                                      
         USING SCRLIN1D,RE                                                      
         LA    R1,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    VK70PRT1                                                         
         LA    R0,CK2PFLNH                                                      
         LA    RE,CK2SEL0H                                                      
         CLI   ACTNUM,ACTUPDTE                                                  
         BNE   *+8                                                              
         LA    RE,UP2SEL1H                                                      
         USING SCRLIN2D,RE                                                      
         LA    R1,SLN2NETH                                                      
*                                                                               
VK70PRT1 OI    1(R1),X'20'         PROTECT THE FIELD                            
         OI    6(R1),X'80'                                                      
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SLN1LEN(R1)                                                   
         B     *+8                                                              
         LA    R1,SLN2LEN(R1)                                                   
         CR    R1,R0                                                            
         BL    VK70PRT1                                                         
*                                                                               
VKEY70E  ZIC   R1,CHKOPTNH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),CHKOPTN                                                  
         BE    VKEY80                                                           
*                                                                               
VKEY70NX LA    R3,OPTNLEN(R3)      DOESN'T MATCH, GET NEXT ENTRY                
         B     VKEY70                                                           
*                                                                               
VKEY80   MVC   8(11,R2),2(R3)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         CLC   =C'***',QPRD                                                     
         BNE   VKEY80A2                                                         
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VKEY80A1                                                         
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+18                                                             
         MVC   SC1ITEM+46(5),=C'PROD '                                          
         OI    SC1ITEMH+6,X'80'                                                 
         B     VKEY80A2                                                         
         MVC   SC2ITEM+34(5),=C'PROD '                                          
         OI    SC2ITEMH+6,X'80'                                                 
         B     VKEY80A2                                                         
*                                                                               
VKEY80A1 CLI   SCRTYPE,C'N'                                                     
         BNE   *+18                                                             
         MVC   SC1COLS+46(5),=C'PROD '                                          
         OI    SC1COLSH+6,X'80'                                                 
         B     *+14                                                             
         MVC   SC2COLS+34(5),=C'PROD '                                          
         OI    SC2COLSH+6,X'80'                                                 
*                                                                               
****     UNPROTECT THE BOTTOM HALF OF THE PRD COLUMN                            
         LA    R0,CK1PFLNH                                                      
         CLI   ACTNUM,ACTUPDTE                                                  
         BE    VKEY80AA                                                         
*                                                                               
         LA    RE,CK1SEL0H                                                      
         USING SCRLIN1D,RE                                                      
         LA    R2,SLN1CTPH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+12                                                             
         LA    RE,CK2SEL0H                                                      
         USING SCRLIN2D,RE                                                      
         LA    R2,SLN2CTPH                                                      
         B     VKEY80AB                                                         
*                                                                               
VKEY80AA LA    RE,UP1SEL1H                                                      
         USING SCRLIN1D,RE                                                      
         LA    R2,SLN1CTPH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+12                                                             
         LA    RE,UP2SEL1H                                                      
         USING SCRLIN2D,RE                                                      
         LA    R2,SLN2CTPH                                                      
         DROP  RE                                                               
*                                                                               
VKEY80AB NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R2,SLN1LEN(R2)                                                   
         B     *+8                                                              
         LA    R2,SLN2LEN(R2)                                                   
         CR    R2,R0                                                            
         BL    VKEY80AB                                                         
*                                                                               
VKEY80A2 TM    BITFLAG,X'40'       SWITCHED TO ANOTHER SCREEN?                  
         BZ    VKEYX               NO                                           
*                                                                               
         CLI   MYGRSNET,C'G'       DO WE NEED TO CHANGE HEADLINES?              
         BE    VKEY90              NO, STILL GROSS                              
*                                                                               
         CLI   ACTNUM,ACTCHECK     YES, IT SHOULD BE '----NET----'              
         BNE   VKEY80A                                                          
         LA    R2,CK1GROSH         R2 = WHERE '---GROSS---' IS                  
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2GROSH                                                      
         B     VKEY80C                                                          
*                                                                               
VKEY80A  CLI   ACTNUM,ACTUPDTE                                                  
         BNE   VKEY80B                                                          
         LA    R2,UP1GROSH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2GROSH                                                      
         B     VKEY80C                                                          
*                                                                               
VKEY80B  LA    R2,SC1GROSH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2GROSH                                                      
*                                                                               
VKEY80C  MVC   8(L'CK1GROS,R2),=C'----NET----'   PUT NET OUT INSTEAD            
         OI    6(R2),X'80'         AND TRANSMIT                                 
*                                                                               
VKEY90   NI    BITFLAG,X'FF'-X'40'  WE'RE IN THE RIGHT SCREEN NOW               
         MVC   QPRD2,=C'CDL'                                                    
*        GOTO1 CALLOV,DMCB,(X'04',0),(0,0)    USE CHECK DISPLAY LOGIC           
*        CLI   DMCB+4,X'FF'                                                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        L     RF,DMCB                                                          
*        GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVPER   MVI   GERROR1,INVLPER                                                  
         B     ERREXIT                                                          
*                                                                               
INVLNUM  MVI   GERROR1,NOTNUM                                                   
         B     ERREXIT                                                          
*                                                                               
DIFPER   MVI   GERROR1,DIFFPER                                                  
         B     ERREXIT                                                          
*                                                                               
BIGSIZE  MVI   GERROR1,SIZEBIG                                                  
         B     ERREXIT                                                          
*                                                                               
INVLRATE MVI   GERROR1,INVRATE                                                  
         B     ERREXIT                                                          
*                                                                               
INVLAMT  MVI   GERROR1,INVAMT                                                   
         B     ERREXIT                                                          
*                                                                               
INVLPREM MVI   GERROR1,INVPREM                                                  
         B     ERREXIT                                                          
*                                                                               
NOPREM   MVI   GERROR1,MISSPREM                                                 
         B     ERREXIT                                                          
*                                                                               
DATEPER  MVI   GERROR1,DTNOTPER                                                 
         B     ERREXIT                                                          
*                                                                               
INVLOPT  MVI   GERROR1,INVOPT                                                   
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         B     ERREXIT                                                          
*                                                                               
NOHDR    MVI   GERROR1,NOINVHDR                                                 
         B     ERREXIT                                                          
*                                                                               
INVLSEL  MVI   GERROR1,INVSEL                                                   
         B     ERREXIT                                                          
*                                                                               
INVLDUPL MVI   GERROR1,DUPLNTRY                                                 
         B     ERREXIT                                                          
*                                                                               
INVLDREP MVI   GERROR1,INVLREP                                                  
         B     ERREXIT                                                          
*                                                                               
INVLPBI  MVI   GERROR1,PBIFORBY    PBI ONLY FOR BUYS                            
         B     ERREXIT                                                          
*                                                                               
INVLPAID MVI   GERROR1,MPAIDBUY    CAN'T MANIPULATE A PAID INSERTION            
         B     ERREXIT                                                          
*                                                                               
ALRDYMTC MVI   GERROR1,ISMATCHD    INSERTION IS ALREADY MATCHED                 
         B     ERREXIT                                                          
*                                                                               
NTMATCHD MVI   GERROR1,NOTMTCHD    INSERTION IS NOT MATCHED                     
         B     ERREXIT                                                          
*                                                                               
TORNINSR MVI   GERROR1,TORNALRD    INSERTION HAS TEARSHEET ALREADY              
         B     ERREXIT                                                          
*                                                                               
CANTMCOR MVI   GERROR1,MTCHCORR    CAN'T MATCH A CORRECTED BUY                  
         B     ERREXIT                                                          
*                                                                               
CANTCORM MVI   GERROR1,CORRMTCH    CAN'T CORRECTED A MATCHED BUY                
         B     ERREXIT                                                          
*                                                                               
CANTPCOR MVI   GERROR1,PAYCORR     CAN'T PAY A CORRECTED BUY                    
         B     ERREXIT                                                          
*                                                                               
PAYAPAID MVI   GERROR1,PAIDALRD    INSERTION HAS BEEN PAID ALREADY              
         B     ERREXIT                                                          
*                                                                               
MISSPROF MVI   GERROR1,NEEDPROF    NO A0A PROFILE                               
         B     ERREXIT                                                          
*                                                                               
NOMTCHBY MVI   GERROR1,MATCHEDQ    NO MATCHED BUYS                              
         B     PAYPERR                                                          
*                                                                               
NOMTCHTS MVI   GERROR1,MTCHTSEQ    NO MATCHED BUYS WITH TEARSHEET               
         B     PAYPERR                                                          
*                                                                               
NTALMTCH MVI   GERROR1,ALLMTCHQ    NOT ALL MATCHED BUYS                         
         B     PAYPERR                                                          
*                                                                               
NTALMTTS MVI   GERROR1,AMTCHTSQ    NOT ALL MATCHED BUYS WITH TEARSHEET          
         B     PAYPERR                                                          
*                                                                               
PAYPERR  LA    R2,CHKOPTNH                                                      
         B     ERREXIT                                                          
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         B     MYERRXIT                                                         
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE ANY CHANGES ON ERRORS           
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE CHECK                                     
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
MONTHLST DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',X'00'                    
***********************************************************************         
* OPTIONS TABLE                                                                 
***********************************************************************         
OPTNTABL DS    0C                                                               
         DC    C'A.',CL11'--ADCODE---'                                          
OPTNNTRY DC    C'B.',CL11'-BILLDATE--'                                          
         DC    C'C.',CL11'----CD-----'                                          
         DC    C'CT',CL11'CTPBI------'                                          
         DC    C'G.',CL11'---GROSS---'                                          
         DC    C'GL',CL11'---GLCD----'                                          
         DC    C'GS',CL11'----GST----'                                          
         DC    C'L.',CL11'--LASTIO---'                                          
         DC    C'N.',CL11'----NET----'                                          
         DC    C'NL',CL11'---NLCD----'                                          
         DC    C'P.',CL11'--PAYDATE--'                                          
         DC    C'T.',CL11'----TAX----'                                          
         DC    C'Z.',CL11'-ZONE,EDTN-'                                          
*                                                                               
         DC    X'FF'                                                            
*                                                                               
OPTNLEN  EQU   OPTNNTRY-OPTNTABL                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEED          (OUR SUPERCK SCREEN FOR NEWSPAPER)           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEDD          (OUR SUPERCK SCREEN FOR MAGAZINE)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATECD          (OUR UPDATE SCREEN FOR NEWSPAPER)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDCD          (OUR UPDATE SCREEN FOR MAGAZINE)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCBD          (OUR COMMENTS SCREEN)                        
         EJECT                                                                  
* DDGENTWA                                                                      
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* PUBGENEL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         PRINT ON                                                               
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* MY STORAGE AREA                                                               
       ++INCLUDE PPMATWK02D        (SYSTEM AREAS)                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPMAT22   05/01/02'                                      
         END                                                                    
