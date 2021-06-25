*          DATA SET PPMAT04    AT LEVEL 166 AS OF 05/01/02                      
*PHASE T40204A                                                                  
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 03/02    FIX BUG FOR PRODUCT VARIOUS (***) - WAS                         
*               ONLY TESTING FOR BUYDATE AND BUYLINE TO DISTINGUISH             
*               DISCREPANCIES, THUS IF DIFFERENT PRODUCTS HAD THE SAME          
*               BUYDATE, THE DISCREPANCY ENTERED FOR ONE PRODUCT WOULD          
*               BE LISTED FOR THE FIRST PRODUCT ENCOUNTERED, EVEN IF            
*               THIS ENTRY HAD NO DISCREPANCIES                                 
* SMYE 04/26/01 FIX SUPERCK DISPLAY AT DISPCORR                                 
* SMYE 4/01     FIX DISPLAY OF PBUYKLIN GREATER THAN 9                          
* BPLA 2/98     CHANGE OUTDOOR SPACE DISPLAY                                    
* BPLA 2/98     FIX DISPLAY OF LINE NUMBER                                      
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T40204 - CHECKING OF PRINT INVOICES                                   
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
* ***NOTE*** IF YOU ARE MAKING CHANGES TO THIS PROGRAM BE AWARE THAT            
* FOR PRODUCT VARIOUS, QPRD IS ***.  THIS WILL AFFECT ANY CALLS YOU             
* MAKE TO ALREADY EXISTING PRINT MATCH SUBROUTINES.  IF YOU DO CALL             
* ANY, THEN BE SURE TO COMPARE YOUR CALLS WITH EXISTING CALLS TO                
* THE SUBROUTINE TO SEE IF QPRD IS HANDLED IN AN UNUSUAL WAY.                   
* THANKS, ABBEY                                                                 
***********************************************************************         
T40204   TITLE 'PPMAT04 - INVOICE CHECK DISPLAY OVERLAY'                        
T40204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40204*,R7,RR=R3                                              
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
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE LISTS THE RECORDS                                                
***********************************************************************         
LR       MVI   WINDOW,0            WHICH WINDOW NOT DETERMINED                  
*                                                                               
         CLI   ACTNUM,ACTUPDTE                                                  
         BNE   LR10                UPDATE ONLY HAS ONE WINDOW                   
         LA    R1,UP1SEL1H         NEW CURSOR POSITION                          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,UP2SEL1H                                                      
         OI    6(R1),X'40'                                                      
         B     LR100                                                            
*                                                                               
LR10     CLI   ACTNUM,ACTSUPCK                                                  
         BNE   LR20                SUPERCK ONLY HAS ONE WINDOW                  
         LA    R1,SC1SEL1H         NEW CURSOR POSITION                          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,SC2SEL1H                                                      
         OI    6(R1),X'40'                                                      
         B     LR100                                                            
*                                                                               
LR20     L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         LH    R0,TIOBCURS         CURSOR IN WHICH WINDOW?                      
         DROP  R1                                                               
*                                                                               
         LA    R1,CK1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2SEL1H                                                      
*                                                                               
         CLM   R0,3,2(R1)                                                       
         BL    LR90                CURSOR IS ABOVE BOTH WINDOWS                 
*                                                                               
         LA    R1,CK1ITEMH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2ITEMH                                                      
*                                                                               
         CLM   R0,3,2(R1)                                                       
         BNL   LR50                                                             
         MVI   WINDOW,C'U'         CURSOR IS IN UPPER WINDOW                    
*                                                                               
         LA    R1,CK1SEL1H         NEW CURSOR POSITION                          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2SEL1H                                                      
*                                                                               
         OI    6(R1),X'40'                                                      
         B     LR100                                                            
*                                                                               
LR50     LA    R1,CK1PFLNH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2PFLNH                                                      
*                                                                               
         CLM   R0,3,2(R1)                                                       
         BNL   LR90                CURSOR IS BELOW BOTH WINDOWS                 
*                                                                               
         MVI   WINDOW,C'L'         CURSOR IS IN LOWER WINDOW                    
         LA    R1,CK1SEL0H         NEW CURSOR POSITION                          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2SEL0H                                                      
*                                                                               
         OI    6(R1),X'40'                                                      
         B     LR100                                                            
*                                                                               
LR90     OI    CHKOPTNH+6,X'40'                                                 
*                                                                               
LR100    CLI   ACTNUM,ACTUPDTE                                                  
         BNE   *+12                                                             
         BAS   RE,CALCBUYS                                                      
         B     LR110                                                            
*                                                                               
         GOTO1 =A(LISTBUYS),DMCB,(RC),RR=RELO                                   
LR110    ZAP   P11,BUYAMNTG                                                     
         CLI   MYGRSNET,C'G'                                                    
         BE    *+10                                                             
         ZAP   P11,BUYAMNTN                                                     
         EDIT  (P11,P11),(13,CHKBGRS),2,ALIGN=LEFT,MINUS=YES                    
         OI    CHKBGRSH+6,X'80'                                                 
         EJECT                                                                  
*                                                                               
* LOWER PORTION OF THE CHECK SCREEN                                             
*                                                                               
LR200    DS    0H                                                               
         MVC   NEWLOWTB,LOWERTBL                                                
*                                                                               
*  TWAXC LOWER PORTION OF SCREEN AND ALSO VALIDATE FIELDS                       
*                                                                               
         CLI   ACTNUM,ACTSUPCK                                                  
         BE    LR300                                                            
*                                                                               
         CLI   ACTNUM,ACTCHECK     ACTION CHECK?                                
         BNE   LR200A                                                           
*                                                                               
         LA    R1,CK1SEL0H                                                      
         LA    R2,CK1LSTLH                                                      
         USING SCRLIN1D,R2                                                      
         LA    RF,SLN1ESTH                                                      
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    LR210                                                            
*                                                                               
         LA    R1,CK2SEL0H                                                      
         LA    R2,CK2LSTLH                                                      
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
         DROP  R2                                                               
         B     LR210                                                            
*                                                                               
LR200A   LA    R1,UP1SEL1H         UPDATE'S LINES                               
         LA    R2,UP1LSTLH                                                      
         USING SCRLIN1D,R2                                                      
         LA    RF,SLN1ESTH                                                      
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    LR210                                                            
*                                                                               
         LA    R1,UP2SEL1H                                                      
         LA    R2,UP2LSTLH                                                      
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
LR210    ZIC   RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         LTR   RE,RE                                                            
         BM    LR220                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)       NULL OUT THE DATA                            
         OI    6(R1),X'80'         TRANSMIT FIELD                               
         OI    4(R1),X'20'         VALIDATE THE FIELD                           
         ZIC   RE,0(R1)                                                         
         BXLE  R1,RE,LR210                                                      
*                                                                               
LR220    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ    DEFAULT, START AT VERY BEGINNING             
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         TM    BITFLAG,X'80'       FIRST TIME IN CHECK?                         
         BNZ   LRMINHI             YES, START FROM BEGINNING                    
*                                                                               
         TM    LBITFLAG,X'40'      DISPLAY NEWEST?                              
         BZ    LR220B                                                           
         MVC   MINEKEY(L'PINVMINI),LSTMINSQ                                     
         B     LRMINHI                                                          
*                                                                               
LR220A   MVC   MINEKEY(L'PINVMINI),NEWLOWTB   REDISPLAY THE PAGE                
         B     LRMINHI                                                          
*                                                                               
LR220B   CLI   WINDOW,C'U'         JUST UPPER WINDOW?                           
         BE    LR220A              YES, REDISPLAY THE PAGE                      
*                                                                               
         CLI   PFKEY,5             START LIST FROM VERY BEGINNING?              
         BE    LRMINHI             YES                                          
*                                                                               
         CLI   PFKEY,6             BOTTOM OF THE LIST?                          
         BNE   *+12                                                             
         BAS   RE,MINPF6           SET MINEKEY TO 3RD FROM BOTTOM               
         B     LRMINHI                                                          
*                                                                               
         CLI   PFKEY,7             PREVIOUS?                                    
         BNE   *+12                                                             
         BAS   RE,MINPF7           SET MINEKEY TO 3RD FROM TOP                  
         B     LRMINHI                                                          
*                                                                               
         CLI   PFKEY,0             IF NOT NEXT                                  
         BNE   LR220A              REDISPLAY PAGE FOR NOW                       
         TM    LBITFLAG,X'80'      NO MORE FOR NEXT?                            
         BNZ   LRMINHI             THEN DISPLAY FIRST PAGE AGAIN                
*                                                                               
         LA    R1,LOWERTBL         A(NEXT LOWER ENTRY)                          
         AH    R1,DLSTLENT         A(NEXT LOWER ENTRY)                          
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         ZICM  R1,MINEKEY+2,2      INCREMENT DETAIL SEQ TO GET NEXT             
         LA    R1,1(R1)                                                         
         STCM  R1,3,MINEKEY+2                                                   
*                                                                               
LRMINHI  MVI   LBITFLAG,0          CLEAR ANY PREVIOUS FLAGS                     
LRMINHI2 XC    LOWERTBL,LOWERTBL   CLEAR LOWER MINIO KEY TABLE                  
         XC    DLSTLENT,DLSTLENT                                                
         MVI   LINENUM,0                                                        
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    *+12                                                             
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         B     LR300                                                            
*                                                                               
         L     R6,MINELEM          DETAIL ELEMENT STILL?                        
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BE    LRMIN00                                                          
*                                                                               
         TM    LBITFLAG,X'80'                                                   
         BNZ   LR300                                                            
*                                                                               
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         OC    NEWLOWTB(L'PINVMINI),NEWLOWTB                                    
         BZ    LR300                                                            
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         MVC   MINEKEY(L'PINVMINI),NEWLOWTB                                     
         B     LRMINHI2                                                         
*                                                                               
LRMIN00  CLC   PIMDTLS1,LSTHDRSQ   HEADER SEQUENCE MATCH?                       
         BE    LRMIN01                                                          
*                                                                               
         TM    LBITFLAG,X'80'                                                   
         BNZ   LR300                                                            
*                                                                               
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         OC    NEWLOWTB(L'PINVMINI),NEWLOWTB                                    
         BZ    LR300                                                            
         MVC   MINEKEY(L'PINVMINI),NEWLOWTB                                     
         B     LRMINHI2                                                         
*                                                                               
LRMIN01  CLI   ACTNUM,ACTCHECK                                                  
         BNE   LRMINHIA                                                         
*                                                                               
         LA    R2,CK1SEL0H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    LRMINLP                                                          
         LA    R2,CK2SEL0H                                                      
         B     LRMINLP                                                          
*                                                                               
LRMINHIA LA    R2,UP1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2SEL1H                                                      
*                                                                               
LRMINLP  CLI   PIMDTLEL,PIMDTLEQ   DETAIL ELEMENT STILL?                        
         BE    *+12                                                             
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         B     LR300                                                            
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   HEADER SEQUENCE MATCH?                       
         BE    *+12                                                             
         OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         B     LR300                                                            
*                                                                               
         ZAP   PUBCASHD,MYPUBCD    USE PUB CASH DISCOUNT AND AGENCY             
         ZAP   PUBAGYCM,MYPUBAC        COMMISSION PERCENTAGES                   
*                                                                               
         CLI   PIMBLINE,0          IF THERE IS A BUYLINE                        
         BE    LRMIN10                                                          
         BAS   RE,BUYXISTS         SEE IF BUYLINE REALLY EXISTS                 
         BNE   LRMIN10             IT DOESN'T, IT IS UNMATCHED NOW              
*                                                                               
         CLI   ACTNUM,ACTCHECK     CHECK ACTION?                                
         BNE   LRMIN05             NO, SHOW ALL INVOICE DETAILS                 
*                                                                               
         BAS   RE,MINIOSEQ         GET NEXT MINIO ELEMENT                       
         BE    LRMIN03A                                                         
*                                                                               
LRMIN03  OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         LH    R1,DLSTLENT         A(LAST LOWER ENTRY)                          
         SH    R1,=Y(L'PINVMINI)                                                
         STH   R1,DLSTLENT                                                      
         B     LR300                                                            
*                                                                               
LRMIN03A CLI   PIMDTLEL,PIMDTLEQ   A DETAIL ELEMENT?                            
         BNE   LRMIN03                                                          
         CLC   PIMDTLS1,LSTHDRSQ   OUR DETAIL ELEMENT?                          
         BNE   LRMIN03                                                          
*                                                                               
         TM    LBITFLAG,X'20'      WAS A DETAIL DISPLAYED ALREADY?              
         BZ    LRMINLP             NO, DON'T SAVE MINELEM KEY                   
         LA    R1,LOWERTBL         SAVE LAST MINELEM KEY READ                   
         AH    R1,DLSTLENT         A(LAST LOWER ENTRY)                          
         MVC   0(L'PINVMIEL,R1),PIMDTLEL                                        
         MVC   1(L'SEQUENCE,R1),PIMDTLS1                                        
         B     LRMINLP                                                          
*                                                                               
LRMIN05  CLI   SCRTYPE,C'N'        PROTECT THIS LINE WITH A BUYLINE             
         BNE   LRMIN05A                                                         
         USING SCRLIN1D,R2                                                      
         OI    SLN1SELH+6,X'28'                                                 
         OI    SLN1IDTH+6,X'28'                                                 
         OI    SLN1SIZH+6,X'28'                                                 
         OI    SLN1RTEH+6,X'28'                                                 
         OI    SLN1PRMH+6,X'28'                                                 
         OI    SLN1CTPH+6,X'28'                                                 
         OI    SLN1GRSH+6,X'28'                                                 
         OI    SLN1NETH+6,X'28'                                                 
         OI    SLN1ESTH+6,X'28'                                                 
         LA    R3,SLN1SELH                                                      
         B     LRMIN05B                                                         
         USING SCRLIN2D,R2                                                      
LRMIN05A OI    SLN2SELH+6,X'28'                                                 
         OI    SLN2IDTH+6,X'28'                                                 
         OI    SLN2SPCH+6,X'28'                                                 
         OI    SLN2CTPH+6,X'28'                                                 
         OI    SLN2GRSH+6,X'28'                                                 
         OI    SLN2NETH+6,X'28'                                                 
         OI    SLN2ESTH+6,X'28'                                                 
         LA    R3,SLN2SELH                                                      
         DROP  R2                                                               
*                                                                               
LRMIN05B MVI   8(R3),C'*'          ASSUME MATCHED FIRST                         
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BNZ   *+10                                                             
         MVC   8(L'SLN1SEL,R3),=CL2'->'     NO, CORRECTION                      
*                                                                               
LRMIN10  XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         ZAP   VALCLMS,=P'0'                                                    
         MVI   VALCOSTY,0          ZERO-OUT SO FIELDS WILL BE SHOWN             
         MVI   VALCOSIN,C' '                                                    
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
         LA    R1,LOWERTBL                                                      
         ST    R1,FULL                                                          
*                                                                               
         LR    R0,R1               SAVE A(CURRENT ENTRY IN LOWERTBL)            
         GOTO1 DISPDTL                                                          
         LR    R1,R0               RESTORE A(CURRENT ENTRY IN LOWERTBL)         
*                                                                               
         AH    R1,DLSTLENT         SAVE LAST MINELEM KEY USED                   
         MVC   0(L'PINVMIEL,R1),PIMDTLEL                                        
         MVC   1(L'SEQUENCE,R1),PIMDTLS1                                        
*                                                                               
         OI    LBITFLAG,X'20'      A DETAIL WAS DISPLAYED                       
*                                                                               
         LH    R1,DLSTLENT                                                      
         LA    R1,L'PINVMINI(R1)   SAVE A(NEXT ENTRY TO BE USED)                
         STH   R1,DLSTLENT                                                      
*                                                                               
LRMINNX  BAS   RE,MINIOSEQ                                                      
         BE    LR262                                                            
*                                                                               
LR260    OI    LBITFLAG,X'80'      NO, START FROM TOP                           
         LH    R1,DLSTLENT         A(LAST LOWER ENTRY)                          
         SH    R1,=Y(L'PINVMINI)                                                
         STH   R1,DLSTLENT                                                      
         B     LR300                                                            
*                                                                               
LR262    CLI   PIMDTLEL,PIMDTLEQ   A DETAIL ELEMENT?                            
         BNE   LR260                                                            
         CLC   PIMDTLS1,LSTHDRSQ   UNDER SAME HEADER SEQUENCE?                  
         BNE   LR260                                                            
         CLI   ACTNUM,ACTCHECK     IF ACTION NOT CHECK THEN WE DON'T            
         BNE   LR265                   CARE IF CORRECTED OR MATCHED             
         CLI   PIMBLINE,0          CORRECTED OR MATCHED?                        
         BNE   LRMINNX                                                          
*                                                                               
LR265    LA    R1,LOWERTBL         SAVE LAST MINELEM KEY READ                   
         AH    R1,DLSTLENT         A(LAST LOWER ENTRY)                          
         MVC   0(L'PINVMIEL,R1),PIMDTLEL                                        
         MVC   1(L'SEQUENCE,R1),PIMDTLS1                                        
*                                                                               
         IC    R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)      R2 = A(NEXT LINE)                            
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)      R2 = A(NEXT LINE)                            
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   LR265A                                                           
*                                                                               
         CLI   LINENUM,3           LEAVE BOTTOM LINE OPEN FOR ADDITIONS         
         BL    LRMINLP                                                          
         B     LR265B                                                           
*                                                                               
LR265A   CLI   LINENUM,11          LEAVE BOTTOM LINE OPEN FOR ADDITIONS         
         BL    LRMINLP                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
LR265B   LA    R3,SLN1IDT                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDT                                                       
*                                                                               
         MVC   0(L'SLN1IDT,R3),=C'**MORE**'  JUST SAY THAT THERE'S MORE         
*                                                                               
         LA    R1,LOWERTBL         SAVE LAST MINELEM KEY READ                   
         AH    R1,DLSTLENT           CAN USE THIS LAST LINE FOR ADDING          
         XC    0(L'PINVMINI,R1),0(R1)                                           
         LH    R1,DLSTLENT                                                      
         SH    R1,=Y(L'PINVMINI)                                                
         STH   R1,DLSTLENT                                                      
         EJECT                                                                  
*                                                                               
* CORRECTED INVOICE DETAILS                                                     
*                                                                               
LR300    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ    DEFAULT, START AT VERY BEGINNING             
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         ZAP   INVDTLAG,=P'0'                                                   
         ZAP   INVDTLAN,=P'0'                                                   
         ZAP   INVDTLGS,=P'0'                                                   
         MVI   NUMMTCHD,0                                                       
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   LRX                                                              
*                                                                               
LR300LP  L     R6,MINELEM          DETAIL ELEMENT STILL?                        
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   LRX                                                              
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   LRX                                                              
*                                                                               
         CLC   PIMIDATE,INVSTDT    INVOICE ITEM WITHIN INVOICE PERIOD?          
         BL    LR300NX             NO                                           
         CLC   PIMIDATE,INVENDDT                                                
         BH    LRX                 NO                                           
*                                                                               
LR310    GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
         L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         AP    INVDTLAG,DUB                                                     
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         AP    INVDTLAN,DUB                                                     
         L     R1,GSTTAX                                                        
         CVD   R1,DUB                                                           
         AP    INVDTLGS,DUB                                                     
*                                                                               
         CLI   ACTNUM,ACTSUPCK                                                  
         BE    LR320                                                            
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   LR300NX                                                          
*                                                                               
LR320    CLI   PIMBLINE,0                                                       
         BE    LR300NX                                                          
*                                                                               
         BAS   RE,BUYXISTS                                                      
         BNE   LR300NX                                                          
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BZ    LR330                                                            
         ZIC   R0,NUMMTCHD         YES, INCREMENT TOTAL NUMBER MATCHED          
         AH    R0,=H'1'                                                         
         STC   R0,NUMMTCHD                                                      
         B     LR300NX                                                          
*                                                                               
LR330    BAS   RE,DISPCORR         NO, DISPLAY THE CORRECTION(S) MADE           
*                                                                               
LR300NX  BAS   RE,MINIOSEQ                                                      
         BE    LR300LP                                                          
*                                                                               
LRX      ZAP   P11,INVDTLAG                                                     
         CLI   MYGRSNET,C'G'                                                    
         BE    *+10                                                             
         ZAP   P11,INVDTLAN                                                     
*                                                                               
         EDIT  (P11,P11),(13,CHKDGRS),2,ALIGN=LEFT,MINUS=YES                    
*                                                                               
         OI    CHKDGRSH+6,X'80'                                                 
         CP    INVHDRAM,P11                                                     
         BE    LRX10                                                            
         LA    R1,CHKDGRS                                                       
         LA    R1,L'CHKDGRS-1(R1)  GO TO LAST CHARACTER                         
*                                                                               
LRX0     CLI   0(R1),C' '                                                       
         BE    LRX1                                                             
         CLI   0(R1),0                                                          
         BE    LRX1                                                             
         LA    R1,1(R1)            PUT A * AFTER LAST DIGIT                     
         MVI   0(R1),C'*'                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIORD          SEE IF HEADER RELIES ON DETAILS $$           
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         TM    PIMSTAT,X'10'                                                    
         BZ    LRX10                                                            
         ZAP   PIMAMT,P11          YES, COPY TOTAL OF DETAILS TO HDR            
*                                                                               
         BAS   RE,MINIOWRT                                                      
*                                                                               
         B     LRX10                                                            
*                                                                               
LRX1     BCTR  R1,0                CHECK PREVIOUS CHARACTER                     
         B     LRX0                                                             
*                                                                               
LRX10    NI    BITFLAG,X'FF'-X'C0'    NOT FIRST TIME IN CHECK ANYMORE           
*                                                                               
         TM    MNIOFLAG,X'40'      PUT OUT 'NUMBER MATCHED: ' MESSAGE?          
         BZ    LRX20                                                            
         NI    MNIOFLAG,X'FF'-X'40'                                             
*                                                                               
         LA    R2,CK1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2SEL1H                                                      
*                                                                               
         TM    MNIOFLAG,X'20'      MATCHED SUCCESSFULLY?                        
         BNZ   *+12                                                             
         MVI   GERROR1,MTCHSUCC    YES                                          
         B     INFEXIT                                                          
*                                                                               
         MVI   GERROR1,NUMBMTCH                                                 
         XC    BLOCK(256),BLOCK                                                 
         MVI   BLOCK,10                                                         
         EDIT  (B1,NUMMTCHD),(3,BLOCK+1),ALIGN=LEFT,ZERO=NOBLANK                
         MVI   BLOCK+10,0                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,24           PRINT INVOICE MATCHING ERROR SYSTEM          
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         B     INFEXIT                                                          
         DROP  RF                                                               
*                                                                               
LRX20    CLI   PAYFLAG,C'Y'                                                     
         BNE   LRXIT                                                            
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,80,GLVPRRTN                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVPRRTN  DELETE RETURN VARIABLE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MARKPAID                                                      
*                                                                               
         MVI   GERROR1,BLNKMESS    YES, INVOICE HAS BEEN PAID                   
         MVC   BLOCK(256),BLOCK+1  COPY LENGTH & MESSAGE TO BEG(BLOCK)          
         ZIC   R1,BLOCK                                                         
         LA    R1,BLOCK(R1)                                                     
         MVI   0(R1),0             TERMINATING 0                                
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
         LA    R2,CHKOPTNH                                                      
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,24                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         B     INFEXIT                                                          
         DROP  RF                                                               
*                                                                               
LRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL CALCULATE THE TOTAL FROM THE BUYS                           
***********************************************************************         
CALCBUYS NTR1                                                                   
         ZAP   BUYAMNTG,=P'0'                                                   
         ZAP   BUYAMNTN,=P'0'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              NO, SET KEY TO SAME YEAR AND                 
         USING PBUYKEY,R3              MONTH AS THE INVOICE                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD   YES, NULL OUT THE PRODUCT                    
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'            PUB,ALL?                               
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON      YES, NULL OUT ZONE/EDITION             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE START DATE                       
         GOTO1 HIGH                                                             
         B     CALBLOOP                                                         
*                                                                               
CALBNEXT GOTO1 SEQ                 GET NEXT BUY                                 
*                                                                               
CALBLOOP LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE    SAME UPTO PRODUCT?              
         BNE   CALBX                            NO MORE BUYS                    
*                                                                               
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BE    *+14                YES, SKIP PRODUCT TEST                       
         CLC   PBUYKPRD,QPRD       NO, MAKE SURE PRODUCTS MATCH                 
         BNE   CALBX                   DON'T MATCH, NO MORE BUYS                
*                                                                               
         CLC   PBUYKPUB(4),BPUB    PUB NUMBER THE SAME?                         
         BE    CALB5               YES                                          
         BL    CALBNEXT            NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          VARIOUS PRODUCTS?                        
         BNE   CALBX                   NO, NO MORE BUYS                         
         MVI   PBUYKPUB,X'FF'          YES, FORCE TO NEXT PRODUCT               
         B     CALBHIGH                                                         
*                                                                               
CALB5    TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    CALB20              YES                                          
         CLC   PBUYKZON(2),BPUB+4  NO, MAKE SURE SAME ZONE/EDITION              
         BNE   CALBNEXT                NOT SAME, NO MORE BUYS                   
         B     CALB20                                                           
*                                                                               
CALBHIGH GOTO1 HIGH                                                             
         B     CALBLOOP                                                         
*                                                                               
CALB20   CLC   PBUYKDAT,INVSTDT    BUY'S DATE BELOW START DATE?                 
         BNL   CALB30                                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BNZ   *+14                     YES, READ FROM THE START DATE           
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   CALBX                        NO, NO MORE BUYS                    
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     CALBHIGH                                                         
*                                                                               
CALB30   CLC   PBUYKDAT,INVENDDT   BUY'S DATE AFTER INVOICE END DATE?           
         BNH   CALB35                                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'           YES, FORCE TO NEXT ZONE/EDITION         
         B     CALBHIGH                                                         
*                                                                               
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   CALBX                        NO, NO MORE BUYS                    
         MVI   PBUYKPUB,X'FF'               YES, FORCE TO NEXT PRODUCT          
         B     CALBHIGH                                                         
*                                                                               
CALB35   OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    CALB40              NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   CALBNEXT            NO, GET NEXT BUY RECORD                      
*                                                                               
CALB40   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   CALBNEXT            THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    CALBNEXT                                                         
*                                                                               
         MVI   ELCODE,X'80'        ANY SPECIAL REP?                             
         BAS   RE,GETEL                                                         
         BE    CALB50              YES                                          
         OC    SPCLREP,SPCLREP     NONE, SHOULD WE HAVE ONE?                    
         BZ    CALB60              NO, WE'RE NOT SUPPOSE TO HAVE ONE            
         B     CALBNEXT            YES WE ARE, GET NEXT BUY                     
*                                                                               
         USING PBSREPEL,R6                                                      
CALB50   CLC   PBSREP,SPCLREP      DOES SPECIAL REP MATCH INVHDR'S?             
         BNE   CALBNEXT            NO, GET NEXT BUY                             
*                                                                               
CALB60   L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT                         
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
         L     R1,GROSS            LOAD UP THE GROSS                            
         CVD   R1,DUB                                                           
         AP    BUYAMNTG,DUB                                                     
*                                                                               
         L     R0,AGYCOM           NO, SHOW NET (NET = GROSS - AGYCOM)          
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         AP    BUYAMNTN,DUB                                                     
         B     CALBNEXT                                                         
*                                                                               
CALBX    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS IF THE BUYLINE EXISTS, AND RETURNS A 'YES' IF IT          
* DOES.  IF IT DOESN'T EXIST MAKE THE INVOICE DETAIL UNMATCHED AND              
* RETURN A 'NO'                                                                 
***********************************************************************         
BUYXISTS NTR1                                                                   
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         XC    KEY,KEY             SET UP BASIC INFORMATION                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD       SET PRODUCT CODE                             
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+26                                                             
         MVC   PBUYKPRD,PIMSPRD    YES, GET PRODUCT FROM DETAIL                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT EXISTS                  
         BNH   *+10                                                             
         MVC   PBUYKPRD,PIMCPRD       USE IT                                    
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'                                                   
         BZ    *+10                                                             
         MVC   PBUYKZON(2),PIMBZONE                                             
*                                                                               
         MVC   PBUYKDAT(L'PIMBDATE+L'PIMBEST),PIMBDATE                          
         MVC   PBUYKLIN,PIMBLINE                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'  DON'T NEED DELETED RECS ANYMORE             
*                                                                               
BXIST10  CLC   KEY(PBUYLEN-PBUYKEY),KEYSAVE   BASIC INFO MATCHES?               
         BNE   BXIST20             NO                                           
         TM    PBUYLEN,X'80'       CONTROL BYTE FOR KEY                         
         BZ    BXISTYES            NO, BUY STILL EXISTS                         
*                                                                               
*                                  NO ASSOCIATED BUYLINES                       
BXIST20  XC    PIMBZONE(PIMSPACE-PIMBZONE),PIMBZONE                             
         NI    PIMDSTAT,X'FF'-X'10'  UNMATCHED NOW                              
         BAS   RE,MINIOWRT                                                      
*                                                                               
BXISTNO  B     NO                                                               
*                                                                               
BXISTYES DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    NO                                                               
         ZAP   PUBCASHD,PBDCD      USE BUY'S CASH DISCOUNT AND AGENCY           
         ZAP   PUBAGYCM,PBDACP         COMMISSION PERCENTAGES                   
         B     YES                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SET MINEKEY TO AT MOST THE 3RD LAST MINEKEY                 
***********************************************************************         
MINPF6   NTR1                                                                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         LA    R1,LOWERTBL         CHECK FROM LAST LOWER ENTRY                  
         AH    R1,DLSTLENT         CHECK FROM LAST LOWER ENTRY                  
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    PF6MX                                                            
*                                                                               
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
*                                                                               
         BAS   RE,MINIOHI                                                       
*                                                                               
PF6M10   L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   PF6MX                                                            
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   PF6MX                                                            
         MVC   MINEKEY+1(L'SEQUENCE),PIMDTLS1                                   
         BAS   RE,MINIOSEQ                                                      
         BE    PF6M10                                                           
*                                                                               
PF6MX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SET MINEKEY TO AT MOST THE 3RD PREVIOUS MINEKEY             
***********************************************************************         
MINPF7   NTR1                                                                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         LA    R1,LOWERTBL                                                      
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    PF7MX                                                            
*                                                                               
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
*                                                                               
         BAS   RE,MINIOHI                                                       
*                                                                               
         LA    R0,4                                                             
PF7M10   GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,MINEEOF                                                   
         BE    PF7MX                                                            
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   PF7MX                                                            
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   PF7MX                                                            
*                                                                               
         MVC   MINEKEY+1(L'SEQUENCE),PIMDTLS1                                   
         BCT   R0,PF7M10                                                        
*                                                                               
PF7MX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE CORRECTION(S) MADE TO A BUY                         
***********************************************************************         
DISPCORR NTR1                                                                   
         CLI   ACTNUM,ACTCHECK     ACTION CHECK?                                
         BNE   DC00                                                             
*                                                                               
         LA    R2,CK1SEL1H         YES                                          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2SEL1H                                                      
         B     DC00A                                                            
*                                                                               
DC00     LA    R2,SC1SEL1H         NO, SUPERCHECK ACTION                        
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2SEL1H                                                      
*                                                                               
DC00A    L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         LA    R1,UPPERTBL                                                      
         USING PBUYKEY,R1                                                       
         LA    R0,8                8 LINES MAX                                  
         CLI   ACTNUM,ACTCHECK     ACTION CHECK?                                
         BE    *+8                                                              
         LA    R0,12               NO, SUPERCK, 12 LINES MAX                    
*                                                                               
DCLOOP   OC    0(L'PBUYKEY,R1),0(R1)   IF NOTHING IN THIS ENTRY                 
         BZ    DCNEXT                  THEN NEXT                                
*                                                                               
*NOP*    CLI   PBUYKCLT,0          CLIENT STARTS WITH X'00'?                    
*NOP*    BE    DCNEXT              YES, THEN NOT A BUY KEY, MINEKEY             
         CLI   0(R1),X'40'         ENTRY START GREATER THAN BLANK?              
         BNH   DCNEXT              NO, THEN NOT A BUY KEY, MINEKEY              
*                                                                               
*                                  FOR ZONE, EDTN, DATE, & ESTIMATE?            
         CLC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON                             
         BNE   DCNEXT              NO                                           
*                                                                               
         CLC   PIMBLINE,PBUYKLIN   FOR THIS BUYLINE?                            
         BNE   DCNEXT              NO                                           
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BNE   *+14                                                             
         CLC   PIMSPRD,PBUYKPRD    FOR THIS PRODUCT ?                           
         BNE   DCNEXT              NO                                           
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1SELH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SELH                                                      
         DROP  R2                                                               
*                                                                               
         MVC   8(L'SLN1SEL,R3),=CL2'->'                                         
         OI    6(R3),X'08'         SHOW IN HIGH INTENSITY                       
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)      POINT TO NEXT LINE                           
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         ST    R1,FULL                                                          
         DROP  R1                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PBUYREC,R3                                                       
*                                                                               
         MVC   VALSPACE,PBDSPACE                                                
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   DC05                                                             
         CP    PBDSHOW,=P'0'                                                    
         BNE   DC05                                                             
         CP    PBDREG,=P'0'                                                     
         BNE   DC05                                                             
         CP    PBDILLUM,=P'0'                                                   
         BNE   DC05                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BE    DC04                                                             
*                                                                               
DC03     L     R6,MINELEM                                                       
         B     DC05                                                             
*                                                                               
         USING PCOMELEM,R6                                                      
DC04     ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'VALSPACE)                                                
         BNH   *+14                                                             
         MVC   VALSPACE,PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM                 
         B     DC03                                                             
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   DC03                                                             
         XC    VALSPACE,VALSPACE                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VALSPACE(0),PCOMELEM+2                                           
         B     DC03                                                             
*                                                                               
         USING PIMDTLEL,R6                                                      
DC05     MVC   VALUIND,PBDUIND                                                  
         ZAP   VALUNITS,PBDUNITS                                                
         ZAP   VALCLMS,PBDCLMS                                                  
         MVC   VALCOSTY,PBDCOSTY                                                
         MVC   VALCOSIN,PBDCOSIN                                                
         ZAP   VALCOST,PBDCOS                                                   
         MVC   VALCL,PBDCL                                                      
         ZAP   VALPRCOS,PBDPRCOS                                                
         ZAP   PUBCASHD,PBDCD      USE THE CASH DISCOUNT AND                    
         ZAP   PUBAGYCM,PBDACP         AGENCY COMMISSION FROM THE BUY           
         DROP  R3                                                               
*                                                                               
         L     R1,FULL                                                          
         USING PBUYKEY,R1                                                       
         LR    R3,R1               SAVE A(CURRENT ENTRY IN UPPERTBL)            
         GOTO1 DISPDTL                                                          
         LR    R1,R3               RESTORE A(CURRENT ENTRY IN UPPERTBL)         
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   DC10                                                             
         USING SCRLIN1D,R2                                                      
         NI    SLN1SELH+6,X'FF'-X'20'  DON'T PROTECT FOR NEXT INPUT             
         OI    SLN1IDTH+6,X'08'    SHOW IN HIGH INTENSITY                       
         OI    SLN1SIZH+6,X'08'                                                 
         OI    SLN1RTEH+6,X'08'                                                 
         OI    SLN1PRMH+6,X'08'                                                 
         OI    SLN1CTPH+6,X'08'                                                 
         OI    SLN1GRSH+6,X'08'                                                 
         OI    SLN1NETH+6,X'08'                                                 
         OI    SLN1ESTH+6,X'08'                                                 
         CLC   QPRD,=C'***'        IF NOT PRODUCT VARIOUS                       
         BE    *+8                                                              
         OI    SLN1ESTH+6,X'20'       PROTECT ESTIMATE                          
         B     DC20                                                             
*                                                                               
         USING SCRLIN2D,R2                                                      
DC10     NI    SLN2SELH+6,X'FF'-X'20'  DON'T PROTECT FOR NEXT INPUT             
         OI    SLN2IDTH+6,X'08'    SHOW IN HIGH INTENSITY                       
         OI    SLN2SPCH+6,X'08'                                                 
         OI    SLN2CTPH+6,X'08'                                                 
         OI    SLN2GRSH+6,X'08'                                                 
         OI    SLN2NETH+6,X'08'                                                 
         OI    SLN2ESTH+6,X'08'                                                 
*                                                                               
         CLC   QPRD,=C'***'        IF NOT PRODUCT VARIOUS                       
         BE    *+8                                                              
         OI    SLN2ESTH+6,X'20'    PROTECT ESTIMATE                             
*                                                                               
DC20     LA    R1,L'PBUYKEY(R1)                                                 
         MVC   0(L'PIMDTLEL,R1),PIMDTLEL                                        
         MVC   L'PIMDTLEL(L'SEQUENCE,R1),PIMDTLS1                               
         B     DCX                 FOUND IT AND DONE                            
*                                                                               
DCNEXT   CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)      POINT TO NEXT LINE                           
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         LA    R1,L'PBUYKEY(R1)                                                 
         BCT   R0,DCLOOP                                                        
*                                                                               
DCX      B     XIT                                                              
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MARKS PAID THOSE DETAILS WHOSE CORRESPONDING MATCHED             
* ARE PAID                                                                      
***********************************************************************         
MARKPAID NTR1                                                                   
         MVC   MELEM2,MELEM        SAVE INVOICE HEADER                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   MRKPX                                                            
*                                                                               
MRKPLOOP L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   MRKPX                                                            
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   MRKPX                                                            
*                                                                               
         TM    PIMDSTAT,X'02'      PAID ALREADY?                                
         BNZ   MRKPNXT                                                          
         TM    PIMDSTAT,X'10'      NO, IS DETAIL MATCHED?                       
         BZ    MRKPNXT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD       SET PRODUCT CODE                             
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+26                                                             
         MVC   PBUYKPRD,PIMSPRD    YES, GET PRODUCT FROM DETAIL                 
         CLC   PIMCPRD,=CL3' '     IF CORRECTED PRODUCT EXISTS                  
         BNH   *+10                                                             
         MVC   PBUYKPRD,PIMCPRD       USE IT                                    
*                                                                               
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKZON(2),PIMBZONE                                             
         MVC   PBUYKDAT,PIMBDATE                                                
         MVC   PBUYKEST,PIMBEST                                                 
         MVC   PBUYKLIN,PIMBLINE                                                
         DROP  R3,R6                                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    MRKPNXT                                                          
*                                                                               
         MVI   ELCODE,X'25'        SEE IF THIS INSERTION'S PAID                 
         BAS   RE,GETEL                                                         
MRKP00   BNE   MRKPNXT                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   MRKP10                                                           
         BAS   RE,NEXTEL                                                        
         B     MRKP00                                                           
****     BZ    MRKPNXT                                                          
         DROP  R6                                                               
*                                                                               
MRKP10   L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         OI    PIMDSTAT,X'02'                                                   
         BAS   RE,MINIOWRT                                                      
         DROP  R6                                                               
*                                                                               
MRKPNXT  BAS   RE,MINIOSEQ                                                      
         BE    MRKPLOOP                                                         
*                                                                               
MRKPX    MVC   MELEM,MELEM2                                                     
         B     XIT                                                              
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
         EJECT                                                                  
         DROP  R5                  DON'T NEED MINIO BLOCK FROM HERE ON          
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLOPT  MVI   GERROR1,BADOPTN                                                  
         B     ERREXIT                                                          
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         B     MYERRXIT                                                         
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE LISTS THE BUYS RECORDS ON THE SCREEN (THE UPPER LIST)            
***********************************************************************         
LISTBUYS DS    0H                                                               
         NMOD1 0,**LBUY**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
*  TWAXC LIST PORTION OF SCREEN AND ALSO VALIDATE FIELDS                        
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   LB10                                                             
         LA    R1,CK1SEL1H         CLEAR LIST PORTION OF SCREEN                 
         LA    R2,CK1SEL9H                                                      
         USING SCRLIN1D,R2                                                      
         LA    RF,SLN1ESTH                                                      
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    LB20                                                             
         LA    R1,CK2SEL1H                                                      
         LA    R2,CK2SEL9H                                                      
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
         B     LB20                                                             
*                                                                               
LB10     LA    R1,SC1SEL1H                                                      
         LA    R2,SC1LSTLH                                                      
         USING SCRLIN1D,R2                                                      
         LA    RF,SLN1ESTH                                                      
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    LB20                                                             
         LA    R1,SC2SEL1H                                                      
         LA    R2,SC2LSTLH                                                      
         USING SCRLIN2D,R2                                                      
         LA    RF,SLN2ESTH                                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
LB20     ZIC   RE,0(R1)            GET LENGTH                                   
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO                                           
         SH    RE,=H'8'            YES                                          
         LTR   RE,RE                                                            
         BM    LB30                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)       NULL OUT THE DATA                            
         OI    6(R1),X'80'         TRANSMIT FIELD                               
         OI    4(R1),X'20'         VALIDATE THE FIELD                           
         ZIC   RE,0(R1)                                                         
         BXLE  R1,RE,LB20                                                       
*                                                                               
LB30     XC    KEY,KEY             SET UP KEY TO READ BUYS                      
*                                                                               
         TM    BITFLAG,X'80'       FIRST TIME IN CHECK?                         
         BZ    LB40                                                             
         ZAP   BUYAMNTG,=P'0'      RESET TOTAL BUY AMOUNT                       
         ZAP   BUYAMNTN,=P'0'                                                   
         B     LB60                YES, START LIST FROM THE TOP                 
*                                                                               
LB40     TM    UBITFLAG,X'40'      REDISPLAY PAGE?                              
         BZ    LB50                                                             
LB45     OC    UPPERTBL(L'PBUYKEY),UPPERTBL                                     
         BZ    LB60                                                             
         MVC   KEY(L'PBUYKEY),UPPERTBL   YES, USE 1ST BUY KEY IN TABLE          
         B     LBSETUP                                                          
*                                                                               
LB50     CLI   WINDOW,C'L'         JUST LOWER WINDOW?                           
         BE    LB45                YES, REDISPLAY PAGE                          
*                                                                               
         CLI   PFKEY,5             START LIST FROM VERY BEGINNING?              
         BE    LB60                YES                                          
*                                                                               
         CLI   PFKEY,6             BOTTOM?                                      
         BNE   *+12                                                             
         BAS   RE,BUYPF6                                                        
         B     LBSETUP                                                          
*                                                                               
LB50A    CLI   PFKEY,7             PREVIOUS?                                    
         BNE   *+12                                                             
         BAS   RE,BUYPF7                                                        
         B     LBSETUP                                                          
*                                                                               
         CLI   PFKEY,0             ENTER?                                       
         BNE   LB45                NO, REDISPLAY PAGE FOR NOW                   
*                                                                               
         TM    UBITFLAG,X'80'      NO MORE FOR NEXT?                            
         BNZ   LB60                THEN DISPLAY FIRST PAGE AGAIN                
*                                                                               
         MVC   KEY(L'PBUYKEY),LSTBUYKY                                          
         B     LBSETUP                                                          
*                                                                               
LB60     LA    R3,KEY              NO, SET KEY TO SAME YEAR AND                 
         USING PBUYKEY,R3              MONTH AS THE INVOICE                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD                                                
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         DROP  R3                                                               
*                                                                               
LBSETUP  MVI   UBITFLAG,0          CLEAR ANY PREVIOUS FLAGS                     
         LA    R0,UPPERTBL                                                      
         LA    R1,L'UPPERTBL                                                    
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR KEY TABLE                              
         XC    DLSTUENT,DLSTUENT                                                
         MVI   LINENUM,0           NO LINES DISPLAYED YET                       
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
         LA    R2,CK1SEL1H         FIRST LINE ON SCREEN TO LIST ON              
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2SEL1H                                                      
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    LBHIGH                                                           
*                                                                               
         LA    R2,SC1SEL1H         FIRST LINE ON SCREEN TO LIST ON              
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2SEL1H                                                      
*                                                                               
LBHIGH   GOTO1 HIGH                                                             
*                                                                               
LBLOOP   LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME UPTO PRD?                   
         BNE   LBBEGNXT                 NO, START FROM BEG NEXT TIME            
*                                                                               
         CLC   =C'***',CHKPRD      YES, PRODUCT VARIOUS?                        
         BE    *+14                     YES, SKIP PRODUCT TEST                  
         CLC   PBUYKPRD,QPRD            NO, MAKE SURE PROD SAME THEN            
         BNE   LBBEGNXT                     START FROM BEG NEXT TIME            
*                                                                               
         CLC   PBUYKPUB(4),BPUB    ARE THE PUBS THE SAME?                       
         BE    LB62                YES                                          
         BL    LBNEXT              NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          PRODUCT VARIOUS?                         
         BNE   LBBEGNXT                NO, START FROM BEG NEXT TIME             
         MVI   PBUYKPUB,X'FF'          YES, GO TO NEXT PRODUCT                  
         B     LBHIGH                                                           
*                                                                               
LB62     TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    LB80                                                             
         CLC   PBUYKZON(2),BPUB+4                                               
         BNE   LBNEXT                                                           
         B     LB80                                                             
*                                                                               
LBBEGNXT OI    UBITFLAG,X'80'      NO, DISPLAY FROM BEGINNING NEXT TIME         
         LA    R1,UPPERTBL                                                      
         AH    R1,DLSTUENT                                                      
         XC    0(L'PBUYKEY,R1),0(R1)  AND CLEAR OUT CURR ENTRY IN TABLE         
         B     LBBX                DISPLAY LOWER LIST                           
*                                                                               
LB80     CLC   PBUYKDAT,INVSTDT    BUY'S DATE BELOW START DATE?                 
         BNL   LB85                                                             
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BNZ   *+14                     YES, READ FROM THE START DATE           
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS                    
         BNE   LBBEGNXT                                                         
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     LBHIGH                                                           
*                                                                               
LB85     CLC   PBUYKDAT,INVENDDT   BUY'S DATE AFTER INVOICE PERIOD?             
         BNH   LB88                                                             
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'           YES, FORCE TO NEXT ZONE/EDITION         
         B     LBHIGH                                                           
*                                                                               
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   LBBEGNXT                     NO, START FROM BEG NXT TIME         
         MVI   PBUYKPUB,X'FF'               YES, FORCE TO NEXT PRODUCT          
         B     LBHIGH                                                           
*                                                                               
LB88     OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    LB90                NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   LBNEXT              NO, GET NEXT BUY RECORD                      
*                                                                               
LB90     OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   LBNEXT              THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    LBNEXT                                                           
*                                                                               
         MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    LB100                                                            
         L     R6,AIO                                                           
         OC    SPCLREP,SPCLREP     DO WE NEED SPECIAL REP?                      
         BZ    LB110               NO                                           
         B     LBNEXT              YES, GET NEXT BUY                            
*                                                                               
         USING PBSREPEL,R6                                                      
LB100    CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BNE   LBNEXT                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
LB110    GOTO1 =A(SHOWBUY),DMCB,(RC),(R2),RR=RELO                               
*                                                                               
LBNXTL   IC    R1,LINENUM          INCREMENT NUMBER OF LINES USED               
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
*                                                                               
         LA    R1,UPPERTBL         SAVE LAST BUY KEY USED INTO TABLE            
         AH    R1,DLSTUENT                                                      
         MVC   0(L'PBUYKEY,R1),KEY                                              
         MVC   LSTBUYKY,KEY                                                     
         LH    R1,DLSTUENT                                                      
         AH    R1,=Y(L'PBUYKEY)    SAVE A(NEXT ENTRY TO BE USED)                
         STH   R1,DLSTUENT                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         TM    PBDSTAT,X'40'       IF MATCHED                                   
         BZ    LB130                                                            
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1SELH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SELH                                                      
         DROP  R2                                                               
*                                                                               
         XC    8(L'SLN1SEL,R3),8(R3)   THEN CLEAR THE ".."                      
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   8(2,R3),=C'PM'      BUYS MATCHED PRIOR TO NEW ELEM               
         B     LB115+4             DON'T GET PROTECTED                          
         USING PBINVELM,R6                                                      
         CLC   PBINVNUM,CHKINVN                                                 
         BE    LB115                                                            
         MVC   8(2,R3),=C'PM'        PREVIOUSLY MATCHED                         
         OI    6(R3),X'20'         PROTECT THE SELECT FIELD                     
         B     *+8                                                              
LB115    MVI   8(R3),C'*'               AND PUT OUT THE ASTERIX                 
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         OI    6(R3),X'08'                                                      
*                                                                               
         CLI   ACTNUM,ACTCHECK     CHECK ACTION?                                
         BE    *+16                                                             
         CLI   LINENUM,11          NO, SUPERCHECK HAS 12 LINES                  
         BH    LB145                                                            
         B     *+12                                                             
         CLI   LINENUM,7           YES, CHECK HAS 8 LINES                       
         BH    LB145                                                            
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   LB120                                                            
         USING SCRLIN1D,R2                                                      
         OI    SLN1IDTH+6,X'20'    PROTECT LINE AFTER MATCHED BUY               
         OI    SLN1SIZH+6,X'20'                                                 
         OI    SLN1RTEH+6,X'20'                                                 
         OI    SLN1PRMH+6,X'20'                                                 
         OI    SLN1CTPH+6,X'20'                                                 
         OI    SLN1GRSH+6,X'20'                                                 
         OI    SLN1NETH+6,X'20'                                                 
         OI    SLN1ESTH+6,X'20'                                                 
         B     LBNEXT                                                           
*                                                                               
         USING SCRLIN2D,R2                                                      
LB120    OI    SLN2IDTH+6,X'20'    PROTECT LINE AFTER MATCHED BUY               
         OI    SLN2SPCH+6,X'20'                                                 
         OI    SLN2CTPH+6,X'20'                                                 
         OI    SLN2GRSH+6,X'20'                                                 
         OI    SLN2NETH+6,X'20'                                                 
         OI    SLN2ESTH+6,X'20'                                                 
         B     LBNEXT                                                           
         DROP  R2                                                               
*                                                                               
LB130    CLI   ACTNUM,ACTCHECK     CHECK ACTION?                                
         BE    LB140                                                            
*                                                                               
         CLI   LINENUM,11          NO, NEED 2 LINES/BUY IF NOT MATCHED          
         BL    LB180                                                            
         B     LB145                                                            
*                                                                               
LB140    CLI   LINENUM,7           YES, NEED 2 LINES/BUY IF NOT MATCHED         
         BL    LB180                                                            
*                                                                               
         USING SCRLIN1D,R2                                                      
LB145    LA    R1,SLN1SELH                                                      
         LA    RF,SLN1ESTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+12                                                             
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2SELH                                                      
         LA    RF,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
         TWAXC (R1),(RF),PROT=Y    THEN ERASE THAT LINE AND                     
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   LB150                                                            
         USING SCRLIN1D,R2                                                      
         MVC   SLN1IDT,=C'**MORE**'    TELL USER WE HAVE MORE                   
         OI    SLN1SELH+6,X'20'    PROTECT "**MORE**" LINE                      
         CLI   ACTNUM,ACTCHECK     CHECK ACTION?                                
         BE    *+16                                                             
         CLI   LINENUM,12          NO, SUPERCHECK HAS 12 LINES                  
         BNL   LB160                                                            
         B     *+12                                                             
         CLI   LINENUM,8           YES, CHECK HAS 8 LINES                       
         BNL   LB160                                                            
         LA    R2,SLN1NXTL         PROTECT LINE AFTER "**MORE**" LINE           
         OI    SLN1SELH+6,X'20'                                                 
         OI    SLN1IDTH+6,X'20'                                                 
         OI    SLN1SIZH+6,X'20'                                                 
         OI    SLN1RTEH+6,X'20'                                                 
         OI    SLN1PRMH+6,X'20'                                                 
         OI    SLN1CTPH+6,X'20'                                                 
         OI    SLN1GRSH+6,X'20'                                                 
         OI    SLN1ESTH+6,X'20'                                                 
         B     LB160                                                            
*                                                                               
         USING SCRLIN2D,R2                                                      
LB150    MVC   SLN2IDT,=C'**MORE**'    TELL USER WE HAVE MORE                   
         OI    SLN2SELH+6,X'20'    PROTECT "**MORE**" LINE                      
         CLI   ACTNUM,ACTCHECK     CHECK ACTION?                                
         BE    *+16                                                             
         CLI   LINENUM,12          NO, SUPERCHECK HAS 12 LINES                  
         BNL   LB160                                                            
         B     *+12                                                             
         CLI   LINENUM,8           YES, CHECK HAS 8 LINES                       
         BNL   LB160                                                            
         LA    R2,SLN2NXTL         PROTECT LINE AFTER "**MORE**" LINE           
         OI    SLN2SELH+6,X'20'                                                 
         OI    SLN2IDTH+6,X'20'                                                 
         OI    SLN2SPCH+6,X'20'                                                 
         OI    SLN2CTPH+6,X'20'                                                 
         OI    SLN2GRSH+6,X'20'                                                 
         OI    SLN2ESTH+6,X'20'                                                 
         DROP  R2                                                               
*                                                                               
LB160    LA    R1,UPPERTBL                                                      
         AH    R1,DLSTUENT                                                      
         SH    R1,=Y(L'PBUYKEY)                                                 
         XC    0(L'PBUYKEY,R1),0(R1)                                            
LB170    SH    R1,=Y(L'PBUYKEY)                                                 
         CLI   L'PINVMINI(R1),0                                                 
         BE    LB170                                                            
         LA    R0,UPPERTBL                                                      
         SR    R1,R0                                                            
         STH   R1,DLSTUENT                                                      
*                                                                               
         TM    BITFLAG,X'80'       CALCULATE TOTAL BUYS?                        
         BZ    LBBX                NO, DID IT BEFORE                            
         GOTO1 =A(CALCMORE),DMCB,(RC),RR=RELO                                   
         B     LBBX                                                             
*                                                                               
LB180    CLI   SCRTYPE,C'N'        PROTECT CHANGE LINE'S SELECT FIELD           
         BNE   LB185                   AND EST FIELD IF WE GOT AN EST           
         USING SCRLIN1D,R2                                                      
         LA    R2,SLN1NXTL                                                      
         OI    SLN1SELH+6,X'20'                                                 
*****    OI    SLN1ESTH+6,X'20'                                                 
*****    CLC   =C'***',CHKPRD      IF VARIOUS PRODUCT                           
*****    BNE   *+8                                                              
*****    OI    SLN1CTPH+6,X'20'    THEN PROTECT THIS COLUMN TOO                 
         B     LB190                                                            
         USING SCRLIN2D,R2                                                      
LB185    LA    R2,SLN2NXTL                                                      
         OI    SLN2SELH+6,X'20'                                                 
*****    OI    SLN2ESTH+6,X'20'                                                 
*****    CLC   =C'***',CHKPRD      IF VARIOUS PRODUCT                           
*****    BNE   *+8                                                              
*****    OI    SLN2CTPH+6,X'20'    THEN PROTECT THIS COLUMN TOO                 
         DROP  R2                                                               
*                                                                               
LB190    LH    R1,DLSTUENT         SAVE LAST MINIO SEQ USED INTO TABLE          
         AH    R1,=Y(L'PBUYKEY)    SAVE A(NEXT ENTRY TO BE USED)                
         STH   R1,DLSTUENT                                                      
*                                                                               
         CLI   SCRTYPE,C'N'        POINT TO NEXT DISPLAY LINE                   
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         IC    R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
         DROP  R6                                                               
*                                                                               
LBNEXT   GOTO1 SEQ                                                              
*                                                                               
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE    SAME UPTO PRD?                  
         BNE   LBNEXTX                   NO, START FROM BEG NEXT TIME           
*                                                                               
         CLC   =C'***',CHKPRD      YES, PRODUCT VARIOUS?                        
         BE    *+14                     YES, SKIP PRODUCT TEST                  
         CLC   PBUYKPRD,QPRD            NO, MAKE SURE PROD SAME THEN            
         BNE   LBNEXTX                      START FROM BEG NEXT TIME            
*                                                                               
         CLC   PBUYKPUB(4),BPUB    ARE THE PUBS THE SAME?                       
         BE    LBNEXT1             YES                                          
         BL    LBNEXT              NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          VARIOUS PRODUCTS?                        
         BNE   LBNEXTX                 NO, START FROM BEG NEXT TIME             
         MVI   PBUYKPUB,X'FF'          YES, GO TO NEXT PRODUCT                  
         B     LBHIGH                                                           
*                                                                               
LBNEXT1  TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    LBNX20                                                           
         CLC   PBUYKZON(2),BPUB+4                                               
         BNE   LBNEXT                                                           
         B     LBNX20                                                           
*                                                                               
LBLSTKEY LA    R1,UPPERTBL                                                      
         AH    R1,DLSTUENT                                                      
         MVC   0(L'PBUYKEY,R1),KEY    SAVE LAST PRINT BUY KEY READ              
         MVC   LSTBUYKY,KEY                                                     
         B     LBHIGH                                                           
*                                                                               
LBNX20   CLC   PBUYKDAT,INVSTDT    BUY'S DATE BELOW START DATE?                 
         BNL   LBNX30                                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BNZ   *+14                     YES, READ FROM START DATE THEN          
         CLC   =C'***',CHKPRD           NO, PRODUCT VARIOUS?                    
         BNE   LBNEXTX                      NO                                  
         MVC   PBUYKDAT,INVSTDT             YES                                 
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     LBLSTKEY                                                         
*                                                                               
LBNX30   CLC   PBUYKDAT,INVENDDT   BUY'S DATE AFTER INVOICE PERIOD?             
         BNH   LB200                                                            
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'           YES, FORCE TO NEXT ZONE/EDITION         
         B     LBLSTKEY                                                         
*                                                                               
LBNX31   CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   LBNEXTX                      NO, START FROM BEG NXT TIME         
         MVI   PBUYKPUB,X'FF'               YES, FORCE TO NEXT PRODUCT          
         B     LBLSTKEY                                                         
*                                                                               
LB200    LA    R1,UPPERTBL                                                      
         AH    R1,DLSTUENT                                                      
         MVC   0(L'PBUYKEY,R1),KEY    SAVE LAST PRINT BUY KEY READ              
         MVC   LSTBUYKY,KEY                                                     
         B     LBLOOP                                                           
*                                                                               
LBNEXTX  OI    UBITFLAG,X'80'      NO, DISPLAY FROM TOP NEXT TIME               
         LH    R1,DLSTUENT                                                      
         SH    R1,=Y(2*L'PBUYKEY)                                               
         STH   R1,DLSTUENT                                                      
*                                                                               
LBBX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SET KEY TO AT MOST THE 3RD LAST KEY                         
***********************************************************************         
BUYPF6   NTR1                                                                   
         LA    R2,THREEKYS         POINT TO THE TABLE OF 3 KEYS                 
*                                                                               
         LA    R3,KEY              NO, SET KEY TO SAME YEAR AND                 
         USING PBUYKEY,R3              MONTH AS THE INVOICE                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD                                                
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'      PUB,ALL?                                     
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
*                                                                               
         MVC   THREEKYS,KEY                                                     
*                                                                               
PF6BHI   GOTO1 HIGH                                                             
*                                                                               
PF6BLP   LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME UPTO PRODUCT?               
         BNE   PF6BX               NO                                           
*                                                                               
         CLC   =C'***',CHKPRD      YES, VARIOUS PRODUCTS?                       
         BE    *+14                                                             
         CLC   PBUYKPRD,QPRD            NO, MAKE SURE PROD SAME THEN            
         BNE   PF6BX                                                            
*                                                                               
         CLC   PBUYKPUB(4),BPUB    ARE THE PUBS THE SAME?                       
         BE    PF6B00              YES                                          
         BL    PF6BNX              NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          VARIOUS PRODUCTS?                        
         BNE   PF6BX                   NO                                       
         MVI   PBUYKPUB,X'FF'          YES, GO TO NEXT PRODUCT                  
         B     PF6BHI                                                           
*                                                                               
PF6B00   TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    PF6B05                                                           
         CLC   PBUYKZON(2),BPUB+4                                               
         BNE   PF6BNX                                                           
*                                                                               
PF6B05   CLC   PBUYKDAT,INVSTDT    WITHIN INVOICE PERIOD?                       
         BNL   PF6B10              NO                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BNZ   *+14                     YES, READ FROM THE START DATE           
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS                    
         BNE   PF6BX                                                            
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     PF6BHI                                                           
*                                                                               
PF6B10   CLC   PBUYKDAT,INVENDDT   WITHIN INVOICE PERIOD?                       
         BNH   PF6B20              NO                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'           YES, FORCE TO NEXT ZONE/EDITION         
         B     PF6BHI                                                           
*                                                                               
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   PF6BX                        NO, START FROM BEG NXT TIME         
         MVI   PBUYKPUB,X'FF'               YES, FORCE TO NEXT PRODUCT          
         B     PF6BHI                                                           
*                                                                               
PF6B20   OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    PF6B30              NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   PF6BNX              NO, GET NEXT BUY RECORD                      
*                                                                               
PF6B30   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   PF6BNX              THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PF6BNX                                                           
*                                                                               
         MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    PF6B40                                                           
         L     R6,AIO                                                           
         OC    SPCLREP,SPCLREP     DO WE NEED SPECIAL REP?                      
         BZ    PF6B50              NO                                           
         B     PF6BNX              YES, GET NEXT BUY                            
*                                                                               
         USING PBSREPEL,R6                                                      
PF6B40   CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BNE   PF6BNX                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
PF6B50   LA    R0,SIXKEYS          SHOULDN'T GO BEYOND TABLE OF 3 KEYS          
         CR    R2,R0                                                            
         BL    PF6B90                                                           
         MVC   THREEKYS(2*L'PBUYKEY),THREEKYS+L'PBUYKEY    KEEP LAST 3          
         MVC   THREEKYS+2*L'PBUYKEY,KEY                                         
         B     PF6BNX                                                           
*                                                                               
PF6B90   MVC   0(L'PBUYKEY,R2),KEY                                              
         LA    R2,L'PBUYKEY(R2)                                                 
*                                                                               
PF6BNX   GOTO1 SEQ                                                              
         B     PF6BLP              STORE 3 KEYS INTO TABLE                      
*                                                                               
PF6BX    XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),THREEKYS                                          
*                                                                               
PFBX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SET KEY TO AT MOST THE 3RD PREVIOUS KEY                     
***********************************************************************         
BUYPF7   NTR1                                                                   
         LA    R2,THREEKYS         POINT TO THE TABLE OF 3 KEYS                 
*                                                                               
         LA    R3,KEY              NO, SET KEY TO SAME YEAR AND                 
         USING PBUYKEY,R3              MONTH AS THE INVOICE                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
*                                                                               
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD      VARIOUS PRODUCTS?                            
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD                                                
*                                                                               
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         TM    GLOBFLG1,X'80'      PUB,ALL?                                     
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
*                                                                               
         MVC   PREVKEY,UPPERTBL    SAVE KEY TO GO PREVIOUS FROM                 
         MVC   THREEKYS,KEY        START HERE                                   
*                                                                               
         OC    PREVKEY,PREVKEY     IF NO KEY TO GO PREVIOUS FROM                
         BZ    PF7BX               THEN EXIT                                    
*                                                                               
PF7BHI   GOTO1 HIGH                                                             
*                                                                               
PF7BLP   LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME UPTO PRODUCT?               
         BNE   PF7BX               NO                                           
*                                                                               
         CLC   =C'***',CHKPRD      YES, VARIOUS PRODUCTS?                       
         BE    *+14                                                             
         CLC   PBUYKPRD,QPRD            NO, MAKE SURE PROD SAME THEN            
         BNE   PF7BX                                                            
*                                                                               
         CLC   PBUYKPUB(4),BPUB    ARE THE PUBS THE SAME?                       
         BE    PF7B00              YES                                          
         BL    PF7BNX              NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          VARIOUS PRODUCTS?                        
         BNE   PF7BX                   NO                                       
         MVI   PBUYKPUB,X'FF'          YES, GO TO NEXT PRODUCT                  
         B     PF7BHI                                                           
*                                                                               
PF7B00   TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    PF7B05                                                           
         CLC   PBUYKZON(2),BPUB+4                                               
         BNE   PF7BNX                                                           
*                                                                               
PF7B05   CLC   PBUYKDAT,INVSTDT    WITHIN INVOICE PERIOD?                       
         BNL   PF7B10              NO                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BNZ   *+14                     YES, READ FROM THE START DATE           
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS                    
         BNE   PF7BX                                                            
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     PF7BHI                                                           
*                                                                               
PF7B10   CLC   PBUYKDAT,INVENDDT   WITHIN INVOICE PERIOD?                       
         BNH   PF7B20              NO                                           
*                                                                               
         TM    GLOBFLG1,X'80'      YES, PUB,ALL?                                
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'           YES, FORCE TO NEXT ZONE/EDITION         
         B     PF7BHI                                                           
*                                                                               
         CLC   =C'***',CHKPRD           NO, VARIOUS PRODUCTS?                   
         BNE   PF7BX                        NO, START FROM BEG NXT TIME         
         MVI   PBUYKPUB,X'FF'               YES, FORCE TO NEXT PRODUCT          
         B     PF7BHI                                                           
*                                                                               
PF7B20   OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    PF7B30              NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   PF7BNX              NO, GET NEXT BUY RECORD                      
*                                                                               
PF7B30   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   PF7BNX              THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         CLC   PREVKEY,KEY         IF WE ENCOUNTER KEY TO GO PREV FROM          
         BE    PF7BX               THEN WE GOT LAST 3 KEYS FROM IT              
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PF7BNX                                                           
*                                                                               
         MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    PF7B40                                                           
         L     R6,AIO                                                           
         OC    SPCLREP,SPCLREP     DO WE NEED SPECIAL REP?                      
         BZ    PF7B50              NO                                           
         B     PF7BNX              YES, GET NEXT BUY                            
*                                                                               
         USING PBSREPEL,R6                                                      
PF7B40   CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BNE   PF7BNX                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
PF7B50   LA    R0,SIXKEYS          SHOULDN'T GO BEYOND TABLE OF 3 KEYS          
         CR    R2,R0                                                            
         BL    PF7B90                                                           
         MVC   THREEKYS(2*L'PBUYKEY),THREEKYS+L'PBUYKEY    KEEP LAST 3          
         MVC   THREEKYS+2*L'PBUYKEY,KEY                                         
         B     PF7BNX                                                           
*                                                                               
PF7B90   MVC   0(L'PBUYKEY,R2),KEY                                              
         LA    R2,L'PBUYKEY(R2)                                                 
*                                                                               
PF7BNX   GOTO1 SEQ                                                              
         B     PF7BLP              STORE 3 KEYS INTO TABLE                      
*                                                                               
PF7BX    XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),THREEKYS                                          
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SHOWS THE BUY.                                                   
***********************************************************************         
SHOWBUY  DS    0H                                                               
         NMOD1 0,**SBUY**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1SEL                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SEL                                                       
         DROP  R2                                                               
         MVC   0(L'SLN1SEL,R3),=CL2'..'   NOTHING FOR INSERTION                 
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1IDT                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDT                                                       
         DROP  R2                                                               
*                                                                               
         ST    R3,DMCB+4           DEFAULT IS TO PUT OUT FULL DATE              
         MVI   DMCB+4,12                                                        
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+8                                                              
         MVI   DMCB+4,9            JUST PUT OUT MONTH/YEAR                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT)     SHOW THE BUY DATE OR MONTH          
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         XC    3(3,R3),3(R3)       DON'T WANT THE YEAR                          
         SH    R3,=H'2'      BACK-UP SO LINE NUMBER WILL FOLLOW MTH             
*                                                                               
         CLI   PBUYKLIN,1          IF NOT BUYLINE 1                             
         BE    SBUY10                                                           
         MVI   5(R3),C'-'          THEN SHOW ME THE BUYLINE                     
*NOP*    GOTO1 HEXOUT,DMCB,PBUYKLIN,6(R3),L'PBUYKLIN                            
*                                                                               
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    SBUY04                                                           
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  7(1,R3),DUB+7(1)                                                 
         OI    7(R3),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   6(1,R3),0(RF)                                                    
         B     SBUY10              DONE                                         
*                                                                               
SBUY04   OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R3),DUB                                                      
         CLI   6(R3),C'0'                                                       
         BNE   SBUY10                                                           
         MVC   6(2,R3),7(R3)                                                    
*                                                                               
         USING SCRLIN1D,R2                                                      
SBUY10   OC    MYBEST,MYBEST                                                    
         BNZ   SBUY20                                                           
*                                                                               
         LA    R3,SLN1EST                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2EST                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B2,PBUYKEST),(3,0(R3)),FILL=0   PUT OUT ESTIMATE                
*                                                                               
SBUY20   MVI   ELCODE,X'20'        GET BUY DESCRIPTION ELEMENT                  
         BAS   RE,GETEL                                                         
         USING PBDELEM,R6                                                       
*                                                                               
         CLI   SCRTYPE,C'N'        NEWSPAPER?                                   
         BNE   SBUY200                                                          
         USING SCRLIN1D,R2                                                      
*                                                                               
         LA    R3,SLN1SIZ                                                       
*                                                                               
         CLI   PBDSPACE,C' '       IF 1ST BYTE > SPACE?                         
         BNH   SBUY40                                                           
*        CLI   PBDSPACE,X'FF'      3 BYTES PACKED FOLLOWS?                      
*        BNE   SBUY30                                                           
*        EDIT  (P3,PBDSHOW),(8,0(R3)),ALIGN=LEFT   YES                          
*        B     SBUY80                                                           
*                                                                               
SBUY30   MVC   0(8,R3),PBDSPACE                                                 
         B     SBUY80                                                           
*                                                                               
SBUY40   CLI   PBDUIND,0           PRINT SIZE WITH NO DECIMALS?                 
         BE    SBUY50                                                           
         CLI   PBDUIND,C'L'                                                     
         BE    SBUY50                                                           
         CLI   PBDUIND,C'I'                                                     
         BNE   SBUY70                                                           
*                                                                               
SBUY50   EDIT  (P3,PBDUNITS),(8,0(R3)),ALIGN=LEFT     YES                       
*                                                                               
         SR    R1,R1               THEN PUT AN 'I' AFTER THE NUMBER             
SBUY50LP LA    R3,SLN1SIZ                                                       
         AR    R3,R1                                                            
         CLI   0(R3),C' '                                                       
         BE    SBUY60                                                           
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'SLN1SIZ)                                                 
         BL    SBUY50LP                                                         
         B     SBUY75                                                           
*                                                                               
SBUY60   CLI   PBDUIND,C'I'        IF INCHES                                    
         BE    *+12                                                             
         CLI   PBDUIND,X'89'                                                    
         BNE   SBUY75                                                           
*                                                                               
         MVI   0(R3),C'I'                                                       
         LA    R3,1(R3)                                                         
         B     SBUY75                                                           
*                                                                               
SBUY70   CLI   PBDUIND,X'89'       INCHES WITH 2 DECIMALS?                      
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P3,PBDUNITS),(8,SLN1SIZ),2,ALIGN=LEFT    YES                    
         SR    R1,R1                                                            
         B     SBUY50LP            GO BACK AND PUT AN 'I' AFTER NUMBER          
*                                                                               
SBUY75   OC    PBDCLMS,PBDCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PBDCLMS,=P'0'                                                    
         ZAP   DUB,PBDCLMS                                                      
         BZ    SBUY80                                                           
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         MVC   WORK+32(L'SLN1SIZ),SLN1SIZ                                       
         LA    R0,SLN1SIZ                                                       
         LR    R1,R3                                                            
         SR    R1,R0                                                            
         LA    R3,WORK+32(R1)                                                   
         EDIT  (P8,DUB),(3,0(R3)),ALIGN=LEFT                                    
         MVC   SLN1SIZ,WORK+32                                                  
*                                                                               
SBUY80   ZAP   P11,PBDCOS                                                       
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BE    SBUY80A                                                          
*                                                                               
         CLI   MYGRSNET,C'N'                                                    
         BNE   SBUY80A                                                          
         ZAP   GROSSAMT,P11                                                     
         ZAP   PERCENTG,PBDACP                                                  
         BAS   RE,GRSTONET                                                      
         ZAP   P11,NETAMNT                                                      
*                                                                               
SBUY80A  CLI   PBDCOSTY,C'U'       UNIT COST?                                   
         BNE   SBUY95                                                           
*                                                                               
         LA    R1,SLN1RTE                                                       
*                                                                               
         CP    PBDCOS,=P'0'        FREE?                                        
         BNE   *+14                                                             
         MVC   SLN1RTE(4),=C'FREE'  YES                                         
         B     SBUY100                                                          
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BNE   *+12                                                             
         MVI   SLN1RTE,C'S'                                                     
         LA    R1,1(R1)                                                         
*                                                                               
         CP    P11,=P'-99999999'  GREATER THAN 999.99999?                       
         BL    *+14                                                             
         CP    P11,=P'99999999'                                                 
         BNH   SBUY90              NO, CAN PUT OUT 5 DECIMALS                   
         ZAP   GROSSAMT,P11                                                     
         SRP   GROSSAMT,64-3,0     DIVIDE BY 1000 BUT DON'T ROUND               
         ZAP   P11,GROSSAMT                                                     
*                                                                               
         CLI   PBDCOSIN,C'S'                                                    
         BE    SBUY80B                                                          
*                                                                               
         EDIT  (P11,P11),(10,(R1)),2,ALIGN=LEFT,MINUS=YES    UNIT RATE          
         B     SBUY100                                        2 DEC             
*                                                                               
SBUY80B  EDIT  (P11,P11),(9,(R1)),2,ALIGN=LEFT,MINUS=YES                        
         B     SBUY100                                                          
*                                                                               
SBUY90   CLI   PBDCOSIN,C'S'                                                    
         BE    SBUY90A                                                          
*                                                                               
         EDIT  (P11,P11),(10,(R1)),5,ALIGN=LEFT,MINUS=YES  UNIT RATE            
         B     SBUY100                                         5 DEC            
*                                                                               
SBUY90A  SRP   P11,64-1,5          DIVIDE BY 10 AND ROUND                       
         EDIT  (P11,P11),(9,(R1)),4,ALIGN=LEFT,MINUS=YES                        
         B     SBUY100                                                          
*                                                                               
SBUY95   LA    R1,SLN1RTE                                                       
         CLI   PBDCOSIN,C'S'                                                    
         BNE   SBUY97                                                           
         MVI   0(R1),C'S'                                                       
         LA    R1,1(R1)                                                         
         B     SBUY97A                                                          
*                                                                               
SBUY97   MVI   SLN1RTE,C'T'        SIGNIFY TOTAL RATE                           
         LA    R1,1(R1)                                                         
*                                                                               
SBUY97A  EDIT  (P11,P11),(9,(R1)),2,ALIGN=LEFT,MINUS=YES                        
*                                                                               
SBUY100  CLI   PBDCL,0             NO COLORS?                                   
         BE    SBUY100A                                                         
*                                                                               
         EDIT  (B1,PBDCL),(1,SLN1PRM)                                           
         MVC   SLN1PRM+1(2),=C'C/'                                              
         LA    R1,SLN1PRM+3                                                     
         EDIT  (P5,PBDPRCOS),(8,(R1)),2,ALIGN=LEFT,MINUS=YES                    
         B     SBUY110                                                          
*                                                                               
SBUY100A CP    PBDPRCOS,=P'0'                                                   
         BNE   *+14                                                             
         XC    SLN1PRM,SLN1PRM                                                  
         B     SBUY110                                                          
*                                                                               
         EDIT  (P5,PBDPRCOS),(11,SLN1PRM),2,ALIGN=LEFT,MINUS=YES                
*                                                                               
SBUY110  B     SBUY205                                                          
*                                                                               
SBUY200  DS    0H                  SOLELY FOR MAGAZINE TYPES                    
         USING PBDELEM,R6                                                       
*                                                                               
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SPC                                                       
*                                                                               
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   SBUY204             NO                                           
*                                                                               
         CLI   PBDSPACE,X'FF'      3 BYTES PACKED FOLLOWS?                      
         BNE   SBUY204                                                          
*                                                                               
         CP    PBDSHOW,=P'0'       NO VALUES IN SHOW/REG/ILLUM?                 
         BNE   SBUY201                                                          
         CP    PBDREG,=P'0'                                                     
         BNE   SBUY201                                                          
         CP    PBDILLUM,=P'0'                                                   
         BE    SBUY202             YES, GET COMMENT INSTEAD                     
*                                                                               
SBUY201  XC    SLN2SPC,SLN2SPC                                                  
         MVC   0(4,R3),=C'SRI='  DISPLAY SHOW/REG/ILLUM                         
         LA    R3,4(R3)                                                         
*                                                                               
         CP    PBDSHOW,=P'99999'                                                
         BNE   SBUY201A                                                         
         MVC   0(3,R3),=C'SPC'                                                  
         LA    R3,3(R3)                                                         
         B     SBUY201B                                                         
*                                                                               
SBUY201A EDIT  (P3,PBDSHOW),(3,0(R3)),ALIGN=LEFT                                
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
SBUY201B MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (P3,PBDREG),(4,0(R3)),ALIGN=LEFT                                 
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (P3,PBDILLUM),(4,0(R3)),ALIGN=LEFT                               
         B     SBUY205                                                          
*                                                                               
SBUY202  L     R6,AIO                                                           
         MVI   ELCODE,X'66'        GET FIRST COMMENT                            
         BAS   RE,GETEL                                                         
         BE    SBUY203                                                          
*                                                                               
SBUY202A L     R6,AIO                                                           
         MVI   ELCODE,X'20'        RESTORE WHERE WE WERE BEFORE COMMENT         
         BAS   RE,GETEL                                                         
         B     SBUY205                                                          
*                                                                               
         USING PCOMELEM,R6                                                      
SBUY203  ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'SLN2SPC)                                                 
         BNH   *+14                                                             
         MVC   0(L'SLN2SPC,R3),PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM          
         B     SBUY202A                                                         
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   SBUY202A                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PCOMELEM+2                                               
         B     SBUY202A                                                         
*                                                                               
         USING PBDELEM,R6                                                       
SBUY204  MVC   0(L'SLN2SPC,R3),PBDSPACE                                         
*                                                                               
         USING SCRLIN1D,R2                                                      
SBUY205  LA    R3,SLN1CTPH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2CTPH                                                      
         DROP  R2                                                               
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BNE   SBUY209                                                          
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         MVC   8(3,R3),PBUYKPRD                                                 
         NI    1(R3),X'FF'-X'20'                                                
         MVI   ELCODE,X'20'        RESTORE R6 TO PREVIOUS POSITION              
         BAS   RE,GETEL                                                         
         USING PBDELEM,R6                                                       
*                                                                               
         CLC   CHKOPTN(2),=C'CT'    CTPBI WITH PROD ***                         
         BNE   SBUY220                                                          
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NETH                                                      
         DROP  R2                                                               
*                                                                               
SBUY209  MVI   8(R3),C'N'                                                       
         CP    PBDCD,=P'0'                                                      
         BE    *+8                                                              
         MVI   8(R3),C'Y'                                                       
*                                                                               
         TM    PBDSTAT,X'10'                                                    
         BE    *+8                                                              
         MVI   9(R3),C'T'                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        LOOK FOR A PAID ITEM                         
         BAS   RE,GETEL                                                         
SBUY209A BNE   SBUY210                                                          
         USING PPAYELEM,R6                                                      
*                                                                               
         OC    PPDDATE,PPDDATE                                                  
         BNZ   *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     SBUY209A                                                         
***      BZ    SBUY210                                                          
         MVI   10(R3),C'P'                                                      
*                                                                               
SBUY210  L     R6,AIO                                                           
         MVI   ELCODE,X'26'        LOOK FOR A BILL ITEM                         
         BAS   RE,GETEL                                                         
         BNE   SBUY210A                                                         
         USING PBILELEM,R6                                                      
*                                                                               
         OC    PBLDATE,PBLDATE                                                  
         BZ    SBUY210A                                                         
         MVI   11(R3),C'B'                                                      
*                                                                               
SBUY210A L     R6,AIO                                                           
         MVI   ELCODE,X'70'        LOOK FOR THE INSERTION ORDER                 
         BAS   RE,GETEL                                                         
         BNE   SBUY220             NONE                                         
         USING PIOELEM,R6                                                       
*                                                                               
         OC    PIOIDATE,PIOIDATE                                                
         BZ    SBUY220                                                          
         MVI   12(R3),C'I'                                                      
*                                                                               
SBUY220  DS    0H                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT,=C'GST'                 
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         L     R1,DMCB+16                                                       
         MVC   GVALUES(GSTTAXBL+L'GSTTAXBL-GVALUES),0(R1)                       
*                                                                               
         CLI   SCRTYPE,C'M'                                                     
         BNE   SBUY225                                                          
         CP    PBDCOS,=P'0'                                                     
         BNE   SBUY225                                                          
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2GRS                                                       
         MVC   0(4,R1),=C'FREE'                                                 
         B     SBUY230                                                          
*                                                                               
SBUY225  L     R1,GROSS            LOAD UP THE GROSS                            
         SR    R0,R0                                                            
         CLI   MYGRSNET,C'G'       INVOICE HEADER HAS GROSS AMOUNT?             
         BE    *+8                 YES                                          
         L     R0,AGYCOM           NO, SHOW NET (NET = GROSS - AGYCOM)          
         SR    R1,R0                                                            
         ST    R1,FULL             STORE GROSS OR NET IN FULL                   
*                                                                               
         USING SCRLIN1D,R2         SHOW ORDERED AMOUNT                          
         LA    R1,SLN1GRS                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2GRS                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,0(R1)),2,ALIGN=LEFT,MINUS=YES                      
*                                                                               
         TM    BITFLAG,X'80'       FIRST TIME IN?                               
         BZ    SBUY230                                                          
         L     R1,GROSS            YES, TOTAL THE BUYS                          
         CVD   R1,DUB                                                           
         AP    BUYAMNTG,DUB                                                     
         L     R0,AGYCOM           NO, SHOW NET (NET = GROSS - AGYCOM)          
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         AP    BUYAMNTN,DUB                                                     
*                                                                               
SBUY230  CLI   CHKOPTN+1,C'.'      ONE BYTE OPTION?                             
         BE    *+12                                                             
         CLI   CHKOPTN+1,0                                                      
         BNE   SBUY235                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
SBUY230A CLI   CHKOPTN,C'A'        AD CODE?                                     
         BNE   SBUY230B                                                         
*                                                                               
         MVC   0(L'PBDJOB,R3),PBDJOB  AD CODE IS JOB NUMBER (GRANT)             
         B     SBUY240                                                          
*                                                                               
SBUY230B CLI   CHKOPTN,C'B'        BILLABLE DATE                                
         BNE   SBUY230L                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(11,0(R3))                              
         B     SBUY240                                                          
*                                                                               
SBUY230L CLI   CHKOPTN,C'L'        LAST INSERTION ORDER                         
         BNE   SBUY230P                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'        LOOK FOR THE INSERTION ORDER                 
         BAS   RE,GETEL                                                         
         BNE   SBUY231L            NONE                                         
         USING PIOELEM,R6                                                       
*                                                                               
         OC    PIOIDATE,PIOIDATE                                                
         BNZ   *+14                                                             
SBUY231L MVC   0(4,R3),=C'NONE'                                                 
         B     SBUY240                                                          
         GOTO1 DATCON,DMCB,(3,PIOIDATE),(11,0(R3))                              
         B     SBUY240                                                          
*                                                                               
         USING PBUYREC,R6                                                       
SBUY230P CLI   CHKOPTN,C'P'        PAYABLE DATE?                                
         BNE   SBUY230Z                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(11,0(R3))                              
         B     SBUY240                                                          
*                                                                               
SBUY230Z CLI   CHKOPTN,C'Z'        ZONE AND EDITION?                            
         BNE   SBUY230T                                                         
*                                                                               
         OC    BPUB+4(2),BPUB+4    ANY ZONE AND EDITION REQUESTED?              
         BZ    SBUY240             YES, SAME, NOTHING TO DISPLAY HERE           
*                                                                               
         OC    PBUYKZON(L'PBUYKZON+L'PBUYKEDT),PBUYKZON                         
         BZ    SBUY240             NO ZONE OR EDITION                           
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VPUBEDIT,DMCB,(8,PBUYKPUB),(C'S',WORK)                           
*                                                                               
         MVC   0(6,R3),WORK+9      SHOW THE ZONE AND EDITION                    
         B     SBUY240                                                          
*                                                                               
SBUY230T LA    R3,GETINSA                                                       
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R1,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
         CLI   CHKOPTN,C'T'        TAX DOLLARS?                                 
         BNE   SBUY230C                                                         
*                                                                               
         EDIT  (B4,TAX),(11,0(R1)),2,ALIGN=LEFT,MINUS=YES                       
         B     SBUY240                                                          
*                                                                               
SBUY230C CLI   CHKOPTN,C'C'        CASH DISCOUNT?                               
         BNE   SBUY230G                                                         
*                                                                               
         OC    CSHDSC,CSHDSC                                                    
         BNZ   *+14                                                             
         MVC   0(4,R1),=C'NONE'                                                 
         B     SBUY240                                                          
         MVC   FULL,CSHDSC                                                      
         B     SBUYSHW0                                                         
*                                                                               
SBUY230G CLI   CHKOPTN,C'G'        GROSS?                                       
         BNE   SBUY230N                                                         
*                                                                               
         MVC   FULL,GROSS                                                       
         B     SBUYSHW0                                                         
*                                                                               
SBUY230N CLI   CHKOPTN,C'N'        NET?                                         
         BNE   SBUYBOPT                                                         
                                                                                
         L     R1,GROSS            YES                                          
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
SBUYSHW0 LA    R1,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,0(R1)),2,ALIGN=LEFT,MINUS=YES                      
         B     SBUY240                                                          
*                                                                               
SBUYBOPT LA    R2,CHKOPTNH                                                      
         B     INVLOPT                                                          
*                                                                               
SBUY235  DS    0H                                                               
*                                                                               
SBUY235G CLC   =C'GL',CHKOPTN      GROSS LESS CASH DISCOUNT?                    
         BNE   SBUY235N                                                         
*                                                                               
         L     R1,GROSS            R1 = GROSS                                   
         L     R0,CSHDSC           R1 = GROSS - CASH DISCOUNT                   
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         B     SBUYSHW5                                                         
*                                                                               
SBUY235N CLC   =C'NL',CHKOPTN      NET LESS CASH DISCOUNT?                      
         BNE   SBUY235S                                                         
*                                                                               
         L     R1,GROSS            R1 = NET                                     
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         L     R0,CSHDSC           R1 = NET - CASH DISCOUNT                     
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
*                                                                               
         USING SCRLIN1D,R2                                                      
SBUYSHW5 LA    R1,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,0(R1)),2,ALIGN=LEFT,MINUS=YES                      
         B     SBUY240                                                          
*                                                                               
SBUY235S CLC   =C'CT',CHKOPTN      CTPBI DISPLAY?                               
         BE    SBUY240                                                          
         CLC   =C'GS',CHKOPTN      GST AMT?                                     
         BNE   SBUYBOPT                                                         
*                                                                               
         MVC   FULL,GSTTAX                                                      
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NET                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NET                                                       
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(11,0(R3)),2,ALIGN=LEFT,MINUS=YES                      
         B     SBUY240                                                          
*                                                                               
SBUY240  CLI   SCRTYPE,C'N'                                                     
         BNE   SBUY240A                                                         
         USING SCRLIN1D,R2                                                      
         OI    SLN1IDTH+6,X'20'                                                 
         OI    SLN1SIZH+6,X'20'                                                 
         OI    SLN1RTEH+6,X'20'                                                 
         OI    SLN1PRMH+6,X'20'                                                 
         OI    SLN1CTPH+6,X'20'                                                 
         OI    SLN1GRSH+6,X'20'                                                 
         OI    SLN1NETH+6,X'20'                                                 
         OI    SLN1ESTH+6,X'20'                                                 
         B     SBUYX                                                            
         USING SCRLIN2D,R2                                                      
SBUY240A OI    SLN2IDTH+6,X'20'    PROTECT LINE WITH THE BUY                    
         OI    SLN2SPCH+6,X'20'                                                 
         OI    SLN2CTPH+6,X'20'                                                 
         OI    SLN2GRSH+6,X'20'                                                 
         OI    SLN2NETH+6,X'20'                                                 
         OI    SLN2ESTH+6,X'20'                                                 
*                                                                               
SBUYX    B     XIT                                                              
*                                                                               
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE CALCULATES GROSS AMOUNTS TO NET AMOUNTS BASED ON A               
* PERCENTAGE.                                                                   
*                                                                               
* ON ENTRY:    GROSSAMT    P       GROSS AMOUNT                                 
*              PERCENTG    P       PERCENTAGE OF GROSS THAT IS NET              
*                                                                               
* ON EXIT:     NETAMNT     P       NET AMOUNT                                   
*                                                                               
* WARNING:     ALL 3 SPECIFIED VARIABLES WILL GET CLOBBERED                     
***********************************************************************         
GRSTONET NTR1                                                                   
         CP    PERCENTG,=P'0'                                                   
         BE    GTON10                                                           
         ZAP   NETAMNT,=P'100000'       NET = GROSS * (1-%AGE)                  
         SP    NETAMNT,PERCENTG                                                 
         ZAP   PERCENTG,NETAMNT                                                 
         MP    GROSSAMT,PERCENTG                                                
         SRP   GROSSAMT,64-5,5          DIVIDE BY 100000 AND ROUND              
GTON10   ZAP   NETAMNT,GROSSAMT                                                 
*                                                                               
         B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL SET KEY TO AT MOST THE 3RD PREVIOUS KEY                     
***********************************************************************         
CALCMORE DS    0H                                                               
         NMOD1 0,**CALC**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
****     B     CALC90              CALCULATE BEFORE GOING SEQ                   
*                                                                               
CALCNEXT GOTO1 SEQ                 GET NEXT BUY                                 
*                                                                               
CALCLOOP LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME UPTO PRD?                   
         BNE   CALCX                           NO                               
*                                                                               
         CLC   =C'***',CHKPRD      YES, PRODUCT VARIOUS?                        
         BE    *+14                     YES, SKIP PRODUCT TEST                  
         CLC   PBUYKPRD,QPRD            NO, MAKE SURE PROD SAME THEN            
         BNE   CALCX                        NOT THE SAME                        
*                                                                               
         CLC   PBUYKPUB(4),BPUB    ARE THE PUBS THE SAME?                       
         BE    CALC10              YES                                          
         BL    CALCNEXT            NO, BUY'S PUB < REQUESTED PUB                
*                                                                               
*                                  NO, BUY'S PUB > REQUESTED PUB                
         CLC   =C'***',CHKPRD          PRODUCT VARIOUS?                         
         BNE   CALCX                   NO, START FROM BEG NEXT TIME             
         MVI   PBUYKPUB,X'FF'          YES, GO TO NEXT PRODUCT                  
CALCHIGH GOTO1 HIGH                                                             
         B     CALCLOOP                                                         
*                                                                               
CALC10   TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BO    CALC20                                                           
         CLC   PBUYKZON(2),BPUB+4                                               
         BNE   CALCNEXT                                                         
*                                                                               
CALC20   CLC   PBUYKDAT,INVSTDT                                                 
         BNL   CALC30                                                           
*                                                                               
         TM    GLOBFLG1,X'80'                                                   
         BNZ   *+14                                                             
         CLC   =C'***',CHKPRD                                                   
         BNE   CALCX                                                            
         MVC   PBUYKDAT,INVSTDT                                                 
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     CALCHIGH                                                         
*                                                                               
CALC30   CLC   PBUYKDAT,INVENDDT                                                
         BNH   CALC40                                                           
*                                                                               
         TM    GLOBFLG1,X'80'                                                   
         BZ    *+12                                                             
         MVI   PBUYKDAT,X'FF'                                                   
         B     CALCHIGH                                                         
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BNE   CALCX                                                            
         MVI   PBUYKPUB,X'FF'                                                   
         B     CALCHIGH                                                         
*                                                                               
CALC40   OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    CALC70              NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   CALCNEXT            NO, GET NEXT BUY RECORD                      
*                                                                               
CALC70   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   CALCNEXT            THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    CALCNEXT                                                         
*                                                                               
         MVI   ELCODE,X'80'        ANY SPECIAL REP?                             
         BAS   RE,GETEL                                                         
         BE    CALC80              YES                                          
         OC    SPCLREP,SPCLREP     NONE, SHOULD WE HAVE ONE?                    
         BZ    CALC90              NO, WE'RE NOT SUPPOSE TO HAVE ONE            
         B     CALCNEXT            YES WE ARE, GET NEXT BUY                     
*                                                                               
         USING PBSREPEL,R6                                                      
CALC80   CLC   PBSREP,SPCLREP      DOES SPECIAL REP MATCH INVHDR'S?             
         BNE   CALCNEXT            NO, GET NEXT BUY                             
*                                                                               
CALC90   L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT  PD/BILLD INFO          
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
         L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         AP    BUYAMNTG,DUB                                                     
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         AP    BUYAMNTN,DUB                                                     
         B     CALCNEXT                                                         
*                                                                               
CALCX    B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 2                                                                
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
* DDGLOBEQUS                                                                    
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* PUBGENEL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
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
**PAN#1  DC    CL21'166PPMAT04   05/01/02'                                      
         END                                                                    
