*          DATA SET SPSNV01    AT LEVEL 068 AS OF 06/07/16                      
*PHASE T21001A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21001 - LIST OF INVOICES                                             
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVFE (T210FE) -- LIST                                        
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
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
***********************************************************************         
T21001   TITLE 'SPSNV01 - SPOT INVOICE LIST OVERLAY'                            
T21001   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21001*,R7,RR=R3                                              
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
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'INV',3,GLVPGM                              
*                                                                               
         MVI   NETPAKSW,C'N'       SET NETPAK SWITCH                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         MVI   NETPAKSW,C'Y'                                                    
         DROP  R1                                                               
*                                                                               
MAIN00   BAS   RE,SETPFTBL                                                      
*                                                                               
         XC    ACURFORC,ACURFORC                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LRECS                                                            
*                                                                               
MAINX    B     XIT                                                              
*                                                                               
***********************************************************************         
* SET THE PFKEY TABLE                                                           
***********************************************************************         
SETPFTBL NTR1                                                                   
         SR    R2,R2               NO TABLE NEEDED YET                          
*                                                                               
         CLI   PFKEY,1             WE DON'T CARE FOR ACTION ADD                 
         BE    STPF20                                                           
*                                                                               
         TM    LSTMEDH+4,X'20'     IF ANY OF THE KEY FIELDS CHANGE              
         BNZ   STPF05                                                           
         CLI   LSTMEDH+5,0                                                      
         BNE   STPF10                                                           
         OI    CTLRFLG1,CF1NOCLR                                                
         MVI   PFKEY,0             THIS WAY SAVE SCREEN, REC, & ACTION          
         B     STPF20                 GETS SAVED, AND WE DON'T CLEAR            
*                                                                               
STPF05   TM    LSTCLTH+4,X'20'                                                  
         BZ    STPF10                                                           
         TM    LSTSTAH+4,X'20'                                                  
         BZ    STPF10                                                           
         TM    LSTOPTNH+4,X'20'                                                 
         BNZ   STPF20                                                           
*                                                                               
STPF10   XC    SEQLIST,SEQLIST     THEN SEQUENCE LIST GETS CLOBBERED            
         MVI   SELOFFST,X'FF'      OFFSET OF SELECTED LINE FROM 1ST LN          
         LA    R3,LSTSELL                                                       
         USING LINDSECT,R3                                                      
         LA    R3,LINTOTH                                                       
         DROP  R3                                                               
         TWAXC LSTSEL1H,(R3),PROT=Y                                             
         B     STPFX               DON'T ALLOW PFKEY PROCESSING                 
*                                                                               
STPF20   SR    R2,R2                                                            
         CLI   PFKEY,5             PFKEYS 1-5 ARE USED                          
         BNH   STPF30                                                           
         CLI   PFKEY,10                                                         
         BNE   STPF50                                                           
*                                                                               
STPF30   LA    R2,PFTABLE                                                       
STPF50   DS    0H                                                               
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   STPF55                                                           
         CLI   PFKEY,5                                                          
         BE    PFERR                                                            
*                                                                               
STPF55   DS    0H                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
*                                                                               
*                                                                               
* CURDISP IS NOT SET CORRECTLY FOR LINES SELECTED THROUGH THE SELECT            
* FIELD UNTIL INITIAL GET A CHANCE TO CONVERT THE SELECT CODE,                  
* SO WE NEED TO DO TWO PASSES FOR THOSE PFKEYS WHERE CURDISP IS VITAL           
* TO THE CALCULATION OF THE INVOICE HEADER.                                     
*                                                                               
         CLI   PFKEY,2                                                          
         BL    STPFX                                                            
         CLI   PFKEY,4             CURDISP VITAL FOR THE PFKEY?                 
         BNH   STPF60                                                           
         CLI   PFKEY,5                                                          
         BE    STPFK05                                                          
         CLI   PFKEY,10                                                         
         BE    STPF60                                                           
         B     STPFX               NO                                           
*                                                                               
STPF60   TM    CTLRFLG1,CF1CKOFF          NEED TO CHECK THE OFFSET?             
         BZ    STPF70                     NO                                    
         NI    CTLRFLG1,X'FF'-CF1CKOFF    DON'T CHECK IT NEXT TIME IN           
*                                                                               
         CLI   SELOFFST,X'FF'      ANY PREVIOUS SELECTION?                      
         BE    STPF70                                                           
         ZIC   R1,SELOFFST         YES, CALCULATE LOCATION ON SCREEN            
         MH    R1,=Y(LINNEXTL-LINDSECT)                                         
         LA    R0,LSTSEL1H                                                      
         AR    R1,R0                                                            
         SR    R1,RA                                                            
         STH   R1,CURDISP                                                       
*                                                                               
STPF70   LH    R2,CURDISP          SEE IF CURSOR IS IN A VALID LOCATION         
         AR    R2,RA                   FOR A PFKEY SELECTION                    
         LA    R0,LSTSEL1H                                                      
         CR    R2,R0                                                            
         BL    MISSFLD                                                          
         LA    R0,LSTPFLNH                                                      
         CR    R2,R0                                                            
         BNL   MISSFLD                                                          
*                                                                               
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF LINE                           
         LA    R3,LSTSEL1H                                                      
         LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         STC   R1,SELOFFST                                                      
         MH    R1,=Y(LSEQNEXT-LSEQNTRY)                                         
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(LSEQNEXT-LSEQNTRY,R1),0(R1)   ANY INVOICE HEADER?              
         BZ    MISSFLD                         DON'T GO ANYWHERE                
*                                                                               
         CLI   PFKEY,4                                                          
         BE    STPFK04                                                          
         CLI   PFKEY,10                                                         
         BE    STPFK10                                                          
         B     STPF100                                                          
*                                                                               
STPFK04  XC    KEYLINE,KEYLINE     YES, DETAILS IS SPECIAL                      
         LA    R3,KEYLINE                                                       
*                                                                               
         MVC   0(L'QMED,R3),QMED                                                
         MVI   L'QMED(R3),C'.'                                                  
         LA    R3,L'QMED+1(R3)                                                  
*                                                                               
         ZIC   R1,LSTCLTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LSTCLT                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,LSTSTAH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LSTSTA                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,SELOFFST               CALCULATE BEGINNING OF LINE            
         MH    R1,=Y(LINNEXTL-LINDSECT)      BECAUSE CURSOR COULD BE IN         
         LA    R2,LSTSEL1H(R1)               THE MIDDLE OF THE LINE             
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STH   RE,CURDISP                                                       
*                                                                               
         USING LINDSECT,R2                                                      
         OC    LINPER,LINPER       ANY DATA ON THIS LINE?                       
         BZ    STPFX               NONE, DON'T ALLOW PFKEY                      
*                                                                               
         ZIC   R1,LINPERH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LINPER                                                   
         LA    R3,1(R1,R3)                                                      
         MVI   0(R3),C'.'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,LININVH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),LININV                                                   
*                                                                               
STPFK04X B     STPF100                                                          
         DROP  R2                                                               
*                                                                               
STPFK10  ZIC   R1,SELOFFST               CALCULATE BEGINNING OF LINE            
         MH    R1,=Y(LINNEXTL-LINDSECT)      BECAUSE CURSOR COULD BE IN         
         LA    R2,LSTSEL1H(R1)               THE MIDDLE OF THE LINE             
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STH   RE,CURDISP                                                       
*                                                                               
         USING LINDSECT,R2                                                      
         ZIC   R0,LINPERH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINPER,(R0),GLVSPPER                          
*                                                                               
         CLI   LINPRDH+7,0                                                      
         BE    STPFK10A                                                         
         ZIC   R0,LINPRDH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINPRD,(R0),GLVSPPRD                          
         B     STPFK10B                                                         
*                                                                               
STPFK10A DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPRD                                    
*                                                                               
STPFK10B CLI   LINESTH+7,0                                                      
         BE    STPFK10C                                                         
         ZIC   R0,LINESTH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINEST,(R0),GLVSPEST                          
         B     STPFK10X                                                         
*                                                                               
STPFK10C DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEST                                    
*                                                                               
STPFK10X B     STPF100                                                          
*                                                                               
STPF100  LA    R2,SPFTABLE                                                      
         OI    CTLRFLG1,CF1TSELQ   IGNORE SELECT CODES THIS TIME IN             
         OI    CTLRFLG1,CF1CKOFF   DIS/CHA NEEDS TO SAVE SEL OFFSET             
         GOTO1 INITIAL,DMCB,(R2)                                                
         B     STPFX                                                            
*                                                                               
STPFK05  LH    R2,CURDISP          CALCULATE BEGINNING OF THE LINE              
         AR    R2,RA                   BECAUSE CURSOR COULD BE IN THE           
         LA    RE,LSTSEL1H         FIRST LIST LINE                              
         CR    R2,RE               DID WE PF5 BEFORE THAT?                      
         BL    PFERR2              YES, GIVE ERROR MESSAGE                      
         LA    RE,LSTPFLNH         PF LINE                                      
         CR    R2,RE               DID WE PF5 ON THIS LINE?                     
         BNL   PFERR2              YES, GIVE ERROR MESSAGE                      
         LH    R1,2(R2)                MIDDLE OF THE LINE                       
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE             ROW NUMBER OF SELECTED LINE                  
         LA    RE,LSTSEL1H                                                      
         LH    R1,2(RE)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0               DISPLACEMENT TO LINE FROM FIRST LINE         
         MH    R1,=Y(LINNEXTL-LINDSECT)                                         
         LA    R2,LSTSEL1H(R1)                                                  
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STH   RE,CURDISP                                                       
*                                                                               
         USING LINDSECT,R2                                                      
         ZIC   R0,LINPERH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINPER,(R0),GLVSPPER                          
*                                                                               
         CLI   LINPRDH+7,0         OUTPUT LENGTH                                
         BE    STPFK05A                                                         
         ZIC   R0,LINPRDH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINPRD,(R0),GLVSPPRD                          
         B     STPFK05B                                                         
*                                                                               
STPFK05A DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'POL',3,GLVSPPRD                            
*                                                                               
STPFK05B CLI   LINESTH+7,0                                                      
         BE    STPFK05C                                                         
         ZIC   R0,LINESTH+7                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',LINEST,(R0),GLVSPEST                          
*                                                                               
         CLI   LINPRDH+7,0         IF WE HAVE A PRODUCT ALSO                    
         BE    STPFK05D                                                         
         ZIC   R1,LINPRDH+7        THEN PUT OUT PRD/EST                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),LINPRD                                                  
         LA    RE,BLOCK+1                                                       
         AR    RE,R1                                                            
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         ZIC   R1,LINESTH+7                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),LINEST                                                   
         ZIC   R1,LINESTH+7                                                     
         ZIC   R0,LINPRDH+7                                                     
         AR    R0,R1                                                            
         AH    R0,=H'1'                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',BLOCK,(R0),GLVSPPRD                           
         B     STPFK05D                                                         
*                                                                               
STPFK05C DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEST                                    
*                                                                               
STPFK05D CLI   LINRME+1,C'M'       DO WE HAVE A MIDFLIGHT INVOICE?              
         BNE   STPFK05E                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',=C'MCT',3,GLVSPOPT                            
         B     STPFK05F                                                         
*                                                                               
STPFK05E DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPOPT                                    
*                                                                               
STPFK05F XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'NIN'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'MAT'                                                 
         OI    GLVXFLG1,GLV1SEPS+GLV1RETG   CALL BASE ON TRANSFER               
         DROP  R1                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,14,GLVXCTL                               
*                                                                               
STPFK05X L     RD,SYSRD                                                         
         B     STPFX                                                            
         DROP  R2                                                               
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         MVC   LLIST,=Y(LINNEXTL-LINPERH)  L(LINE W/O SELECT FIELD)             
         NI    MISCFLG1,X'FF'-MF1KYCHG   ASSUME NO KEY FIELDS CHANGED           
*                                                                               
*        GOTO1 GETFACT,DMCB,0                                                   
*        L     R1,0(R1)                                                         
*        USING FACTSD,R1                                                        
*        CLI   FAOVSYS,3           NET SYSTEM?                                  
         CLI   NETPAKSW,C'Y'                                                    
         BNE   VKMED00                                                          
         MVC   LSTPFLN+43(9),=9C' '   DON'T SHOW PF5=MATCH                      
         OI    LSTPFLNH+6,X'80'                                                 
*        DROP  R1                                                               
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  LA    R2,LSTMEDH                                                       
         CLI   5(R2),0                                                          
         BNE   VKMED10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPMD                                 
         CLI   5(R2),0                                                          
         BE    NEEDMDIA                                                         
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
VKMED10  TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   LSTMDNM(L'MEDNM),MEDNM       SHOW MEDIA NAME                     
         OI    LSTMDNMH+6,X'80'                                                 
*                                                                               
VKMEDX   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPMD                                 
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKCLT00  LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VKCLT10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPCLT                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
VKCLT10  TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   LSTCLNM,CLTNM       SHOW CLIENT NAME                             
         OI    LSTCLNMH+6,X'80'                                                 
*                                                                               
VKCLTX   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPCLT                                
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  LA    R2,LSTSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPSTA                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
VKSTA10  TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALISTA                                                          
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   LSTSTNM,MKTNM       SHOW MARKET NAME                             
         OI    LSTSTNMH+6,X'80'                                                 
         OC    BSTA+2(L'BNTWK),BNTWK                                            
*                                                                               
VKSTAX   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPSTA                                
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE PERIOD                                                           
*****                                                                           
VKPER00  LA    R2,LSTPERDH                                                      
         NI    FILTFLG1,X'FF'-FF1PERD  TAKE OFF PERIOD FILTER                   
         XC    FILTSTDT,FILTSTDT                                                
         XC    FILTNDDT,FILTNDDT                                                
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKPERX                                                           
*                                                                               
         LA    R1,8(R2)            R1 = A(PERIOD TEXT)                          
         MVC   BYTE,5(R2)          BYTE = LENGTH OF PERIOD TEXT                 
*                                                                               
         CLI   8(R2),C'-'          SPECIFIC MONTH & ALL INVOICES B4 IT?         
         BNE   VKPER10                                                          
         LA    R1,1(R1)            YES, SKIP THE '-' SIGN                       
         B     VKPER20                                                          
*                                                                               
VKPER10  CLI   8(R2),C'+'          MOST CURRENT INVOICES TO A MONTH?            
         BNE   VKPER30                                                          
         LA    R1,1(R1)            YES, SKIP THE '+' SIGN                       
*                                                                               
VKPER20  ZIC   R0,BYTE             DECREMENT L(PERIOD TEXT)                     
         BCTR  R0,0                                                             
         STC   R0,BYTE                                                          
*                                                                               
VKPER30  ST    R1,DMCB             SETUP INPUT PARAMETER FOR PERVAL             
         MVC   DMCB(1),BYTE                                                     
*                                                                               
         GOTO1 PERVAL,DMCB,,PERVALST                                            
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLFLD                                                          
*                                                                               
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY  ALL OF END ASSUMED?            
         BNO   VKPER40                           NO                             
***********************************************************************         
* IF ALL OF THE END DATE'S PORTIONS (YYMMDD) ARE ASSUMED THEN THE               
* FIELD WAS ENTERED IN THIS FASHION:  "MMM/YY"  OR  "MMM/YY-"                   
***********************************************************************         
         CLI   8(R2),C'-'          MMM/YY AND ALL INVOICES BEFORE IT?           
         BNE   VKPER35                                                          
         MVC   FILTNDDT,PVALCEND                                                
         XC    FILTNDDT,=X'FFFF'                                                
         B     VKPER60             YES, NO START DATE                           
*                                                                               
VKPER35  CLI   8(R2),C'+'          ALL INVOICES ON AND AFTER MMM/YY?            
         BNE   VKPER40             NO, JUST  MMM/YY INOVICES                    
         MVC   FILTSTDT,PVALCSTA                                                
         XC    FILTSTDT,=X'FFFF'                                                
         B     VKPER60             YES, NO END DATE                             
*                                                                               
VKPER40  MVC   FILTNDDT,PVALCEND      DATE IS X'FF' COMPLEMENTED                
         XC    FILTNDDT,=X'FFFF'                                                
*                                                                               
VKPER50  TM    PVALASSM,PVALASD+PVALASM+PVALASY  ALL OF START ASSUMED?          
         BO    VKPER60                           YES, NO START DATE             
         MVC   FILTSTDT,PVALCSTA                                                
         XC    FILTSTDT,=X'FFFF'                                                
*                                                                               
VKPER60  OI    FILTFLG1,FF1PERD    FILTER ON PERIOD                             
*                                                                               
VKPERX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
         DROP  R3                                                               
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  LA    R2,LSTINVCH                                                      
         XC    FILTINVC,FILTINVC                                                
         MVI   FILTINVL,0                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKINVX                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILTINVC(0),8(R2)                                                
         MVC   FILTINVL,5(R2)                                                   
*                                                                               
VKINVX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE OPTIONS                                                          
*****                                                                           
VKOPT00  LA    R2,LSTOPTNH                                                      
         MVC   BOOKOVR,SPACES                                                   
         MVI   INTOPT,C' '                                                      
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                 YES                                          
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         NI    FILTFLG1,FF1PERD    CLEAR ALL FILTERS BUT PERIOD                 
         MVI   FILTFLG2,0                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKOPTX                                                           
*                                                                               
         L     RE,ATIOB                                                         
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR FOR ERROR                         
         LR    R1,R2                                                            
         SR    R1,RA                                                            
         STCM  R1,3,TIOBCURD       OFFSET TO FIELD FROM START OF TWA            
         DROP  RE                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'88',BLOCK)                                  
         CLI   4(R1),0                                                          
         BE    INVLFLD                                                          
         CLI   4(R1),7                                                          
         BH    INVLFLD             TOO MANY INPUTS FOR OPTIONS                  
*                                                                               
         LA    R3,BLOCK                                                         
VKOPT10  CLI   0(R3),0             ANY MORE ENTRIES?                            
         BE    VKOPTX              NONE                                         
*                                                                               
         CLI   1(R3),0             SEPARATED BY AN '=' SIGN?                    
         BE    VKOPT50             NO                                           
***********************************                                             
* FIELD SEPARATED BY '=' SIGN                                                   
***********************************                                             
         L     RE,ATIOB            POINT TO 1ST HALF                            
         USING TIOBD,RE                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  RE                                                               
*                                                                               
         ZIC   R1,0(R3)            R1=L(1ST HALF)                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'INV'    INVOICE DATE FILTER?                         
         BNE   VKOPT13                                                          
         TM    FILTFLG2,FF2INVDT   GOT ONE ALREADY?                             
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2INVDT   YES                                          
         LA    R2,FILTIVDT         R0=A(FILTER INVOICE DATE)                    
         B     VKOPT16                                                          
*                                                                               
VKOPT13  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'DUE'    DUE DATE FILTER?                             
         BNE   VKOPT20                                                          
         TM    FILTFLG2,FF2DUEDT   GOT ONE ALREADY?                             
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2DUEDT   YES                                          
         LA    R2,FILTDUDT         R0=A(FILTER DUE DATE)                        
*                                                                               
VKOPT16  CLI   1(R3),0             ANY DATA ON 2ND HALF?                        
         BE    VKOPTINV                                                         
*                                                                               
         LA    R0,22(R3)                                                        
         ST    R0,DMCB                                                          
         MVC   DMCB(1),1(R3)                                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(X'40',PERVALST)                                    
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLFLD                                                          
*                                                                               
         LA    RE,PERVALST                                                      
         USING PERVALD,RE                                                       
         MVC   0(L'PVALCSTA,R2),PVALCSTA   SAVE THE DATE                        
         B     VKOPTLP                                                          
         DROP  RE                                                               
*                                                                               
VKOPT20  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'PRD='                                                
         BNE   VKOPT30                                                          
         TM    FILTFLG2,FF2PRDCT   ERROR IF WE HAVE ONE ALREADY                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2PRDCT                                                
         XC    FILTBPRD(L'FILTBPRD*2+L'FILTQPRD*2),FILTBPRD                     
*                                                                               
         CLI   1(R3),3             DO WE HAVE 2 PRODUCTS?                       
         BH    VKOPT24             MAYBE                                        
         CLI   1(R3),2                                                          
         BL    VKOPTINV                                                         
*********                                                                       
* SINGLE PRODUCT TO FILTER ON                                                   
*********                                                                       
         LA    R2,22(R3)           R2=A(EBCDIC PRODUCT CODE)                    
         BAS   RE,FINDBPRD                                                      
         CLI   BYTE,0              WE GET BACK A BINARY CODE?                   
         BNE   VKOPT22             YES                                          
         CLI   NETPAKSW,C'Y'       ARE WE NETPAK?                               
         BNE   VKOPTINV            NO, WE NEED BINARY CODE FOR SPOTPAK          
VKOPT22  MVC   FILTBPRD,BYTE                                                    
         MVC   FILTQPRD,22(R3)                                                  
         B     VKOPT20X                                                         
*********                                                                       
* SHOULD BE TWO PRODUCTS HERE                                                   
*********                                                                       
VKOPT24  CLI   1(R3),7                                                          
         BH    VKOPTINV                                                         
*                                                                               
         LA    R2,22(R3)                                                        
         CLI   2(R2),C'-'          2 BYTE PRODUCT CODE?                         
         BNE   VKOPT26                                                          
*                                                                               
         ZIC   R1,1(R3)            YES, MAKE IT A 3 BYTE PRODUCT CODE           
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R2)       SAVE 2ND PRODUCT WITH THE '-'                
         MVI   2(R2),C' '                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R2),WORK                                                     
*                                                                               
         AH    R1,=H'4'            READJUST LENGTH                              
         STC   R1,1(R3)                                                         
*                                                                               
VKOPT26  CLI   1(R3),7                                                          
         BH    VKOPTINV                                                         
*                                                                               
         CLI   3(R2),C'-'          SHOULD HAVE A PIGGYBACK PRODUCT HERE         
         BNE   VKOPTINV                                                         
         BAS   RE,FINDBPRD                                                      
         CLI   BYTE,0              WE GET BACK A BINARY CODE?                   
         BNE   VKOPT27             YES                                          
         CLI   NETPAKSW,C'Y'       ARE WE NETPAK?                               
         BNE   VKOPTINV            NO, WE NEED BINARY CODE FOR SPOTPAK          
VKOPT27  MVC   FILTBPRD,BYTE                                                    
         MVC   FILTQPRD,0(R2)                                                   
*                                                                               
         ZIC   R1,1(R3)                                                         
         SH    R1,=H'4'                                                         
         CH    R1,=H'2'                                                         
         BL    VKOPTINV                                                         
         CH    R1,=H'3'                                                         
         BH    VKOPTINV                                                         
         LA    R2,4(R2)                                                         
         BAS   RE,FINDBPRD                                                      
         CLI   BYTE,0              WE GET BACK A BINARY CODE?                   
         BNE   VKOPT28             YES                                          
         CLI   NETPAKSW,C'Y'       ARE WE NETPAK?                               
         BNE   VKOPTINV            NO, WE NEED BINARY CODE FOR SPOTPAK          
VKOPT28  MVC   FILTBPR2,BYTE                                                    
         MVC   FILTQPR2,0(R2)                                                   
*                                                                               
VKOPT20X B     VKOPTLP                                                          
*                                                                               
VKOPT30  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'EST='                                                
*        BNE   VKOPTINV                                                         
         BNE   VKOPT40                                                          
         TM    FILTFLG2,FF2ESTMT   ERROR IF WE HAVE ONE ALREADY                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG2,FF2ESTMT                                                
         MVI   FILTBEST,0                                                       
*                                                                               
         CLI   1(R3),0             NEED A VALUE AFTER 'EST='                    
         BE    VKOPTINV                                                         
*                                                                               
* CHECK FOR VALID NUMERIC...WILL DIE ON CVB!!                                   
         ZIC   R1,1(R3)                                                         
         LA    RE,22(R3)                                                        
VKOPT31  CLI   0(RE),X'F0'                                                      
         BL    VKOPTINV                                                         
         CLI   0(RE),X'F9'                                                      
         BH    VKOPTINV                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,VKOPT31                                                       
                                                                                
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R3)                                                     
         CVB   R1,DUB                                                           
         LTR   R1,R1               MAKE SURE ESTIMATE CAN BE A VALID            
         BZ    VKOPTINV               ESTIMATE                                  
         CH    R1,=H'255'                                                       
         BH    VKOPTINV                                                         
         STC   R1,FILTBEST         SAVE THE EBCDIC VALUE                        
         B     VKOPTLP                                                          
*                                                                               
VKOPT40  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'BOOK='                                               
         BNE   VKOPTINV                                                         
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BE    VKOPTINV                                                         
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   VKOPTINV                                                         
*                                                                               
         CLI   1(R3),0             NEED A VALUE AFTER 'BOOK='                   
         BE    VKOPTINV                                                         
*                                                                               
         BRAS  RE,VALBOOK                                                       
         BNE   VKOPTINV                                                         
*                                                                               
         B     VKOPTLP                                                          
*                                                                               
VKOPTINV B     INVLFLD                                                          
***********************************                                             
* PLAIN FIELD, NOT SEPARATED BY '=' SIGN                                        
***********************************                                             
VKOPT50  L     RE,ATIOB                                                         
         USING TIOBD,RE                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  RE                                                               
*                                                                               
         ZIC   R1,0(R3)            R1 = L(OPTION)                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8              FILTER ON RESPONSE INVOICES?                 
         B     *+10                                                             
         CLC   12(0,R3),=C'RESPONSE'                                            
         BNE   VKOPT52                                                          
         TM    FILTFLG1,FF1NRSPN                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1RSPNS   YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT52  EX    R1,*+8              FILTER ON NON-RESPONSE INVOICES?             
         B     *+10                                                             
         CLC   12(0,R3),=C'-RESPONSE'                                           
         BNE   VKOPT54                                                          
         TM    FILTFLG1,FF1RSPNS                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NRSPN   YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT54  EX    R1,*+8              FILTER ON MIDFLIGHT CLEARANCE?               
         B     *+10                                                             
         CLC   12(0,R3),=C'MCT'                                                 
         BNE   VKOPT56                                                          
         TM    FILTFLG1,FF1NMCT                                                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1MCT     YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT56  EX    R1,*+8              FILTER ON NON-MIDFLIGHT CLEARANCE?           
         B     *+10                                                             
         CLC   12(0,R3),=C'-MCT'                                                
         BNE   VKOPT58                                                          
         TM    FILTFLG1,FF1MCT                                                  
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NMCT    YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT58  EX    R1,*+8              FILTER ON EASI INVOICES?                     
         B     *+10                                                             
         CLC   12(0,R3),=C'EASI'                                                
         BNE   VKOPT60                                                          
         TM    FILTFLG1,FF1NEASI                                                
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1EASI    YES                                          
         B     VKOPTLP                                                          
*                                                                               
VKOPT60  EX    R1,*+8              FILTER ON EASI INVOICES?                     
         B     *+10                                                             
         CLC   12(0,R3),=C'-EASI'                                               
         BNE   VKOPT70                                                          
         TM    FILTFLG1,FF1EASI                                                 
         BNZ   VKOPTINV                                                         
         OI    FILTFLG1,FF1NEASI    YES                                         
         B     VKOPTLP                                                          
*                                                                               
VKOPT70  DS    0H                                                               
* INT/INTONLY OPTIONS DISABLED FOR NOW 11/04/2007                               
         B     VKOPTINV                                                         
*                                                                               
         EX    R1,*+8              INT OPTION?                                  
         B     *+10                                                             
         CLC   12(0,R3),=C'INT'                                                 
         BNE   VKOPT80                                                          
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   VKOPTINV                                                         
         CLI   INTOPT,C' '                                                      
         BNE   VKOPTINV                                                         
         MVI   INTOPT,C'I'                                                      
*                                                                               
VKOPT80  EX    R1,*+8              INTONLY OPTION?                              
         B     *+10                                                             
         CLC   12(0,R3),=C'INTONLY'                                             
         BNE   VKOPTINV                                                         
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   VKOPTINV                                                         
         CLI   INTOPT,C' '                                                      
         BNE   VKOPTINV                                                         
         MVI   INTOPT,C'O'                                                      
*                                                                               
VKOPTLP  LA    R3,32(R3)           CHECK NEXT ENTRY                             
         B     VKOPT10                                                          
*                                                                               
VKOPTX   LA    R2,LSTOPTNH         VALIDATE THE FIELD                           
         OI    4(R2),X'20'                                                      
*                                                                               
         L     RE,ATIOB            DON'T NEED SPECIAL ERROR CURSOR              
         USING TIOBD,RE               ANYMORE                                   
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  RE                                                               
*                                                                               
VKX      TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    XIT                                                              
*                                                                               
         XC    SVMASTKY,SVMASTKY   YES, START FROM BEGINNING                    
         XC    KEYSAVED,KEYSAVED                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINE THE BINARY PRODUCT CODE FROM EBCDIC PRODUCT CODE                    
*                                                                               
* ON ENTRY:    (R2)                A(EBCDIC PRODUCT)                            
*                                                                               
* ON EXIT:     BYTE                BINARY PRODUCT OR 0 IF NONE                  
*                                                                               
* NOTE: R1 AND BYTE BOTH GET CLOBBERED                                          
***********************************************************************         
FINDBPRD LA    R1,SVCLIST                                                       
         MVI   BYTE,0                                                           
FBPRD10  CLI   0(R1),0                                                          
         BE    FBPRDX                                                           
*                                                                               
         CLC   0(3,R1),0(R2)       EBCIDC PRODUCT MATCH?                        
         BNE   *+14                                                             
         MVC   BYTE,3(R1)          YES, SAVE IT IN BYTE                         
         B     FBPRDX                                                           
*                                                                               
         LA    R1,4(R1)                                                         
         B     FBPRD10                                                          
*                                                                               
FBPRDX   BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORDS                                                              
***********************************************************************         
LRECS    DS    0H                                                               
*                                                                               
         CLI   PFKEY,11                                                         
         BNE   *+10                                                             
         MVC   SVMASTKY,KEYSAVED                                                
*                                                                               
         MVI   NLISTS,13                                                        
         XC    SEQLIST,SEQLIST     NO RECORDS SHOWN                             
         NI    MISCFLG1,X'FF'-MF1NDSCR   NOT END OF SCREEN YET                  
*                                                                               
         LA    R2,LSTSEL1H         R2 = A(1ST LIST LINE)                        
         LA    R3,LSTSELLH         CLEAR LIST PORTION OF SCREEN                 
         USING LINDSECT,R3                                                      
         TWAXC LSTSEL1H,LINTOTH,PROT=Y                                          
         DROP  R3                                                               
*                                                                               
         USING LINDSECT,R2                                                      
         LA    R3,SEQLIST          R3 = A(1ST SAVED RECORD KEY)                 
         USING LSEQNTRY,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         OC    SVMASTKY,SVMASTKY         DID WE FINISH OUR LIST BEFORE?         
         BZ    LR10                                                             
         MVC   KEY(L'SVMASTKY),SVMASTKY  NO, CONTINUE FROM BEFORE               
         XC    SVMASTKY,SVMASTKY                                                
         B     LR20                                                             
*                                                                               
LR10     LA    R1,KEY                                                           
         USING SNVKEYD,R1                                                       
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         DROP  R1                                                               
*                                                                               
LR20     MVC   KEYSAVED,KEY           SAVE A COPY OF THE BASE KEY               
*                                                                               
         BAS   RE,XSPDHIGH                                                      
         CLI   DMCB+8,0                                                         
         BE    LRLOOP                                                           
         DC    H'0'                                                             
*                                                                               
LRLOOP   L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKEY(SNVKMOS-SNVKEY),KEYSAVED    GOT SAME BASE KEY?            
         BNE   LRENDLST                                                         
*                                                                               
LRFILT10 TM    FILTFLG1,FF1PERD    NEED TO CHECK PERIOD FILTER?                 
         BZ    LRFILT12            NO                                           
         OC    FILTSTDT,FILTSTDT                                                
         BZ    *+14                                                             
         CLC   FILTSTDT,SNVKMOS                                                 
         BL    LRNXTREC                                                         
         OC    FILTNDDT,FILTNDDT                                                
         BZ    *+14                                                             
         CLC   FILTNDDT,SNVKMOS                                                 
         BH    LRNXTREC                                                         
*                                                                               
LRFILT12 CLI   FILTINVL,0          NEED TO CHECK INVOICE FILTER?                
         BE    LR40                NO                                           
         ZIC   R1,FILTINVL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SNVKINV(0),FILTINVC                                              
         BNE   LRNXTREC                                                         
*                                                                               
LR40     CLC   SVMASTKY,SNVKMAST   SAME MASTER KEY?                             
         BE    LRNXTREC            YES, SAW THIS ONE ALREADY                    
         MVC   SVMASTKY,SNVKMAST   NO, SAVE A COPY OF THE MASTER KEY            
*                                                                               
         TM    MISCFLG1,MF1NDSCR   GOT ANOTHER RECORD AND END OF LIST?          
         BNZ   LRDISPLD                                                         
*                                                                               
         MVC   BMOSS,SNVKMOS       SET UP MINIO INFORMATION                     
         XC    BMOSS,=X'FFFF'      PERIOD IN KEY IS X'FF' COMPLEMENTED          
         MVC   QINVOICE,SNVKINV                                                 
         GOTO1 INITMNIO                                                         
*                                                                               
         MVC   LSEQPERD,BMOSS      SAVE THESE VALUES                            
         MVC   LSEQINVC,SNVKINV                                                 
*                                                                               
         MVI   IMFLAG,C'N'                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIMELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BNE   LR40A                                                            
         L     R6,MINELEM                                                       
         CLI   0(R6),SNVIMELQ                                                   
         BNE   LR40A                                                            
         MVI   IMFLAG,C'Y'                                                      
*                                                                               
LR40A    DS    0H                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
*                                                                               
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   EZFLAG,C'N'                                                      
*                                                                               
         L     R6,MINELEM          NOT INVOICE HEADERS?                         
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    LR41                                                             
*                                                                               
         L     R1,AIO                                                           
         USING SNVKEYD,R1                                                       
         MVC   LININV,SNVKINV                                                   
         MVC   BMOSS,SNVKMOS                                                    
         XC    BMOSS,=X'FFFF'                                                   
         GOTO1 DATCON,DMCB,(2,BMOSS),(9,LINPER)                                 
         MVC   LINEST,=CL3'***'                                                 
         MVC   LINDAT,=C'INVALID'                                               
         MVC   LINDDT(3),=CL3'***'                                              
         B     LR90X3                                                           
         DROP  R1                                                               
*                                                                               
LR41     TM    FILTFLG2,FF2PRDCT   FILTERING ON PRODUCT?                        
         BZ    LRFILT20                                                         
         CLI   FILTBPRD,0          WE HAVE A BINARY PRODUCT?                    
         BNE   LRFILT14            YES, USE IT BECAUSE IT IS FAR EASIER         
         CLI   SNVHDLEN,SNVHDLN2   HAVE EBCDIC PRODUCT IN HDR?                  
         BNH   LRAFMNIO            NO, CAN POSSIBLY MATCH FILTER THEN           
         CLC   FILTQPRD,SNVHDAP1   IT HAS TO MATCH EBCDIC PRODUCT               
         BNE   LRAFMNIO                                                         
         B     LRFILT16                                                         
LRFILT14 CLC   SNVHDPRD,FILTBPRD                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT16 CLI   FILTBPR2,0          WE HAVE A PIGGYBACK?                         
         BNE   LRFILT18            YES, AND IT IS BINARY                        
         CLC   FILTQPR2,SPACES     MAYBE, IS IT JUST EBCDIC?                    
         BNH   LRFILT20            NO PIGGYBACK PRODUCT FILTER                  
         CLI   SNVHDLEN,SNVHDLN2   HAVE EBCDIC PRODUCT IN HDR?                  
         BNH   LRAFMNIO            NO, CAN POSSIBLY MATCH FILTER THEN           
         CLC   FILTQPR2,SNVHDAP2   IT HAS TO MATCH EBCDIC PRODUCT               
         BNE   LRAFMNIO                                                         
         B     LRFILT16                                                         
*                                                                               
LRFILT18 CLC   SNVHDPR2,FILTBPR2                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT20 TM    FILTFLG2,FF2ESTMT   FILTERING ON ESTIMATE?                       
         BZ    LRFILT25                                                         
         CLC   SNVHDEST,FILTBEST                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT25 TM    FILTFLG2,FF2INVDT   FILTERING ON INVOICE DATE?                   
         BZ    LRFILT30                                                         
         CLC   SNVHDIDT,FILTIVDT                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT30 TM    FILTFLG2,FF2DUEDT   FILTERING ON DUE DATE?                       
         BZ    LRFILT32                                                         
         CLC   SNVHDDDT,FILTDUDT                                                
         BNE   LRAFMNIO                                                         
*                                                                               
LRFILT32 TM    FILTFLG1,FF1RSPNS   FILTERING ON RESPONSE INVOICES?              
         BZ    LRFILT34                                                         
         TM    SNVHDCTL,SNVHDRSQ                                                
         BZ    LRAFMNIO                                                         
         B     LRFILT36                                                         
*                                                                               
LRFILT34 TM    FILTFLG1,FF1NRSPN   FILTERING ON NON-RESPONSE INVOICES?          
         BZ    LRFILT36                                                         
         TM    SNVHDCTL,SNVHDRSQ                                                
         BNZ   LRAFMNIO                                                         
*                                                                               
LRFILT36 TM    FILTFLG1,FF1MCT     FILTERING ON MIDFLIGHT INVOICES?             
         BZ    LRFILT38                                                         
         TM    SNVHDCTL,SNVHDMCQ                                                
         BZ    LRAFMNIO                                                         
         B     LRFILT40                                                         
*&&DO                                                                           
LRFILT38 TM    FILTFLG1,FF1NMCT    FILTERING ON NON-MIDFLIGHT INVOICES?         
         BZ    LRFILT40                                                         
*&&                                                                             
LRFILT38 TM    SNVHDCTL,SNVHDMCQ                                                
         BNZ   LRAFMNIO                                                         
*                                                                               
LRFILT40 TM    FILTFLG1,FF1EASI    FILTERING ON EASI INVOICES?                  
         BZ    LRFILT42                                                         
         OC    SNVHDEZS,SNVHDEZS                                                
         BZ    LRAFMNIO                                                         
         B     LR45                                                             
*                                                                               
LRFILT42 TM    FILTFLG1,FF1NEASI   FILTERING ON NON-EASI INVOICES?              
         BZ    LR45                                                             
         OC    SNVHDEZS,SNVHDEZS                                                
         BNZ   LRAFMNIO                                                         
*                                                                               
LR45     ZAP   TAXAMNT,=P'0'                                                    
         CLC   =C'CK',AGENCY                                                    
         BNE   LR47                                                             
         ICM   R1,15,SNVHDTAX                                                   
         CVD   R1,TAXAMNT                                                       
*                                                                               
LR47     GOTO1 DATCON,DMCB,(2,LSEQPERD),(9,LINPER)                              
*                                                                               
         MVC   LININV,LSEQINVC                                                  
         TM    SNVHDCTL,SNVHDAUQ   INVOICE TO BE AUDITED?                       
         BZ    *+8                                                              
         OI    LININVH+6,X'88'     CHANGE TO HIGH INTENSITY                     
*                                                                               
         CLI   SNVHDPRD,0          ANY BINARY PRODUCT CODE IN HDR?              
         BNE   LR50                YES                                          
         CLI   SNVHDLEN,SNVHDLN2   NO, WE MIGHT HAVE JUST ALPHA PRODUCT         
         BNH   LR70                    NO, TRY DISPLAYING ESTIMATE              
         CLC   SNVHDAP1,SPACES                                                  
         BNH   LR70                                                             
         MVC   LINPRD(L'QPRD),SNVHDAP1   SURE ENOUGH, JUST ALPHA PRD            
         B     LR56                                                             
*                                                                               
LR50     DS    0H                                                               
         MVC   LINPRD(L'QPRD),=3C'?'                                            
         LA    R1,SNVHDPRD                                                      
         BRAS  RE,GETAPRD                                                       
         BNE   *+10                                                             
         MVC   LINPRD(L'QPRD),0(R1)                                             
*                                                                               
LR56     CLI   SNVHDPR2,0          ANY PIGGY?                                   
         BNE   LR58                                                             
         CLI   SNVHDLEN,SNVHDLN2   NO, WE MIGHT HAVE JUST ALPHA PRODUCT         
         BNH   LR70                    NO, TRY DISPLAYING ESTIMATE              
         CLC   SNVHDAP2,SPACES                                                  
         BNH   LR70                                                             
         MVI   LINPRD+L'QPRD,C'-'                                               
         MVC   LINPRD+L'QPRD+1(L'QPRD),SNVHDAP2   JUST ALPHA PRD                
         B     LR70                                                             
*                                                                               
LR58     DS    0H                                                               
         MVC   LINPRD+L'QPRD(4),=C'-???'                                        
         LA    R1,SNVHDPR2                                                      
         BRAS  RE,GETAPRD                                                       
         BNE   *+10                                                             
         MVC   LINPRD+L'QPRD+1(L'QPRD),0(R1)                                    
*                                                                               
LR70     CLI   SNVHDEST,0          ANY ESTIMATE TO SHOW?                        
         BE    LR80                                                             
         EDIT  (B1,SNVHDEST),(3,LINEST),FILL=0                                  
*                                                                               
LR80     GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,LINDAT)                              
         GOTO1 DATCON,DMCB,(2,SNVHDDDT),(8,LINDDT)                              
         TM    SNVHDCTL,SNVHDRSQ   RESPONSE INVOICE                             
         BZ    *+8                                                              
         MVI   LINRME,C'R'                                                      
         TM    SNVHDCTL,SNVHDMCQ   MCT INVOICE                                  
         BZ    *+8                                                              
         MVI   LINRME+1,C'M'                                                    
         OC    SNVHDEZS,SNVHDEZS   EASI SOURCE?                                 
         BZ    *+12                                                             
         MVI   LINRME+2,C'E'                                                    
         MVI   EZFLAG,C'Y'                                                      
*                                                                               
         CLI   IMFLAG,C'Y'                                                      
         BNE   *+8                                                              
         MVI   LINRME+2,C'I'                                                    
*                                                                               
         TM    SNVHDCTL,SNVHDPDQ   INVOICE IS PAID?                             
         BZ    *+8                                                              
         MVI   LINRME+4,C'P'                                                    
         DROP  R6                                                               
*                                                                               
LR90     XC    MINEKEY,MINEKEY     CALCULATE THE TOTAL OF THE DETAILS           
         MVI   MINEKEY,SNVIDELQ                                                 
         ZAP   PL16,TAXAMNT                                                     
         ZAP   FULL,=P'0'          NUMBER OF DETAILS                            
         NI    LSEQFLAG,X'FF'-(LSFINTQ+LSFCOSTQ)                                
*                                                                               
         BRAS  RE,MINIOHI                                                       
         BE    LR90LOOP                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    LR90X                                                            
         DC    H'0'                                                             
*                                                                               
LR90LOOP L     R6,MINELEM          SUM UP THE DETAIL AMOUNTS FROM THE           
         CLI   0(R6),SNVIDELQ                                                   
         BE    LR91                                                             
*                                                                               
         CLI   0(R6),X'F1'                                                      
         BNE   LR90NEXT                                                         
*                                                                               
         USING ACTVD,R6                                                         
         OC    ACTVCHDT,ACTVCHDT   ANY CHANGE DATE?                             
         BZ    LR90X                                                            
         CLI   EZFLAG,C'Y'         IGNORE ADD/CHA DATE CHECK FOR EASI           
         BE    *+14                                                             
         CLC   ACTVADDT,ACTVCHDT   COMAPRE CHANGED DATE TO ADDED DATE           
         BE    LR90X               NONE, NO CHANGE YET                          
         MVI   LINRME+3,C'C'       INDICATE A CHANGE WAS MADE                   
         B     LR90X                                                            
*                                                                               
         USING SNVIDELD,R6             DETAILS                                  
LR91     CLI   LINRME,C'R'         RESPONSE INVOICE?                            
         BNE   LR92                                                             
         ZICM  R1,SNVIDRSP,3                                                    
         B     LR94                                                             
*                                                                               
LR92     ICM   R1,15,SNVIDCST                                                   
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE AMOUNT?                             
         BZ    *+6                                                              
         LNR   R1,R1               YES                                          
*                                                                               
         OC    SNVIDCST,SNVIDCST   ANYTHING IN COST FIELD?                      
         BZ    *+8                                                              
         OI    LSEQFLAG,LSFCOSTQ                                                
*                                                                               
         OC    SNVIDINT,SNVIDINT   ANYTHING IN INT FIELD?                       
         BZ    *+8                                                              
         OI    LSEQFLAG,LSFINTQ                                                 
*                                                                               
LR94     CVD   R1,DUB                                                           
         AP    PL16,DUB                                                         
LR96     AP    FULL,=P'1'                                                       
*                                                                               
LR90NEXT BRAS  RE,MINIOSEQ                                                      
         BE    LR90LOOP                                                         
*                                                                               
LR90X    CLI   LINRME,C'R'         RESPONSE INVOICE?                            
         BNE   LR90X1                                                           
         EDIT  (P16,PL16),(15,LINTOT),COMMAS=YES,ZERO=NOBLANK                   
         B     LR90X2                                                           
LR90X1   EDIT  (P16,PL16),(15,LINTOT),2,ZERO=NOBLANK,FLOAT=-                    
*                                                                               
LR90X2   MVI   LINTOT+15,C'/'                                                   
         LA    RE,LINTOT+16                                                     
         EDIT  (P4,FULL),(4,0(RE)),ZERO=NOBLANK                                 
*                                                                               
LR90X3   DS    0H                  NO X'10' ELEMENT FIX                         
         LA    R3,LSEQNEXT                                                      
         LA    R2,LINNEXTL         R2 = A(NEXT LINE)                            
         LA    R0,LSTSELLH                                                      
         CR    R2,R0               DID WE GO BEYOND END OF LIST SCREEN?         
         BNH   *+8                                                              
         OI    MISCFLG1,MF1NDSCR   YES                                          
*                                                                               
LRAFMNIO XC    KEY,KEY                    AFTER INITMNIO, YOU HAVE TO           
         MVC   KEY(L'SVMASTKY),SVMASTKY   REESTABLISH THE KEY FOR THE           
         BAS   RE,XSPDHIGH                READ SEQ                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRNXTREC BAS   RE,XSPDSEQ                                                       
         CLI   DMCB+8,0                                                         
         BE    LRLOOP                                                           
         CLI   DMCB+8,X'80'        END-OF-FILE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRENDLST LA    R2,LSTSEL1H                                                      
         XC    SVMASTKY,SVMASTKY   START FROM BEGINNING NEXT TIME               
         BRAS  RE,DOSOONI2                                                      
         BNE   ENDOFLST            END OF LIST - SELECT OR HIT ENTER ..         
         GOTO1 ERREX2                                                           
*                                                                               
LRDISPLD LA    R2,LSTSEL1H         LIST DISPLAYED - SELECT OR HIT ENT..         
         BRAS  RE,DOSOONI2                                                      
         BNE   LSTDSPLD                                                         
         GOTO1 ERREX2                                                           
         DROP  R2,R3                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DATAMGR CALLS FOR XSPDIR AND XSPFIL                                           
***********************************************************************         
XSPDHIGH NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     XSPDRCT                                                          
*                                                                               
XSPDSEQ  NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
*                                                                               
XSPDRCT  DS    0H                                                               
         MVC   DMFILE,INVDIR                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         MVC   KEY(L'SNVKEY),SNVKEY                                             
         MVC   KEYSTATS,SNVDSTAT                                                
         MVC   KEYDSKAD,SNVDDA                                                  
         DROP  R6                                                               
*                                                                               
         CLI   DMCB+8,0            NO ERRORS?                                   
         BE    XSPD10                                                           
         TM    DMCB+8,X'82'        EOF OR RECORD DELETED?                       
         BNZ   XSPD10                                                           
         DC    H'0'                NO, THEN DIE                                 
*                                                                               
XSPD10   DS    0H                                                               
*                                                                               
XSPDX    B     XIT                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
*                                                                               
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         MVI   GETMSYS,23                                                       
         LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         B     ERREXIT                                                          
*                                                                               
PFERR2   MVC   GERROR,=AL2(311)    CURSOR MUST BE ON LIST LINE                  
         MVI   GETMSYS,23                                                       
         LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         B     ERREXIT                                                          
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDTFMT    INVALID DATE EXPRESSION                      
         OI    GENSTAT2,USMYERSY                                                
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
*                                                                               
NEEDMDIA MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
LSTDSPLD MVI   GERROR1,LSTDISPL    LIST DISPLAYED - SELECT OR HIT ENT..         
         B     INFEXIT                                                          
*                                                                               
ENDOFLST MVI   GERROR1,ENDOLIST    END OF LIST - SELECT OR HIT ENTER ..         
         B     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         MVI   GETMSYS,255         INFO MESSAGES HERE ARE FROM THIS SYS         
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE LIST                                      
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* INVOICE ADD                                                                   
         DC    AL1(PF01X-*,01,0,(PF01X-PF01)/KEYLNQ,0)                          
         DC    CL3' ',CL8'INVOICE',CL8'ADD'                                     
PF01     DC    AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTCLT-1),AL2(LSTCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T210FFD)                     
PF01X    EQU   *                                                                
*                                                                               
* INVOICE DISPLAY DRIVER                                                        
         DC    AL1(PF02X-*,02,0,0,PFTRETRN)                                     
         DC    CL3'S',CL8' ',CL8' '                                             
PF02X    EQU   *                                                                
*                                                                               
* INVOICE CHANGE DRIVER                                                         
         DC    AL1(PF03X-*,03,0,0,PFTRETRN)                                     
         DC    CL3'C',CL8' ',CL8' '                                             
PF03X    EQU   *                                                                
*                                                                               
* DETAIL UPDATE DRIVER                                                          
         DC    AL1(PF04X-*,04,0,0,PFTRETRN)                                     
         DC    CL3'D',CL8' ',CL8' '                                             
PF04X    EQU   *                                                                
*                                                                               
* INVOICE MATCH DRIVER                                                          
         DC    AL1(PF05X-*,05,0,0,PFTRETRN)                                     
         DC    CL3'M',CL8' ',CL8' '                                             
PF05X    EQU   *                                                                
*                                                                               
* INVOICE REQUEST DRIVER                                                        
         DC    AL1(PF10X-*,10,0,0,PFTRETRN)                                     
         DC    CL3'R',CL8' ',CL8' '                                             
PF10X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
* SPECIAL PFKEY TABLE AFTER CURDISP HAS BEEN ESTABLISHED                        
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* INVOICE DISPLAY                                                               
         DC    AL1(SPF02X-*,02,PFTCPROG,(SPF02X-SPF02)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'DISPLAY '                               
SPF02    DC    AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTCLT-1),AL2(LSTCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T210FFD)                     
         DC    AL1(KEYTYCUR,L'LINPER-1),AL2(LINPER-LINPER)                      
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINPER)                      
SPF02X   EQU   *                                                                
*                                                                               
* INVOICE CHANGE                                                                
         DC    AL1(SPF03X-*,03,PFTCPROG,(SPF03X-SPF03)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'CHANGE  '                               
SPF03    DC    AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTCLT-1),AL2(LSTCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T210FFD)                     
         DC    AL1(KEYTYCUR,L'LINPER-1),AL2(LINPER-LINPER)                      
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINPER)                      
SPF03X   EQU   *                                                                
*                                                                               
* DETAIL UPDATE                                                                 
         DC    AL1(SPF04X-*,04,PFTCPROG,(SPF04X-SPF04)/KEYLNQ,0)                
         DC    CL3' ',CL8'DETAIL ',CL8'UPDATE  '                                
SPF04    DC    AL1(KEYTYWS,L'KEYLINE-1),AL2(KEYLINE-MYAREAD)                    
SPF04X   EQU   *                                                                
*                                                                               
* INVOICE REQUEST                                                               
         DC    AL1(SPF10X-*,10,PFTCPROG,(SPF10X-SPF10)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'REQUEST '                               
SPF10    DC    AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTCLT-1),AL2(LSTCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'LSTSTA-1),AL2(LSTSTA-T210FFD)                     
SPF10X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
DOSOONI2 DS    0H                                                               
         CLI   PFKEY,11                                                         
         BNER  RE                                                               
*                                                                               
GENI2RQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,=C'SI2R'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'R'                               
         GOTO1 GETPRFIL,DMCB,(R2),PROFI2R                                       
         LA    R2,=C'SI2N'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'N'                               
         GOTO1 (RF),(R1),(R2),PROFI2N                                           
         LA    R2,=C'SI2X'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'X'                               
         GOTO1 (RF),(R1),(R2),PROFI2X                                           
*                                                                               
         MVC   SVMED,QMED                                                       
         CLI   NETPAKSW,C'N'       SET NETPAK SWITCH                            
         BNE   GENI2005                                                         
*                                                                               
         LA    R2,=C'SI2Z'                                                      
         NI    0(R2),X'FF'-X'40'    LOWERCASE 'Z'                               
         GOTO1 (RF),(R1),(R2),PROFI2Z                                           
         CLI   PI2ZSUB,C'Y'                                                     
         BNE   GENI2005                                                         
*                                                                               
         CLC   QMED,QSUBMED                                                     
         BE    GENI2005                                                         
         CLI   QSUBMED,C' '                                                     
         BNH   GENI2005                                                         
         MVC   QMED,QSUBMED                                                     
*                                                                               
GENI2005 DS    0H                                                               
         LA    R2,=C'S0I2'                                                      
         GOTO1 (RF),(R1),(R2),PROFI2                                            
         MVI   UPDFLAG,C'Y'                                                     
         CLI   PI2POST,C'Y'                                                     
         BE    *+8                                                              
         MVI   UPDFLAG,C'N'                                                     
*                                                                               
         MVC   QMED,SVMED                                                       
*                                                                               
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
*        L     R2,AIO3                                                          
         LA    R2,SPOOKBLK                                                      
         USING SPOOK,R2                                                         
*                                                                               
         XC    0(SPOOKXL,R2),0(R2)                                              
*                                                                               
         MVC   SPOOKUID,TWAORIG                                                 
         MVC   SPOOKDES,TWAORIG                                                 
         MVC   SPOOKAGY,AGENCY                                                  
*                                                                               
         LA    R0,SECBLK                                                        
         ST    R0,ASECBLK                                                       
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         MVC   SAVEPID,SPACES                                                   
         L     R1,ASECBLK                                                       
         LA    R1,(SECPID-SECD)(R1)                                             
*                                                                               
         CLC   0(L'SECPID,R1),SPACES                                            
         BNH   *+20                                                             
         CLC   =C'DDS',0(R1)                                                    
         BE    *+10                                                             
         MVC   SAVEPID,0(R1)                                                    
*                                                                               
         MVC   SPOOKDID,=C'SI2'                                                 
         CLC   SAVEPID,SPACES                                                   
         BNH   *+10                                                             
         MVC   SPOOKDID,SAVEPID                                                 
*                                                                               
         MVC   SPOOKSYS,=C'SP'                                                  
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   *+10                                                             
         MVC   SPOOKSYS,=C'NE'                                                  
*                                                                               
         MVC   SPOOKEOD,=C'I2'                                                  
         MVC   SPOOKJCL,=C'I2'                                                  
*                                                                               
         MVC   SPOOKXT,=C'XT='     INDICATE WE HAVE EXTENDED SPOOKD             
         MVC   SPOOKLH,=AL2(48)                                                 
         MVC   SPOOKDH,=AL2(24)                                                 
         MVI   SPOOKWEN,2          NON-UPDATIVE SOON                            
*                                                                               
         DROP  R2                                                               
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LH    R1,CURDISP                                                       
         AR    R1,RA                                                            
         LA    RF,LSTSEL1H                                                      
*                                                                               
         CR    R1,RF                                                            
         BL    GENI2SLX                                                         
*                                                                               
         SR    R1,RF                                                            
         LHI   RF,LINNEXTL-LINDSECT                                             
         DR    R0,RF                                                            
         MHI   R1,LSEQNEXT-LSEQNTRY                                             
         LA    R1,SEQLIST(R1)                                                   
*                                                                               
         OC    0(LSEQNEXT-LSEQNTRY,R1),0(R1)   ANY INVOICE HEADER?              
         BZ    GENI2SLX                                                         
*                                                                               
         MVC   BMOSS,(LSEQPERD-LSEQNTRY)(R1)                                    
         MVC   QINVOICE,(LSEQINVC-LSEQNTRY)(R1)                                 
         MVC   INTFLAG,(LSEQFLAG-LSEQNTRY)(R1)                                  
*                                                                               
         GOTO1 INITMNIO                                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         L     RF,MINELEM                                                       
         CLI   0(RF),SNVIDELQ                                                   
         BNE   GENI2NDX                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BRAS  RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    RCRDCTL,RCRDCTL                                                  
*                                                                               
         MVI   RCRDAREA,C' '                                                    
         MVC   RCRDAREA+1(L'RCRDAREA-1),RCRDAREA                                
         MVI   RCRDREC2,C' '                                                    
         MVC   RCRDREC2+1(L'RCRDREC2-1),RCRDREC2                                
*                                                                               
         MVC   RCRDCODE,=C'I2'                                                  
         MVC   RCRDOPT1+4(1),UPDFLAG                                            
         MVI   RCRDCONT,C'*'                                                    
         OI    RCRDCTL+15,X'12'           SET 2-CARD LINKED                     
*                                                                               
         MVC   RCRDINT,INTOPT                                                   
         CLI   INTOPT,C' '                                                      
         BH    GENI2007                                                         
*                                                                               
         TM    INTFLAG,LSFINTQ     ANY INTEGRATION CHARGES?                     
         BZ    GENI2007            NO - DON'T BOTHER WITH INT OPTIONS           
         MVI   RCRDINT,C'I'        INDICATE SOME INT CHARGES                    
         TM    INTFLAG,LSFCOSTQ    ANY NONZERO COST SPOTS?                      
         BO    GENI2007            NO                                           
         MVI   RCRDINT,C'O'        INDICATE INT-ONLY INVOICE                    
*                                                                               
GENI2007 DS    0H                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
         MVC   RCRDCLT,QCLT                                                     
         OC    RCRDCLT,SPACES                                                   
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDLEN,SNVHDLN3   HEADER WITH ALPHA PRODUCTS?                  
         BL    GENI2010            NO - PROCESS 1-BYTE BINARY PRODS             
*                                                                               
         CLC   SNVHDAP1,SPACES     PRODUCT IN THE HEADER?                       
         BNH   GENI2015            NO - USE POL                                 
*                                                                               
         MVC   RCRDPRD,SNVHDAP1                                                 
         MVC   RCRDPRD2,SNVHDAP2                                                
         OC    RCRDPRD2,SPACES                                                  
*                                                                               
         B     GENI2020            PROCEED TO ESTIMATE                          
*                                                                               
GENI2010 DS    0H                  BINARY PRODUCTS HERE                         
         CLI   SNVHDPRD,X'00'      PRODUCT IN THE HEADER?                       
         BE    GENI2015            NO - USE POL                                 
*                                                                               
         LA    R1,SNVHDPRD                                                      
         BRAS  RE,GETAPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCRDPRD,0(R1)                                                    
*                                                                               
         CLI   SNVHDPR2,X'00'      2ND PRODUCT PRESENT?                         
         BE    GENI2020            NO - PROCEED TO ESTIMATE                     
*                                                                               
         LA    R1,SNVHDPR2                                                      
         BRAS  RE,GETAPRD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCRDPRD2,0(R1)                                                   
         B     GENI2020            NO - PROCEED TO ESTIMATE                     
*                                                                               
GENI2015 DS    0H                  NO PRODUCT IN HEADER                         
         MVC   RCRDPRD,=C'ALL'                                                  
         CLI   PI2RPOL,C'A'                                                     
         BE    *+10                                                             
         MVC   RCRDPRD,=C'POL'                                                  
*                                                                               
GENI2020 DS    0H                                                               
         CLI   COUNTRY,C'C'         IF CANADIAN                                 
         BNE   GENI2025                                                         
         CLC   RCRDPRD,=C'POL'                                                  
         BNE   GENI2025                                                         
         MVC   RCRDPRD2,=C'YES'                                                 
*                                                                               
GENI2025 DS    0H                                                               
         CLI   NETPAKSW,C'Y'       IS IT A NETPAK INVOICE?                      
         BNE   GENI2080            NO - DON'T DO ANYTHING                       
*                                                                               
* NETPAK INVOICE HERE                                                           
         CLI   PI2NPOLN,C'P'                                                    
         BNE   GENI2030                                                         
         CLC   =C'POL',RCRDPRD                                                  
         BE    GENI2080                                                         
         MVI   RCRDOPT1+4,C'N'     NON-UPDATIVE                                 
         B     GENI2080                                                         
*                                                                               
GENI2030 DS    0H                                                               
         CLI   PI2NPOLN,C'A'                                                    
         BNE   GENI2080                                                         
         CLC   =C'ALL',RCRDPRD                                                  
         BE    GENI2080                                                         
         MVI   RCRDOPT1+4,C'N'     NON-UPDATIVE                                 
*                                                                               
GENI2080 DS    0H                  ESTIMATE HERE                                
         MVC   RCRDEST,=C'NO '                                                  
         CLI   SNVHDEST,0                                                       
         BE    GENI2100                                                         
         EDIT  (B1,SNVHDEST),(3,RCRDEST),FILL=0                                 
*                                                                               
GENI2100 DS    0H                  NO ESTIMATE                                  
         LA    R1,PI2NESTS                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    R1,PI2NESTN                                                      
*                                                                               
         CLC   =C'NO ',RCRDEST                                                  
         BE    GENI2300                                                         
*                                                                               
         CLI   0(R1),C'Y'                                                       
         BNE   *+8                                                              
         MVI   RCRDOPT1+4,C'N'     NON-UPDATIVE                                 
*                                                                               
GENI2300 DS    0H                                                               
         MVC   RCRDSTA,QSTA                                                     
*                                                                               
*        LA    R0,RCRDSTA+4                                                     
*        LA    R1,RCRDMED                                                       
*        BRAS  RE,SETMED                                                        
*        GOTO1 SETMED                                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         CLI   RCRDSTA,C'0'                                                     
         BL    *+8                                                              
         MVI   RCRDSTA+4,C'/'                                                   
*                                                                               
         MVC   WORK(2),MINMKEY+SNVKMOS-SNVKEY                                   
         XC    WORK(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(0,RCRDSDAT)                                
         MVC   RCRDSDAT+4(2),=C'  '                                             
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   RCRDAREA+56(1),QSUBMED QCOMPARE                                  
         B     GENI2400                                                         
*                                                                               
         CLI   BOOKOVR,C' '                                                     
         BNH   GENI2310                                                         
         CLI   BOOKOVR,X'FF'                                                    
         BE    GENI2400                                                         
         MVC   RCRDBK1(6),BOOKOVR                                               
         B     GENI2400                                                         
*                                                                               
GENI2310 DS    0H                  NO BOOK GIVEN                                
         CLI   PI2RHUT,C'N'        TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   RCRDBK1+4(2),=C'NO'                                              
         MVC   WORK(6),RCRDSDAT    USE MOS                                      
         CLI   WORK+4,C' '         IF FULL DATE GIVEN                           
         BNH   GENI2320                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK,6   ADD 6 DAYS TO ENSURE                    
*                                                                               
GENI2320 DS    0H                                                               
         PACK  DUB,WORK+2(2)   LOOK UP BOOK BASED ON MOS MONTH                  
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,PI2RHUT(R1)      BOOKS IN PROFI2R+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    GENI2400            NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   GENI2330                                                         
         MVC   RCRDBK1(4),=C'ACT '                                              
         B     GENI2400                                                         
*                                                                               
GENI2330 DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCRDBK1+2(2),DUB    SET MONTH                                    
         MVC   RCRDBK1(2),WORK     SET YEAR = MOS YEAR                          
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   GENI2360                                                         
*                                                                               
GENI2350 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                 
         MVC   RCRDBK1(2),WORK+6                                                
*                                                                               
GENI2360 DS    0H                                                               
         MVC   WORK(4),RCRDBK1                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   KEY+2,C'A'          ARB                                          
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         L     R3,AIO2                                                          
*                                  NOTE- USE IOAREA+200 BECAUSE                 
*                                       REQUEST BUILT AT IOAREA                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,(R3),DMWORK               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+14                                                             
*                                                                               
GENI2380 MVC   RCRDBK1(6),=C'      '   INVALID BOOK - SO SKIP                   
         B     GENI2400                                                         
*                                                                               
         CLC   KEY(5),0(R3)                                                     
         BE    GENI2400            BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   GENI2380            ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     GENI2350                                                         
*                                                                               
GENI2400 DS    0H                                                               
         MVC   RCRDRQTR(10),=CL10'AUTO I2 RQ'                                   
         CLC   SAVEPID,SPACES                                                   
         BNH   *+10                                                             
         MVC   RCRDRQTR(10),SAVEPID                                             
         DROP  R2                                                               
*                                                                               
*        L     R2,AIO3             SPOOK+REQUEST AREA                           
         LA    R2,SPOOKBLK                                                      
         LA    R2,SPOOKXL(R2)      A(RCRDD)                                     
         CLI   RCRDOPT1+4-RCRDD(R2),C'N'   NON-UPDATIVE?                        
         BE    GENI2500            DON'T GENERATE LOCKS                         
*                                                                               
* UPDATIVE REQUEST HERE                                                         
*                                                                               
         CLI   PROFI2N+3,C'Y'      ESTIMATE "NO" INVALID?                       
         BE    GENI2NQX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,(CXTRAINF-COMFACSD)(RF)                                    
         BZ    GENI2450            ALLOW IT                                     
*                                                                               
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BNZ   GENI2NUP                                                         
*                                                                               
GENI2450 DS    0H                                                               
*        L     R2,AIO3             A(SPOOK)                                     
         LA    R2,SPOOKBLK                                                      
         MVI   SPOOKWEN-SPOOK(R2),5 UPDATIVE SOON                               
         BRAS  RE,ADDLOCKS                                                      
         BNE   GENI2LKX                                                         
*                                                                               
GENI2500 DS    0H                                                               
         L     RF,ACOMFACS                                                      
         MVC   DMCB+8(4),CDATAMGR-COMFACSD(RF)                                  
         ST    RF,DMCB+12                                                       
         L     RF,CREQTWA-COMFACSD(RF)                                          
*        L     R2,AIO3                                                          
         LA    R2,SPOOKBLK                                                      
         GOTO1 (RF),DMCB,(5,(RA)),SPOOKXL(R2),,,(R2)                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'REPORT XXX,9999 WILL BE PROCESSED SOON'           
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   CONHEAD+7(3),2(RE)                                               
         LH    RF,6(RE)                                                         
         LA    R2,CONHEAD+11                                                    
         EDIT  (RF),(4,(R2)),ALIGN=LEFT                                         
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2NQX DS    0H                                                               
         MVC   CONHEAD(40),=C'REQUEST NOT GENERATED. CHECK I2N PROFILE'         
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2SLX DS    0H                                                               
         MVC   CONHEAD(40),=CL40'SELECT INVOICE TO REQUEST SOON I2'             
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2NDX DS    0H                                                               
         MVC   CONHEAD(40),=CL40'NO I2 GENERATED: THERE ARE NO DETAILS'         
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2LKX DS    0H                                                               
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2NUP DS    0H                                                               
         MVC   CONHEAD(36),=CL36'NOT AUTHORIZED TO UPDATE THIS SYSTEM'          
         OI    CONHEADH+6,X'80'                                                 
         B     GENI2QX                                                          
*                                                                               
GENI2QX  DS    0H                                                               
         LR    R0,RA                                                            
         AH    R0,CURDISP                                                       
         STCM  R0,15,ACURFORC                                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         JE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         LTORG                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         JE    YES                                                              
         J     NO                  OTHERWISE RETURN 'NO'                        
         LTORG                                                                  
*                                                                               
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         JE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         JE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ADDLOCKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* NINV RECORDS                                                                  
*                                                                               
         BRAS  RE,BLDNVLK                                                       
*                                                                               
* TEST NV LOCK - IF NOT AVAILABLE, STOP                                         
*                                                                               
ADDLK2   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKTESTQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK2                                                           
*                                                                               
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
* BUY RECORDS                                                                   
*                                                                               
         BRAS  RE,TSTBALK          TEST FOR BUY ALLOCATION LOCKS                
         BNE   ADDLKERR                                                         
*                                                                               
* ADD BUY LOCK                                                                  
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+12                                                             
         BRAS  RE,BLDUNLK          YES/UNIT LOCK                                
         B     *+8                                                              
*                                                                               
         BRAS  RE,BLDBULK                                                       
*                                                                               
ADDLK4   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKLOCKQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK4                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         BRAS  RE,BLDNVLK                                                       
*                                                                               
ADDLK6   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('LKLOCKQ',IFLD),ACOMFACS                              
         CLI   4(R1),2                                                          
         BE    ADDLK6                                                           
         CLI   4(R1),0                                                          
         BE    ADDLKOK                                                          
*                                                                               
ADDLKOK  J     YES                                                              
*                                                                               
ADDLKERR J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BLDNVLK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
*        L     R2,AIO3                                                          
         LA    R2,SPOOKBLK                                                      
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'NV'    INVOICE RECORDS                              
         MVC   L.LKNVMED,RCRDMED                                                
         MVC   L.LKNVCLT,RCRDCLT                                                
         MVC   L.LKNVSTA,RCRDSTA                                                
         CLI   RCRDMED,C'X'        IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKNVSTA+4,C'X'    MAKE STATION X                               
*                                                                               
         CLI   COUNTRY,C'C'         IF CANADIAN                                 
         BNE   BLDNV10                                                          
*                                                                               
         CLI   L.LKNVSTA+4,X'40'                                                
         BH    BLDNVX                                                           
         MVC   L.LKNVSTA+4(1),RCRDMED                                           
         B     BLDNVX                                                           
*                                                                               
BLDNV10  CLI   L.LKNVSTA+4,X'40'   ELSE                                         
         BH    *+8                                                              
         MVI   L.LKNVSTA+4,C'T'    USE T                                        
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   BLDNVX                                                           
*                                                                               
         CLI   RCRDSTA+4,X'40'     AND NO NETWORK INDICATOR                     
         BH    BLDNVX                                                           
         MVI   L.LKNVSTA+4,C'N'    MAKE IT N                                    
*                                                                               
BLDNVX   J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDBULK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
*        L     R2,AIO3                                                          
         LA    R2,SPOOKBLK                                                      
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'BU'    BUY RECORDS                                  
         MVC   L.LKBUMED,RCRDMED                                                
         MVC   L.LKBUCLT,RCRDCLT                                                
         MVC   L.LKBUSTA,RCRDSTA                                                
                                                                                
         CLI   RCRDMED,C'X'        IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKBUSTA+4,C'X'    MAKE STATION X                               
                                                                                
         CLI   COUNTRY,C'C'        IF CANADIAN                                  
         BNE   BLDBU10                                                          
*                                                                               
         CLI   L.LKBUSTA+4,X'40'                                                
         BH    BLDBX                                                            
         MVC   L.LKBUSTA+4(1),RCRDMED                                           
         B     BLDBX                                                            
*                                                                               
BLDBU10  CLI   L.LKBUSTA+4,X'40'                                                
         BH    *+8                                                              
         MVI   L.LKBUSTA+4,C'T'                                                 
*                                                                               
BLDBX    J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
BLDUNLK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
*        L     R2,AIO3                                                          
         LA    R2,SPOOKBLK                                                      
         LA    R2,SPOOKXL(R2)                                                   
         USING RCRDD,R2                                                         
*                                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'UN'    UNIT RECORDS                                 
         MVC   L.LKUNCLT,RCRDCLT                                                
         MVC   L.LKUNSTA,RCRDSTA                                                
*                                                                               
BLDUNX   J     YES                                                              
         LTORG                                                                  
         DROP  L                                                                
         DROP  R2                                                               
*                                                                               
* TEST FOR BUY ALLOCATION LOCKS                                                 
*                                                                               
TSTBALK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
*        L     R3,AIO3                                                          
         LA    R3,SPOOKBLK                                                      
         LA    R3,SPOOKXL(R3)                                                   
         USING RCRDD,R3                                                         
*                                                                               
         LA    R2,FULL             R2=A(MEDIA LIST FOR LOCK TESTS)              
         MVC   0(L'RCRDMED,R2),RCRDMED ALWAYS TEST FOR REQUESTED MEDIA          
         LA    R0,1                (R0=MEDIA LOOP COUNT)                        
         CLI   COUNTRY,C'C'                                                     
         BNE   TSTBALK4                                                         
*                                                                               
         CLI   RCRDMED,C'C'        TEST CANADIAN COMBINED MEDIA                 
         BNE   TSTBALK2                                                         
         MVI   1(R2),C'N'          YES - TEST FOR NETWORK                       
         MVI   2(R2),C'T'          AND TELEVISION ALSO                          
         LA    R0,3                                                             
         B     TSTBALK4                                                         
*                                                                               
TSTBALK2 CLI   RCRDMED,C'T'        TEST CANADIAN TELEVISION                     
         BE    *+12                                                             
         CLI   RCRDMED,C'N'        OR CANANDIAN NETWORK                         
         BNE   TSTBALK4                                                         
         MVI   1(R2),C'C'          YES - TEST FOR COMBINED MEDIA ALSO           
         LA    R0,2                                                             
*                                                                               
TSTBALK4 XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RCRDAGY                                                
         MVC   L.LOCKRTY,=C'BA'    BUY ALLOCATION LOCK TYPE                     
         MVC   L.LOCKKEY,SPACES                                                 
         MVC   L.LKBAMED,0(R2)                                                  
         MVC   L.LKBACLT,RCRDCLT                                                
*                                                                               
* FOR 'ALL' ESTIMATE REQUEST THERE CAN'T BE ANY ESTIMATES LOCKED                
*                                                                               
         XC    L.LKBAEST,L.LKBAEST NULL TO TEST FOR ANY ESTIMATE                
         MVC   L.LKBAEST,RCRDEST   YES - TEST FOR SPECIFIC ESTIMATE             
*                                                                               
         BRAS  RE,TSTLOK           TEST FOR SPECIFIC/ALL ESTIMATES              
         BNE   TSTBNQX                                                          
         CLC   L.LKBAEST,SPACES    TEST ALL ESTIMATES CHECKED                   
         BE    TSTBALK8                                                         
         MVC   L.LKBAEST,SPACES                                                 
         BRAS  RE,TSTLOK           TEST FOR ALL ESTIMATES                       
         BNE   TSTBNQX                                                          
*                                                                               
TSTBALK8 LA    R2,1(R2)            BUMP TO NEXT MEDIA LETTER                    
         BCT   R0,TSTBALK4         DO FOR NUMBER OF MEDIAS                      
*                                                                               
TSTBQX   J     YES                                                              
TSTBNQX  J     NO                                                               
         LTORG                                                                  
         DROP  L                                                                
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
TSTLOK   NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD                                                      
*                                                                               
TSTLOK2  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LKTESTQ',L.LKKEYD),ACOMFACS                          
         CLI   4(R1),2                                                          
         BE    TSTLOK2                                                          
         CLI   4(R1),0                                                          
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         DROP  L                                                                
*                                                                               
********************************************************************            
* GETS ALPHA PRODUCT CODE FROM SVCLIST                                          
* R1 EXPECTED TO ADDRESS 1-BYTE BINARY PRODUCT CODE                             
* ON EXIT R1 ADDRESSES 3-CHAR ALPHA PRODUCT CODE + EQ EXIT CONDITION            
* OR UNEQUAL EXIT CONDITION IF NO PRODUCT FOUND                                 
********************************************************************            
GETAPRD  DS    0H                                                               
         LA    RF,SVCLIST                                                       
         LHI   R0,1024                                                          
         AR    R0,RF                                                            
*                                                                               
GETAP10  CLI   0(RF),C' '                                                       
         JL    GETAPNQX                                                         
         CLC   0(1,R1),3(RF)                                                    
         JE    GETAPQX                                                          
         LA    RF,4(RF)                                                         
         CR    RF,R0                                                            
         JL    GETAP10                                                          
*                                                                               
GETAPNQX LTR   RC,RC                                                            
         BR    RE                                                               
*                                                                               
GETAPQX  LR    R1,RF                                                            
         CR    RC,RC                                                            
         BR    RE                                                               
                                                                                
*                                                                               
*                                                                               
VALBOOK  NTR1  BASE=*,LABEL=*                                                   
         MVC   BOOKOVR,SPACES                                                   
*                                                                               
         CLC   =C'NO',22(R3)                                                    
         BNE   VALB10                                                           
         CLI   1(R3),2                                                          
         BNE   VALB10                                                           
         MVI   BOOKOVR,X'FF'                                                    
         B     VALBQX                                                           
*                                                                               
VALB10   DS    0H                                                               
         CLC   =C'ACT',22(R3)                                                   
         BNE   VALB20                                                           
         CLI   1(R3),3                                                          
         BNE   VALB20                                                           
         MVC   BOOKOVR(3),=C'ACT'                                               
         B     VALBQX                                                           
*                                                                               
VALB20   DS    0H                                                               
         GOTO1 DATVAL,DMCB,(2,22(R3)),WORK                                      
         CLC   =C'000000',WORK                                                  
         BE    VALBNQX                                                          
         MVI   WORK,C'0'                                                        
         MVC   BOOKOVR,WORK                                                     
         MVC   BOOKOVR+4(2),=C'  '                                              
         CLC   DMCB+3(1),1(R3)     ONLY DATE GIVEN?                             
         BE    VALB30              YES - NO HUT                                 
*                                                                               
         LA    RF,22(R3)                                                        
         A     RF,DMCB                                                          
         MVC   BOOKOVR+4(2),1(RF)                                               
         CLC   BOOKOVR+4(2),=C'NO'                                              
         BE    VALB30                                                           
         CLC   BOOKOVR+4(2),=C'00'                                              
         BNH   VALBNQX                                                          
         CLC   BOOKOVR+4(2),=C'12'                                              
         BH    VALBNQX                                                          
*                                                                               
VALB30   DS    0H                                                               
         MVC   WORK(4),BOOKOVR                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVI   KEY+2,C'N'          NIELSEN                                      
         CLI   SVCPROF+3,C'0'                                                   
         BE    VALB40                                                           
         CLC   WORK(2),=X'5E01'    JAN94+ ALWAYS NEILSEN                        
         BNL   *+8                                                              
         MVI   KEY+2,C'A'          ELSE ARB                                     
*                                                                               
         CLI   COUNTRY,C'C'        US AGENCY?                                   
         BE    VALB40              NO, CANADA - SKIP                            
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   KEY+2,C'A'          US RADIO STATIONS USE ARB                    
*                                                                               
VALB40   MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,KEYSAVE,DMWORK            
         TM    DMCB+8,X'FF'                                                     
         BNZ   VALBNQX                                                          
         CLC   KEY(5),KEYSAVE                                                   
         BNE   VALBNQX                                                          
*                                                                               
VALBQX   J     YES                                                              
VALBNQX  J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSNVWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENSNV          (RECORD DSECT)                               
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVFED         (OUR LIST SCREEN)                             
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAFACTS                                                                       
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDACTIVD                                                                      
* DDGLVXCTLD                                                                    
* DDMINBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE SPSNVRCRD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FALOCKUPD                                                      
       ++INCLUDE FAXTRAINF                                                      
*                                                                               
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LKBUMED  DS    XL1                                                              
LKBUCLT  DS    XL3                                                              
LKBUSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKBAMED  DS    CL1                                                              
LKBACLT  DS    CL3                                                              
LKBAEST  DS    CL3                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKNVMED  DS    XL1                                                              
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKUNCLT  DS    XL3                                                              
LKUNSTA  DS    XL4                                                              
         ORG   LOCKKEY                                                          
LKNBCLT  DS    XL3                                                              
LKNBEST  DS    XL3                                                              
LKNBNET  DS    XL4                                                              
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
* MY STORAGE AREA                                                               
*                                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
VLOCKET  DS    A                   A(LOCKET)                                    
*                                                                               
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               A KEY FIELD WAS CHANGED                      
MF1NDSCR EQU   X'40'               HIT END OF LIST SCREEN                       
*                                                                               
FILTFLG1 DS    XL1                                                              
FF1PERD  EQU   X'80'               FILTER ON PERIOD                             
FF1RSPNS EQU   X'40'                         RESPONSE INVOICES                  
FF1MCT   EQU   X'20'                         MIDFLIGHT CLEARENCE TRKING         
FF1EASI  EQU   X'10'                         EASI SOURCE INVOICES               
FF1NRSPN EQU   X'08'                     NON-RESPONSE INVOICES                  
FF1NMCT  EQU   X'04'                     NON-MIDFLIGHT CLEARENCE TRKING         
FF1NEASI EQU   X'02'                     NON-EASI SOURCE INVOICES               
*                                                                               
FILTFLG2 DS    XL1                                                              
FF2INVDT EQU   X'80'               FILTER ON INVOICE DATE                       
FF2DUEDT EQU   X'40'                         DUE DATE                           
FF2PRDCT EQU   X'20'                         PRODUCT                            
FF2ESTMT EQU   X'10'                         ESTIMATE                           
*                                                                               
EZFLAG   DS    C                                                                
IMFLAG   DS    C                                                                
INTFLAG  DS    C                                                                
*                                                                               
FILTIVDT DS    XL(L'SNVHDIDT)      FILTER INVOICE DATE                          
FILTDUDT DS    XL(L'SNVHDDDT)      FILTER DUE DATE                              
FILTBPRD DS    XL(L'BPRD)          FILTER BINARY PRIMARY PRODUCT                
FILTBPR2 DS    XL(L'BPRD)          FILTER BINARY PIGGYBACK PRODUCT              
FILTQPRD DS    CL(L'SNVHDAP1)      FILTER EBCDIC PRIMARY PRODUCT                
FILTQPR2 DS    CL(L'SNVHDAP2)      FILTER EBCDIC PIGGYBACK PRODUCT              
FILTBEST DS    XL(L'BEST)          FILTER BINARY ESTIMATE                       
*                                                                               
FILTSTDT DS    XL(L'SNVKMOS)       FILTER PERIOD START DATE                     
FILTNDDT DS    XL(L'SNVKMOS)              PERIOD END DATE                       
FILTINVC DS    CL(L'SNVKINV)              INVOICE                               
FILTINVL DS    XL1                        INVOICE LENGTH                        
*                                                                               
SAVEPID  DS    CL10                                                             
*                                                                               
PL16     DS    PL16                USED FOR AMOUNT TOTALLING                    
TAXAMNT  DS    PL8                 TAX IN INVOICE HEADER                        
BOOKOVR  DS    CL6                 BOOK OVERRIDE FOR SOON I2                    
INTOPT   DS    C                   INT/INTONLY OPTION FOR SOON I2               
*                                                                               
PROFI2   DS    CL16                I2  PROFILE                                  
PI2POST  EQU   PROFI2+1            POST AFFIDS TO BUY RECORDS?                  
*                                                                               
PROFI2Z  DS    CL16                I2Z PROFILE                                  
PI2ZSUB  EQU   PROFI2Z+6           RE-READ I2 BY SUBMEDIA                       
*                                                                               
PROFI2N  DS    CL16                I2N PROFILE                                  
PI2NPOLS EQU   PROFI2N+4           SPOT-PRD=POL UPDATIVE I2S ONLY               
PI2NESTS EQU   PROFI2N+5           SPOT-EST=NO UPDATIVE I2S ONLY                
PI2NPOLN EQU   PROFI2N+6           NET -PRD=POL UPDATIVE I2S ONLY               
PI2NESTN EQU   PROFI2N+7           NET -EST=NO UPDATIVE I2S ONLY                
*                                                                               
PI2XEAN  EQU   PROFI2X             EST=ALL=NO                                   
*                                                                               
SVMED    DS    C                                                                
UPDFLAG  DS    C                                                                
*                                                                               
KEYSAVED DS    XL(L'KEY)           SAVED KEY                                    
KEYSTATS DS    XL(L'SNVDSTAT)            DIRECTORY STATUS                       
KEYDSKAD DS    XL(L'SNVDDA)              DISK ADDRESS                           
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE                               
*                                                                               
KEYLINE  DS    XL(L'WORK)          KEYLINE FOR THE DETAIL SCREEN                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
*                                                                               
IFLDH    DS    CL8                                                              
IFLD     DS    CL30                                                             
IFLDCNT  DS    CL30                                                             
*                                                                               
SEQLIST  DS    XL((LSTLINES+1)*(LSEQNEXT-LSEQNTRY))                             
*                                                                               
LSTLINES EQU   ((LSTSELLH-LSTSEL1H)/(LINNEXTL-LINDSECT))+1                      
*                                                                               
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SPOOKBLK DS    CL(SPOOKXL)         EXTENDED SPOOK BLOCK                         
RCRDBLK  DS    CL(L'RCRDCTL+L'RCRDAREA+L'RCRDREC2) REQ. CARD BLOCK              
*                                                                               
         EJECT                                                                  
*****                                                                           
* LIST LINE DSECT                                                               
*****                                                                           
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'LSTSEL1H)                                                   
LINSEL   DS    CL(L'LSTSEL1)                                                    
LINPERH  DS    CL(L'LSTPER1H)                                                   
LINPER   DS    CL(L'LSTPER1)                                                    
LININVH  DS    CL(L'LSTINV1H)                                                   
LININV   DS    CL(L'LSTINV1)                                                    
LINPRDH  DS    CL(L'LSTPRD1H)                                                   
LINPRD   DS    CL(L'LSTPRD1)                                                    
LINESTH  DS    CL(L'LSTEST1H)                                                   
LINEST   DS    CL(L'LSTEST1)                                                    
LINDATH  DS    CL(L'LSTDAT1H)                                                   
LINDAT   DS    CL(L'LSTDAT1)                                                    
LINDDTH  DS    CL(L'LSTDDT1H)                                                   
LINDDT   DS    CL(L'LSTDDT1)                                                    
LINRMEH  DS    CL(L'LSTRME1H)                                                   
LINRME   DS    CL(L'LSTRME1)                                                    
LINTOTH  DS    CL(L'LSTTOT1H)                                                   
LINTOT   DS    CL(L'LSTTOT1)                                                    
LINNEXTL DS    0C                                                               
*****                                                                           
* LIST LINE SEQUENCE DSECT                                                      
*****                                                                           
LSEQNTRY DSECT                                                                  
LSEQPERD DS    XL(L'SNVKMOS)                                                    
LSEQINVC DS    CL(L'SNVKINV)                                                    
LSEQFLAG DS    X                                                                
LSFINTQ  EQU   X'01'               AT LEAST 1 INTEGRATION SPOT PRESENT          
LSFCOSTQ EQU   X'02'               AT LEAST 1 SPOT WITH NONZERO COST            
LSEQNEXT DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068SPSNV01   06/07/16'                                      
         END                                                                    
