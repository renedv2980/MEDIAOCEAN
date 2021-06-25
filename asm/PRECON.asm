*          DATA SET PRECON     AT LEVEL 020 AS OF 03/26/15                      
*PHASE PRECONA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PUBED                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE GETINS                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM'                                 
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   BPLA 9/96   OMIT BLKSIZE FROM RECOVERY DCB (WAS 8504)                       
*                                                                               
*   BPLA 6/96   USE DATCON INSTEAD OF DTCNV                                     
*                                                                               
         PRINT NOGEN                                                            
PRECON   CSECT                                                                  
         NBASE 0,*PRECON*,=V(REGSAVE)                                           
*                                                                               
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING PRECON+4096,R8                                                   
*                                                                               
         L     RC,=V(PRECONWC)                                                  
         LA    RA,1(RC)                                                         
         LA    RA,4095(RA)                                                      
         USING PRECONWK,RC,RA                                                   
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
         EJECT                                                                  
*                                  FIRST TIME                                   
INITIAL  DS    0H                                                               
         L     RF,=A(SORTLOC)                                                   
         ST    RF,DMCB+8                                                        
         MVI   DMCB+8,45           K FOR SORTER                                 
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     INIT1                                                            
*                                                                               
*        **SORT ON KEY/DATE/REC NO.                                             
SORTCARD DC    CL80'SORT FIELDS=(69,25,A,39,6,A),FORMAT=BI'                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4200'                                  
*                                                                               
INIT1    DS    0H                                                               
         OPEN  (RECOV,(INPUT))                                                  
*                                                                               
         LA    RE,PRECONWK         CLEAR WORK                                   
         LH    RF,=Y(PRECONWX-PRECONWK)                                         
         XCEF                                                                   
*                                                                               
         MVC   LINE,=PL2'99'                                                    
         MVC   TITLE(20),=C'PRINTPAK RECONSTRUCT'                               
         MVI   FILTYP,0                                                         
*                                                                               
INIT2    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    INIT2F                                                           
*                                                                               
         CLC   =C'FILE=',CARD                                                   
         BNE   INIT2A                                                           
         CLC   =C'PRT',CARD+5                                                   
         BNE   *+12                                                             
**NEW 1/30/90          WAS X'12'                                                
         MVI   FILTYP,X'42'                                                     
         B     INIT2D                                                           
*                                                                               
         CLC   =C'PUB',CARD+5                                                   
         BNE   *+12                                                             
**NEW 1/30/90          WAS X'13'                                                
         MVI   FILTYP,X'43'                                                     
         B     INIT2D                                                           
*                                                                               
INIT2A   DS    0H                                                               
         MVC   P(80),CARD                                                       
         MVC   P+82(14),=C'**CARD ERROR**'                                      
         GOTO1 =V(PRINTER)                                                      
         B     OPD10                                                            
*                                                                               
INIT2D   DS    0H                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     INIT2                                                            
*                                                                               
INIT2F   DS    0H                                                               
         MVC   LINE,=PL2'99'                                                    
**NEW 1/30/90      WAS X'12'                                                    
         CLI   FILTYP,X'42'          TEST PRTFILE                               
         BNE   INIT4                                                            
         MVC   TITLE(19),=C'PRTFILE RECONSTRUCT'                                
         MVC   MID1(132),=C'  AG M CLT PRD PUB             DATE     ESTX        
                LN SIN   PR ACTION               GROSS         NET   CAX        
               SH DISC    PAID NET PAID ACTUAL  '                               
         MVC   MID2(132),=C'  -- - --- --- --------------- -------- ---X        
                -- ----- -- ------               -----         ---   --X        
               -------    -------- -----------  '                               
         B     INIT8                                                            
*                                                                               
INIT4    DS    0H                                                               
**NEW 1/30/90            WAS X'13'                                              
         CLI   FILTYP,X'43'                                                     
         BNE   INIT6                                                            
         MVC   TITLE(19),=C'PUBFILE RECONSTRUCT'                                
         MVC   MID1(132),=C'  M PUB             AG RC                  X        
                   SIN   PR ACTION                                     X        
                                                '                               
         MVC   MID2(132),=C'  - --------------- -- --                  X        
                   ---   -- ------                                     X        
                                                '                               
         B     INIT8                                                            
*                                                                               
INIT6    DS    0H                                                               
         MVC   P(33),=C'**PRTFILE/PUBFILE NOT SPECIFIED**'                      
         GOTO1 =V(PRINTER)                                                      
         B     OPD10                                                            
         DC    H'0'                                                             
*                                                                               
INIT8    DS    0H                                                               
         MVI   DCSW,0                                                           
         MVI   LASTTY,0                                                         
         MVI   CCERR,0                                                          
         XC    CRECIN,CRECIN                                                    
         EJECT                                                                  
*                                  INPUT ROUTINE                                
         SPACE 2                                                                
INPUT    DS    0H                                                               
IN4      DS    0H                                                               
         LA    R1,RECOV                                                         
         LA    R0,RECVHDR-4                                                     
         GET   (1),(0)                                                          
*                                                                               
         L     R1,CRECIN                                                        
         LA    R1,1(R1)                                                         
         ST    R1,CRECIN                                                        
         CLC   RFILTY,FILTYP       TEST RIGHT FILE                              
         BNE   IN4                                                              
         LH    R1,RECVHDR-4                                                     
         LA    R1,RECVHDR-4(R1)                                                 
         MVI   0(R1),0             EOR                                          
         CLI   RRECTY,3                                                         
         BE    ADD                                                              
         CLI   RRECTY,2                                                         
         BE    CHG                                                              
         CLI   RRECTY,1                                                         
         BE    CPY                                                              
         B     IN4                                                              
*                                                                               
ADD      DS    0H                                                               
         CLI   LASTTY,1                                                         
         BNE   *+8                                                              
         BAS   R9,CPYERR                                                        
         MVI   LASTTY,3                                                         
         XC    ACTWRK,ACTWRK                                                    
**NEW 1/30/90      WAS X'12'                                                    
         CLI   FILTYP,X'42'                                                     
         BNE   ADD2                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   ADD2                                                             
*                                                                               
         LA    R2,RKEY                                                          
         BAS   R9,GTINS                                                         
         MVC   ACTAMTS,WORK                                                     
*                                                                               
ADD2     DS    0H                                                               
         MVC   ATRANS,=C'A '                                                    
         B     IN50                                                             
*                                                                               
CPY      DS    0H                                                               
         CLI   LASTTY,1                                                         
         BNE   *+8                                                              
         BAS   R9,CPYERR                                                        
         MVI   LASTTY,1                                                         
         XC    SAVWRK,SAVWRK                                                    
**NEW 1/30/90          WAS X'12'                                                
         CLI   FILTYP,X'42'        TEST PRTFILE                                 
         BNE   CPY2                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   CPY2                                                             
*                                                                               
         LA    R2,RKEY                                                          
         BAS   R9,GTINS                                                         
         MVC   SAVAMTS,WORK                                                     
*                                                                               
CPY2     DS    0H                                                               
         MVC   SAVKEY(31),RKEY                                                  
         MVC   SAVSIN,RSIN                                                      
         B     IN4                                                              
*                                                                               
CHG      DS    0H                                                               
         OC    RSIN,RSIN           TEST OFF-LINE                                
         BZ    CHG1                YES                                          
         CLI   LASTTY,1                                                         
         BE    CHG1                                                             
         BAS   R9,CHGERR                                                        
         B     IN4                                                              
CHG1     DS    0H                                                               
         MVI   LASTTY,2                                                         
         XC    ACTWRK,ACTWRK                                                    
**NEW 1/30/90          WAS X'12'                                                
         CLI   FILTYP,X'42'          TEST PRTFILE                               
         BNE   CHG6                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   CHG6                                                             
         CLI   RPRG,0              TEST OFF-LINE                                
         BE    CHG6                                                             
*                                                                               
         LA    R2,RKEY                                                          
         BAS   R9,GTINS                                                         
         MVC   ACTAMTS,WORK                                                     
         TM    RKEY+27,X'80'                                                    
         BZ    CHG2                                                             
         TM    SAVKEY+27,X'80'                                                  
         BNZ   CHG2                                                             
*                                  DELETE                                       
         BAS   R9,CMPLO            COMPLEMENT ORDERED ONLY                      
         MVC   ACTAMTS,SAVAMTS                                                  
         MVC   ATRANS,=C'D '                                                    
         B     IN50                                                             
*                                                                               
CHG2     DS    0H                                                               
         OC    RSIN,RSIN           TEST OFF-LINE                                
         BZ    CHG4                YES - NO KEY CHG                             
         CLC   SAVKEY,RKEY                                                      
         BE    CHG4                                                             
*                                                                               
         MVI   DCSW,1                                                           
         MVC   ADATE,SAVKEY+16                                                  
         B     CHG6                                                             
*                                                                               
CHG4     DS    0H                                                               
         BAS   R9,SUBT                                                          
         XC    SAVALL,SAVALL                                                    
*                                                                               
CHG6     DS    0H                                                               
         MVC   ATRANS,=C'C '                                                    
         CLI   RPRG,3                                                           
         BNE   *+10                                                             
         MVC   ATRANS,=C'P '                                                    
         OC    ADATE,ADATE                                                      
         BZ    *+10                                                             
         MVC   ATRANS,=C'CF'                                                    
         B     IN50                                                             
*                                                                               
IN50     DS    0H                                                               
         LH    R4,RECVHDR-4                                                     
         LA    R4,L'REXP(R4)                                                    
         STH   R4,RREC-4                                                        
         XC    RREC-2(2),RREC-2                                                 
         MVC   REXP1,ACTWRK                                                     
         MVC   REXPRNO,CRECIN      RECORD NUMBER                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',RREC-4                                   
*                                                                               
         CLI   DCSW,0                                                           
         BE    IN4                                                              
*                                                                               
DCHG     DS    0H                  DATE CHANGES                                 
         BAS   R9,CMPLA            COMPLEMENT ALL                               
         MVC   ATRANS,=C'CT'                                                    
         MVC   ADATE,RKEY+16                                                    
         MVC   ACTAMTS,SAVAMTS                                                  
         MVC   RKEY(31),SAVKEY                                                  
         MVC   RECVHDR-4(2),=H'59'                                              
         MVI   DCSW,0                                                           
         XC    SAVALL,SAVALL                                                    
         B     IN50                                                             
INX      DS    0H                                                               
         OC    CRECIN,CRECIN       TEST ANY INPUT                               
         BNZ   OUTPUT                                                           
*                                                                               
         MVC   P(40),=C'**NO RECOVERY INPUT - RUN DISCONTINUED**'               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'     END SORT                             
*                                                                               
         ABEND 662                                                              
*                                                                               
         B     OPD8                SHOULD NEVER GET HERE                        
         SPACE 3                                                                
CPYERR   DS    0H                                                               
         MVC   P+1(20),=C'**STAND ALONE COPY**'                                 
         GOTO1 =V(HEXOUT),DMCB,SAVKEY,P+30,31,=C'NORM'                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   CCERR,1                                                          
         XC    P,P                                                              
         BR    R9                                                               
         SPACE 2                                                                
CHGERR   DS    0H                                                               
         MVC   P+1(27),=C'**CHG NOT PRECEDED BY CPY**'                          
         GOTO1 =V(HEXOUT),DMCB,RKEY,P+30,31,=C'NORM'                            
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   CCERR,1                                                          
         XC    P,P                                                              
         BR    R9                                                               
         EJECT                                                                  
*                                  OUTPUT ROUTINES                              
         SPACE 2                                                                
OUTPUT   DS    0H                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
         XC    CLTTOTS,CLTTOTS                                                  
         XC    AGYTOTS,AGYTOTS                                                  
         XC    SAVKEY,SAVKEY                                                    
         XC    RKEY,RKEY                                                        
         XC    SAVKEY(31),SAVKEY                                                
         MVI   BUYACT,0                                                         
         BAS   R9,RDTAP                                                         
*                                                                               
OP2      DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         MVC   NEXTKEY,=25X'FF'                                                 
         B     OP2A                                                             
*                                                                               
         MVC   NEXTKEY,68(R3)                                                   
OP2A     DS    0H                                                               
OP3      DS    0H                                                               
         CLC   FKEY,RKEY                                                        
         BL    OP6                                                              
         BH    OP8                                                              
         CLI   FKEY,X'FF'                                                       
         BE    OPDONE                                                           
         B     OP8                                                              
OP6      DS    0H                                                               
         LA    R2,FKEY-4                                                        
         BAS   R9,PUT                                                           
         BAS   R9,RDTAP                                                         
         B     OP3                                                              
*                                                                               
OP8      DS    0H                                                               
         OC    RKEY,RKEY                                                        
         BZ    OP8A                                                             
         BAS   R9,FMTREC                                                        
         CLC   RKEY,NEXTKEY                                                     
         BNE   OP9                                                              
OP8A     DS    0H                                                               
         BAS   R9,MOVREC                                                        
         B     OP2                 GET NEXT RECOV REC                           
OP9      DS    0H                                                               
         CLC   ATRANS,=C'CT'                                                    
         BE    OP10                                                             
         LH    R2,RREC-4                                                        
         SH    R2,=Y(L'REXP+24)                                                 
         STH   R2,RKEY-4                                                        
         XC    RKEY-2(2),RKEY-2                                                 
         LA    R2,RKEY-4                                                        
         BAS   R9,PUT                                                           
         B     OP10                                                             
*                                                                               
OP10     DS    0H                                                               
         CLC   FKEY,RKEY                                                        
         BNE   *+8                                                              
         BAS   R9,RDTAP                                                         
         BAS   R9,MOVREC                                                        
         CLI   RKEY,X'FF'                                                       
         BE    OP3                                                              
         B     OP2                 GET NEXT RECOV REC                           
         SPACE 3                                                                
*                                  DONE                                         
OPDONE   DS    0H                                                               
         BAS   R9,PRT                                                           
         BAS   R9,PRT                                                           
         LA    R2,COUNTS                                                        
OPD2     DS    0H                                                               
         EDIT  (P5,0(R2)),(11,P+2),COMMAS=YES                                   
*                                                                               
         MVC   P+15(33),7(R2)                                                   
         BAS   R9,PRT                                                           
         LA    R2,40(R2)                                                        
         C     R2,=A(COUNTSX)                                                   
         BL    OPD2                                                             
         ZAP   DUB,CIN                                                          
         AP   DUB,CADD                                                          
         SP    DUB,CDIROVS                                                      
         CP    DUB,COUT                                                         
         BE    OPD4                                                             
         MVC   P+2(L'WARN),WARN                                                 
         BAS   R9,PRT                                                           
OPD4     DS    0H                                                               
         CLI   CCERR,0                                                          
         BE    OPD6                                                             
         MVC   P+2(49),=C'**THERE ARE COPY/CHG ERRORS - SEE START OF LIX        
               ST**'                                                            
*                                                                               
         BAS   R9,PRT                                                           
OPD6     DS    0H                                                               
         CLOSE (IN,,OUT,)                                                       
*                                                                               
OPD8     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
OPD10    DS    0H                                                               
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
*                                  TOTAL + PRINT RECORD                         
FMTREC   DS    0H                                                               
         ST    R9,SAVR9                                                         
         MVC   ACTWRK,REXP                                                      
**NEW 1/30/90           WAS X'12'                                               
         CLI   FILTYP,X'42'        TEST FOR PRTFILE                             
         BNE   FR15                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   FR10                                                             
*                                  FORMAT BUY RECORD                            
         MVI   BUYACT,X'C0'        CLT/AGY ACTIVITY                             
         LA    R2,RKEY                                                          
         USING PBUYRECD,R2                                                      
         MVC   P+2(2),PBUYKAGY                                                  
         MVC   P+5(1),PBUYKMED                                                  
         MVC   P+7(3),PBUYKCLT                                                  
         MVC   P+11(3),PBUYKPRD                                                 
         GOTO1 =V(PUBED),DMCB,PBUYKPUB,P+15                                     
*                                                                               
*******  GOTO1 =V(DTCNV),DMCB,(1,PBUYKDAT),(3,P+31)                             
         GOTO1 =V(DATCON),DMCB,(3,PBUYKDAT),(5,P+31)                            
*                                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(3),DUB                                                      
         SR    R0,R0                                                            
         IC    R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+44(2),DUB                                                      
         SPACE 3                                                                
FR4      DS    0H                                                               
         DROP  R2                                                               
         LA    R2,ACTAMTS                                                       
         BAS   R9,EDT                                                           
         SR    R2,R2                                                            
         LA    R0,5                                                             
FR4B     DS    0H                                                               
         L     R1,ACTAMTS(R2)                                                   
         A     R1,CLTTOTS(R2)                                                   
         ST    R1,CLTTOTS(R2)                                                   
         L     R1,ACTAMTS(R2)                                                   
         A     R1,AGYTOTS(R2)                                                   
         ST    R1,AGYTOTS(R2)                                                   
         LA    R2,4(R2)                                                         
         BCT   R0,FR4B                                                          
         B     FR20                                                             
         SPACE 3                                                                
*                                  NON-BUY RECORDS                              
FR10     DS    0H                                                               
*                                                                               
         MVC   P+56(1),ATRANS                                                   
         GOTO1 =V(HEXOUT),DMCB,RKEY,P+66,29,=C'NORM'                            
*                                                                               
         B     FR20                                                             
*                                                                               
*                                  PUB RECS                                     
FR15     DS    0H                                                               
         MVC   P+2(1),RKEY                                                      
         GOTO1 =V(PUBED),DMCB,RKEY+1,P+4                                        
*                                                                               
         MVC   P+20(2),RKEY+7                                                   
         GOTO1 =V(HEXOUT),DMCB,RKEY+9,P+23,1,=C'NORM'                           
*                                                                               
         B     FR20                                                             
*                                                                               
FR20     DS    0H                                                               
         L     R0,RSIN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+47(5),DUB                                                      
         GOTO1 =V(HEXOUT),DMCB,RPRG,P+53,1,=C'NORM'                             
*                                                                               
         MVC   P+56(1),ATRANS                                                   
         OC    ADATE,ADATE                                                      
         BZ    FR20B                                                            
*******  GOTO1 =V(DTCNV),DMCB,(1,ADATE),(3,P+61)                                
         GOTO1 =V(DATCON),DMCB,(3,ADATE),(5,P+61)                               
*                                                                               
         MVC   P+58(2),=C'TO'                                                   
         CLI   ATRANS+1,C'T'                                                    
         BE    *+10                                                             
         MVC   P+58(2),=C'FR'                                                   
*                                                                               
FR20B    DS    0H                                                               
         CLI   ATRANS,C'A'         'ADD' OF KEY ON FILE =                       
         BNE   FR20D               'DIRECTORY OVERLAY'                          
         CLC   SAVKEY,RKEY                                                      
         BNE   FR20B1                                                           
         TM    SAVKEY+27,X'80'                                                  
         BNZ   FR20C                                                            
         MVC   P+58(06),=C'******'                                              
         B     FR20D                                                            
FR20B1   DS    0H                                                               
         CLC   FKEY,RKEY                                                        
         BNE   FR20D                                                            
FR20C    DS    0H                                                               
         AP    CDIROVS,=P'1'                                                    
         MVC   P+58(6),=C'DIR OV'                                               
FR20D    DS    0H                                                               
         MVC   SAVKEY(31),RKEY                                                  
         BAS   R9,PRT                                                           
         LA    R2,COUNTS                                                        
FR21     DS    0H                                                               
         CLC   ATRANS,5(R2)                                                     
         BE    *+12                                                             
         LA    R2,40(R2)                                                        
         B     FR21                                                             
*                                                                               
         AP    0(5,R2),=P'1'                                                    
         L     R9,SAVR9                                                         
         BR    R9                                                               
         SPACE 3                                                                
EDT      DS    0H                                                               
         LA    R5,5                                                             
         LA    R6,P+71                                                          
EDT2     DS    0H                                                               
         EDIT  (B4,0(R2)),(12,0(R6)),2,MINUS=YES                                
*                                                                               
         LA    R6,12(R6)                                                        
         LA    R2,4(R2)                                                         
         BCT   R5,EDT2                                                          
         BR    R9                                                               
         SPACE 3                                                                
MOVREC   DS    0H                                                               
         CLI   NEXTKEY,X'FF'                                                    
         BNE   MOVREC2                                                          
         MVC   RKEY,NEXTKEY                                                     
         BR    R9                                                               
MOVREC2  DS    0H                                                               
         LH    R1,0(R3)                                                         
         LA    RF,RREC-4                                                        
         LR    RE,R3                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         BR    R9                                                               
         SPACE 3                                                                
RDTAP    CLC   FKEY,=25X'FF'                                                    
         BER   R9                                                               
         LA    R1,IN                                                            
         LA    R0,FREC-4                                                        
         GET   (1),(0)                                                          
*                                                                               
         AP    CIN,=P'1'                                                        
         BR    R9                                                               
         SPACE 3                                                                
EOIN     DS    0H                                                               
         MVC   FKEY,=25X'FF'                                                    
         BR    R9                                                               
         SPACE 3                                                                
PUT      DS    0H                                                               
         ST    R9,SAVR9                                                         
         PUT   OUT,(R2)                                                         
*                                                                               
         AP    COUT,=P'1'                                                       
**NEW 1/30/90         WAS X'12'                                                 
         CLI   FILTYP,X'42'        TEST FOR PRTFILE                             
         BNE   PUTX                                                             
         LA    R0,RKEY-4                                                        
         CR    R2,R0                                                            
         BNE   PUTX                                                             
         CLC   RKEY(7),NEXTKEY                                                  
         BE    PUTX                                                             
         TM    BUYACT,X'80'             TEST CLT BUY ACTIVITY                   
         BZ    PUT4                                                             
*                                  CLIENT BRK                                   
         MVC   P+2(2),RKEY                                                      
         MVC   P+5(1),RKEY+2                                                    
         MVC   P+7(3),RKEY+4                                                    
         MVC   P+11(17),=C'**CLIENT TOTALS**'                                   
         LA    R2,CLTTOTS                                                       
         BAS   R9,EDT                                                           
         XC    CLTTOTS,CLTTOTS                                                  
         NI    BUYACT,X'7F'                                                     
         BAS   R9,PRT                                                           
*                                                                               
PUT4     DS    0H                                                               
         CLC   RKEY(3),NEXTKEY                                                  
         BE    PUTX                                                             
         TM    BUYACT,X'40'        TEST AGY BUY ACTIVITY                        
         BZ    PUTX                                                             
         MVC   P+2(2),RKEY                                                      
         MVC   P+5(1),RKEY+2                                                    
         MVC   P+11(18),=C'**AGY/MED TOTALS**'                                  
         LA    R2,AGYTOTS                                                       
         BAS   R9,EDT                                                           
         XC    AGYTOTS,AGYTOTS                                                  
         NI    BUYACT,X'BF'                                                     
         BAS   R9,PRT                                                           
*                                                                               
PUTX     L     R9,SAVR9                                                         
         BR    R9                                                               
         SPACE 3                                                                
PRT      DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    P,P                                                              
         BR    R9                                                               
         SPACE 2                                                                
GTINS    DS    0H                                                               
         GOTO1 =V(GETINS),DMCB,(R2),PVALUES,7(R2)                               
*                                                                               
         L     R0,GROSS                                                         
         ST    R0,WORK                                                          
         S     R0,AGYCOM                                                        
         ST    R0,WORK+4                                                        
         L     R0,CSHDSC                                                        
         ST    R0,WORK+8                                                        
         L     R0,PGROSS                                                        
         S     R0,PCSHDSC                                                       
         ST    R0,WORK+12                                                       
         S     R0,PAGYCOM                                                       
         ST    R0,WORK+16                                                       
*                                                                               
         BR    R9                                                               
         SPACE 2                                                                
SUBT     DS    0H                                                               
         LA    RF,16                                                            
SUBT2    L     R0,ACTAMTS(RF)                                                   
         S     R0,SAVAMTS(RF)                                                   
         ST    R0,ACTAMTS(RF)                                                   
         SH    RF,=H'4'                                                         
         BMR   R9                                                               
         B     SUBT2                                                            
         SPACE 2                                                                
CMPLO    DS    0H                                                               
         LA    R0,3                3 FIELDS - ORDERED ONLY                      
         B     CMPL                                                             
CMPLA    DS    0H                                                               
         LA    R0,5                5 FIELDS                                     
CMPL     DS    0H                                                               
         LA    RF,SAVAMTS                                                       
CMPL2    L     R1,0(RF)                                                         
         LCR   R1,R1                                                            
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,CMPL2                                                         
         BR    R9                                                               
         SPACE 2                                                                
RECVEOT  DS    0H                                                               
         CLOSE (RECOV,)                                                         
         MVI   RKEY,X'FF'                                                       
         B     INX                                                              
         SPACE 2                                                                
COUNTS   DS    0C                                                               
CADD     DC    PL5'0',C'A ',CL33'ADDS'                                          
CDIROVS  DC    PL5'0',C'DO',CL33'DIRECTORY OVERLAYS'                            
CDEL     DC    PL5'0',C'D ',CL33'DELETES-(CHANGES)'                             
CPAY     DC    PL5'0',C'P ',CL33'PAYS -(CHANGES)'                               
CCHG     DC    PL5'0',C'C ',CL33'REGULAR CHANGES'                               
CCTO     DC    PL5'0',C'CT',CL33'DATE CHANGES -DROPPED'                         
CCFR     DC    PL5'0',C'CF',CL33'DATE CHANGES -ADDED'                           
CIN      DC    PL5'0',C'IP',CL33'INPUT FILE COUNT'                              
COUT     DC    PL5'0',C'OP',CL33'OUTPUT FILE COUNT'                             
COUNTSX  EQU   *-1                                                              
*                                                                               
WARN     DC    C'**WARNING-  INPUT + ADDS - DIRECTORY OVERLAYS NOT EQUAX        
               L TO OUTPUT**'                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
RECOV    DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08500,                                            X        
               MACRF=GM,                                               X        
               EODAD=RECVEOT                                                    
         SPACE 2                                                                
IN       DCB   DDNAME=FILEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08500,                                            X        
               MACRF=GM,                                               X        
               EODAD=EOIN                                                       
         SPACE 2                                                                
OUT      DCB   DDNAME=FILEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=07996,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
         SPACE 3                                                                
         EJECT                                                                  
PRECONWK DSECT                                                                  
FILTYP   DS    X                                                                
LASTTY   DS    X                   LAST REC TYPE                                
CCERR    DS    X                   COPY/CHG ERR                                 
SAVR3    DS    F                                                                
SAVR1    DS    F                                                                
SAVR9    DS    F                                                                
CRECIN   DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    CL100                                                            
*                                                                               
CARD     DS    CL80                                                             
         DS    0F                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
         DS    0F                                                               
CLTTOTS  DS    CL20                                                             
AGYTOTS  DS    CL20                                                             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
         DS    0F                                                               
ACTWRK   DS    0CL32                                                            
ACTAMTS  DS    0CL20                                                            
AGROSS   DS    F                                                                
ANET     DS    F                                                                
ACD      DS    F                                                                
APAIDN   DS    F                                                                
APAIDA   DS    F                                                                
ATRANS   DS    CL2                                                              
ADATE    DS    CL3                                                              
         DS    CL7                                                              
*                                                                               
         DS    0F                                                               
SAVALL   DS    0CL65                                                            
SAVWRK   DS    0CL32                                                            
SAVAMTS  DS    CL20                                                             
         DS    CL5                                                              
         DS    CL7                                                              
*                                                                               
SAVKEY   DS    CL25                                                             
         DS    CL6                                                              
SAVSIN   DS    CL4                                                              
BUYACT   DS    X                                                                
UPSI     DS    X                   '80' = PUB NOT PRT                           
DCSW     DS    X                                                                
NEXTKEY  DS    CL25                                                             
         DS    F                                                                
RREC     DS    0C                                                               
REXP     DS    0CL40                                                            
REXP1    DS    CL32                                                             
         DS    XL2                                                              
         DS    XL2                                                              
REXPRNO  DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    CL25                                                             
         DS    4000C                                                            
*                                                                               
         DS    F                                                                
FREC     DS    0C                                                               
FKEY     DS    CL25                                                             
         DS    4000C                                                            
PRECONWX EQU   *                                                                
         EJECT                                                                  
*                                                                               
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
*                                                                               
       ++INCLUDE KSDPRINT                                                       
*                                                                               
PRECONWC CSECT                                                                  
         DS    9000C                                                            
*                                                                               
SORTLOC  DS    45800C                                                           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PRECON    03/26/15'                                      
         END                                                                    
