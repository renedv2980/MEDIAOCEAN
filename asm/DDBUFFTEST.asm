*          DATA SET DDBUFFTEST AT LEVEL 004 AS OF 12/13/00                      
*PHASE BUFFTESA BUFFTEST                                                        
*INCLUDE BUFFERIN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
BUFFTEST TITLE '- PROGRAM TO TEST BUFFERIN'                                     
                                                                                
BUFFTEST CSECT                                                                  
         NBASE WORKL,**TEST**,WORK=V(REGSAVE)                                   
         USING WORKD,RC                                                         
BUFF     USING BUFFD,BUFF1                                                      
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(19),=C'Tester for BUFFERIN'                                
                                                                                
INIT02   GOTOR VCARDS,PARM,C,=C'RE00'                                           
         CLI   C,C'/'                                                           
         BE    ENDRUN                                                           
         MVC   P(L'C),C                                                         
         GOTOR =V(PRINTER)                                                      
         CLI   C,C'*'              IGNORE COMMENTS                              
         BE    INIT02                                                           
         CLI   C,C' '                                                           
         BE    INIT02                                                           
                                                                                
         CLC   =C'RUN',C           USER WANTS TO RUN NOW                        
         BE    TEST                                                             
                                                                                
         CLC   =C'DDSIO',C                                                      
         BNE   INIT04                                                           
         L     RF,VDDSIO                                                        
         MVC   0(8,RF),C+6                                                      
         B     INIT02                                                           
                                                                                
INIT04   CLC   =C'TYPE=',C                                                      
         BNE   INIT06                                                           
         CLI   C+5,C'D'            DATA                                         
         BNE   *+12                                                             
         MVI   BUFF.BUFFINDS,BUFFIDTA                                           
         B     INIT02                                                           
         CLI   C+5,C'P'            PACKED                                       
         BNE   *+12                                                             
         MVI   BUFF.BUFFINDS,BUFFIPAK                                           
         B     INIT02                                                           
         CLI   C+5,C'B'            BINARY                                       
         BNE   *+12                                                             
         MVI   BUFF.BUFFINDS,BUFFIBIN                                           
         B     INIT02                                                           
         DC    H'0'                                                             
                                                                                
INIT06   CLC   =C'KEYLEN=',C                                                    
         BNE   INIT08                                                           
         GOTOR GETNUM,C+7                                                       
         CH    R1,=Y(MAXKEY)                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,BUFF.BUFFLKEY                                               
         B     INIT02                                                           
                                                                                
INIT08   CLC   =C'COMLEN=',C                                                    
         BNE   INIT10                                                           
         GOTOR GETNUM,C+7                                                       
         CH    R1,=Y(MAXCOM)                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,BUFF.BUFFLCOM                                               
         B     INIT02                                                           
                                                                                
INIT10   CLC   =C'COLUMNS=',C                                                   
         BNE   INIT12                                                           
         TM    BUFF.BUFFINDS,BUFFIPAK+BUFFIBIN                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR GETNUM,C+8                                                       
         CH    R1,=Y(MAXCOLS)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,1,BUFF.BUFFNCOL                                               
         B     INIT02                                                           
                                                                                
INIT12   CLC   =C'BUFFERS=',C                                                   
         BNE   INIT14                                                           
         GOTOR GETNUM,C+8                                                       
         CH    R1,=Y(MAXBUFS)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,1,BUFF.BUFFNBUF                                               
         B     INIT02                                                           
                                                                                
INIT14   CLC   =C'NUMRECS=',C                                                   
         BNE   INIT16                                                           
         GOTOR GETNUM,C+8                                                       
         C     R1,=A(MAXRECS)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,NUMRECS                                                    
         B     INIT02                                                           
                                                                                
INIT16   CLC   =C'CYCLES=',C                                                    
         BNE   INIT18                                                           
         GOTOR GETNUM,C+7                                                       
         CH    R1,=Y(MAXCYCL)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,NUMCYCL                                                     
         B     INIT02                                                           
                                                                                
INIT18   CLC   =C'HEXPRT=',C                                                    
         BNE   INIT20                                                           
         MVC   HEXOPT,C+7                                                       
         B     INIT02                                                           
                                                                                
INIT20   CLC   =C'NDXPRT=',C                                                    
         BNE   INIT22                                                           
         MVC   NDXOPT,C+7                                                       
         B     INIT02                                                           
                                                                                
INIT22   DC    H'0'                                                             
                                                                                
TEST     ZAP   LINE,=P'99'                                                      
         GOTOR BUFFERIN,PARM,('BUFFAINI',BUFF1),RECORD,COMFACS                  
         GOTOR PRTSTA              PRINT RUN STATISTICS                         
         MVC   P+1(29),=C'*** Start of put sequence ***'                        
         GOTOR =V(PRINTER)                                                      
         LH    R5,NUMCYCL                                                       
                                                                                
TEST04   L     R7,NUMRECS                                                       
         LR    R2,R7                                                            
         LA    R3,1                                                             
         LA    R4,1                                                             
         XC    SAVETRK,SAVETRK                                                  
TEST10   STCM  R7,1,DUB                                                         
         TM    DUB,1                                                            
         BZ    *+16                                                             
         CVD   R3,DUB                                                           
         AHI   R3,2                                                             
         B     TEST20                                                           
         CVD   R2,DUB                                                           
         SHI   R2,2                                                             
TEST20   OI    DUB+L'DUB-1,X'0F'                                                
         LH    R1,BUFF.BUFFLKEY                                                 
         CH    R1,=H'16'                                                        
         BNH   TEST30                                                           
         LA    R0,RECORD                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R1,16                                                            
TEST30   BCTR  R1,0                                                             
         SLL   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         UNPK  RECORD(0),DUB                                                    
         LA    R6,RECORD                                                        
         AH    R6,BUFF.BUFFLKEY                                                 
         LH    R1,BUFF.BUFFLCOM                                                 
         LR    R0,R6                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,=X'FF'         FILL COMMENT WITH X'FF'S                     
         MVCL  R0,RE                                                            
         AH    R6,BUFF.BUFFLCOM                                                 
                                                                                
         TM    BUFF.BUFFINDS,BUFFIPAK+BUFFIBIN                                  
         BZ    TEST38                                                           
         SR    R0,R0                                                            
         ICM   R0,1,BUFF.BUFFNCOL                                               
                                                                                
         TM    BUFF.BUFFINDS,BUFFIPAK                                           
         BZ    TEST34                                                           
         CVD   R4,DUB                                                           
TEST32   ZAP   0(8,R6),DUB                                                      
         LA    R6,8(R6)                                                         
         BCT   R0,TEST32                                                        
         B     TEST38                                                           
                                                                                
TEST34   STCM  R4,15,0(R6)                                                      
         LA    R6,4(R6)                                                         
         BCT   R0,TEST34                                                        
                                                                                
TEST38   MVC   P+1(3),=C'ADD'      OUTPUT 'ADD RECORD KEY'                      
         CH    R5,NUMCYCL          TEST THIS IS THE FIRST CYCLE                 
         BE    *+10                                                             
         MVC   P+1(3),=C'UPD'      NO - CHANGE TO 'UPD RECORD KEY'              
         LH    R1,BUFF.BUFFLKEY                                                 
         CH    R1,=H'16'                                                        
         BNH   *+8                                                              
         LH    R1,=H'16'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   P+5(0),RECORD                                                    
         LH    R0,NUMCYCL                                                       
         SR    R0,R5                                                            
         AHI   R0,1                                                             
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   P+23(6),=C'Cycle#'                                               
         UNPK  P+29(5),DUB                                                      
         GOTOR =V(PRINTER)                                                      
                                                                                
         GOTOR BUFFERIN,PARM,('BUFFAPUT',BUFF1),RECORD,COMFACS                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAVETRK,BUFF.BUFFUTRK                                            
         BE    *+14                                                             
         MVC   SAVETRK,BUFF.BUFFUTRK                                            
         GOTOR PRTNDX                                                           
         LA    R4,1(R4)                                                         
         BCT   R7,TEST10           DO FOR NUMBER OF RECORDS                     
         BCT   R5,TEST04           DO FOR NUMBER OF CYCLES                      
         MVC   P+1(27),=C'*** End of put sequence ***'                          
         GOTOR =V(PRINTER)                                                      
                                                                                
         GOTOR PRTSTA              PRINT STATISTICS                             
                                                                                
         MVC   P+1(29),=C'*** Start of get sequence ***'                        
         GOTOR =V(PRINTER)                                                      
         XC    RECORD(256),RECORD                                               
         GOTOR BUFFERIN,PARM,('BUFFARDH',BUFF1),RECORD,COMFACS                  
         B     TEST42                                                           
TEST40   GOTOR BUFFERIN,PARM,('BUFFASEQ',BUFF1),RECORD,COMFACS                  
TEST42   BNE   TEST50                                                           
         LH    R1,BUFF.BUFFLKEY                                                 
         CH    R1,=H'16'                                                        
         BNH   *+8                                                              
         LH    R1,=H'16'                                                        
         BCTR  R1,0                                                             
         MVC   P+1(3),=C'SEQ'                                                   
         EX    R1,*+4                                                           
         MVC   P+5(0),RECORD                                                    
         GOTOR =V(PRINTER)                                                      
         B     TEST40                                                           
                                                                                
TEST50   MVC   P+1(27),=C'*** End of get sequence ***'                          
         GOTOR =V(PRINTER)                                                      
                                                                                
         GOTOR PRTNDX              PRINT THE TRACK INDEX                        
                                                                                
         GOTOR PRTSTA              PRINT FINAL STATISTICS                       
                                                                                
         ZAP   LINE,=P'99'         SET NEW PAGE                                 
         MVC   NUMCYCL,=H'1'       RESET DEFAULT CYCLE COUNT                    
         B     INIT02              GO BACK FOR MORE PUNISHMENT                  
                                                                                
ENDRUN   XBASE ,                                                                
         EJECT                                                                  
PRTNDX   NTR1  ,                                                                
         CLI   NDXOPT,C'Y'                                                      
         BNE   PRTNDXX                                                          
         GOTOR =V(PRINTER)                                                      
         MVC   P+1(27),=C'*** Start of page index ***'                          
         GOTOR =V(PRINTER)                                                      
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   R0,RE                                                            
         L     R3,BUFF.BUFFANDX                                                 
         SR    R1,R1                                                            
         ICM   R1,1,BUFF.BUFFNBUF                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MH    R1,=H'6'            R1=L'TRACK INDEX                             
         LA    R0,RECORD                                                        
         L     RE,BUFF.BUFFANDX                                                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    RE,*+10                                                          
         N     RE,=X'7FFFFFFF'                                                  
         BSM   R0,RE                                                            
         SR    R0,R0                                                            
         IC    R0,BUFF.BUFFNBUF                                                 
         LA    R3,RECORD                                                        
         ZAP   DUB,=P'1'                                                        
                                                                                
PRTNDX02 MVC   P+1(5),=C'Page#'                                                 
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+6(3),DUB                                                       
         MVC   P+10(10),=C'<<Unused>>'                                          
         OC    0(6,R3),0(R3)                                                    
         BZ    PRTNDX04                                                         
         MVC   P+10(6),=C'Track#'                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         CVD   R1,DUB2                                                          
         OI    DUB2+L'DUB2-1,X'0F'                                              
         UNPK  P+16(5),DUB2                                                     
         MVC   P+22(5),=C'Inds='                                                
         GOTOR =V(HEXOUT),PARM,2(R3),P+27,1,=C'N'                               
         MVC   P+30(7),=C'UCount='                                              
         ICM   R1,7,3(R3)                                                       
         CVD   R1,DUB2                                                          
         OI    DUB2+L'DUB2-1,X'0F'                                              
         UNPK  P+37(8),DUB2                                                     
                                                                                
PRTNDX04 GOTOR =V(PRINTER)                                                      
         AH    R3,=H'6'                                                         
         AP    DUB,=P'1'                                                        
         BCT   R0,PRTNDX02                                                      
         GOTOR =V(PRINTER)                                                      
                                                                                
         L     R3,BUFF.BUFFANDX                                                 
         SR    R0,R0                                                            
         IC    R0,BUFF.BUFFNBUF                                                 
         MH    R0,=H'6'            R0=L'BUFFER INDEX                            
         AR    R3,R0                                                            
         SR    R4,R4                                                            
         ICM   R4,3,BUFF.BUFFUTRK                                               
         BZ    PRTNDXX             R4=N'INDEX ENTRIES                           
         MVC   P+1(28),=C'*** Start of track index ***'                         
         GOTOR =V(PRINTER)                                                      
                                                                                
PRTNDX10 LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   R0,RE                                                            
         LH    R1,BUFF.BUFFLKEY                                                 
         SLL   R1,1                                                             
         AHI   R1,4                                                             
         LA    R0,RECORD                                                        
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    RE,*+10                                                          
         N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
                                                                                
         MVC   P+1(4),=C'Low='                                                  
         LA    R1,RECORD+4                                                      
         LH    RE,BUFF.BUFFLKEY                                                 
         CH    RE,=H'16'                                                        
         BNH   *+8                                                              
         LH    RE,=H'16'                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   P+5(0),0(R1)                                                     
         EX    RE,*+4                                                           
         TR    P+5(0),TRTAB                                                     
         AH    R1,BUFF.BUFFLKEY                                                 
         MVC   P+5+16+2(5),=C'High='                                            
         EX    RE,*+4                                                           
         MVC   P+5+16+7(0),0(R1)                                                
         EX    RE,*+4                                                           
         TR    P+5+16+7(0),TRTAB                                                
         MVC   P+59(5),=C'Recs='                                                
         SR    RE,RE                                                            
         ICM   RE,3,RECORD+2                                                    
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+64(5),DUB                                                      
         MVC   P+70(4),=C'Trk='                                                 
         ICM   RE,3,RECORD                                                      
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+74(5),DUB                                                      
         GOTOR =V(PRINTER)                                                      
         AH    R3,BUFF.BUFFLKEY                                                 
         AH    R3,BUFF.BUFFLKEY                                                 
         AHI   R3,4                                                             
         BCT   R4,PRTNDX10                                                      
         GOTOR =V(PRINTER)                                                      
                                                                                
PRTNDXX  XIT1  ,                                                                
                                                                                
PRTSTA   NTR1  ,                                                                
         GOTOR =V(PRINTER)                                                      
         MVC   P+1(20),=C'**** Statistics ****'                                 
         GOTOR =V(PRINTER)                                                      
                                                                                
         MVC   P+1(20),=C'Usage count.........'                                 
         SR    R0,R0                                                            
         ICM   R0,7,BUFF.BUFFUCNT                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'I/O count...........'                                 
         SR    R0,R0                                                            
         ICM   R0,3,BUFF.BUFFNIOS                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Tracks allocated....'                                 
         LH    R0,BUFF.BUFFNTRK                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Tracks used.........'                                 
         LH    R0,BUFF.BUFFUTRK                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Track size..........'                                 
         SR    R0,R0                                                            
         ICM   R0,3,BUFF.BUFFLTRK                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Track buffer size...'                                 
         L     R0,BUFF.BUFFLBUF                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Record size.........'                                 
         SR    R0,R0                                                            
         ICM   R0,3,BUFF.BUFFLREC                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Records per track...'                                 
         SR    R0,R0                                                            
         ICM   R0,3,BUFF.BUFFRPRT                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Total index size....'                                 
         L     R0,BUFF.BUFFLNDX                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Maximum records.....'                                 
         LH    R0,BUFF.BUFFNTRK                                                 
         MH    R0,BUFF.BUFFRPRT                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Number of records...'                                 
         L     R0,BUFF.BUFFNREC                                                 
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Buffers allocated...'                                 
         SR    R0,R0                                                            
         ICM   R0,1,BUFF.BUFFNBUF                                               
         GOTOR EDITPRNT                                                         
                                                                                
         MVC   P+1(20),=C'Storage allocated...'                                 
         SR    R1,R1                                                            
         IC    R1,BUFF.BUFFNBUF                                                 
         M     R0,BUFF.BUFFLBUF                                                 
         A     R1,BUFF.BUFFLNDX                                                 
         LR    R0,R1                                                            
         GOTOR EDITPRNT                                                         
         GOTOR =V(PRINTER)                                                      
                                                                                
PRTSTAX  XIT1  ,                                                                
                                                                                
EDITPRNT CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+21(8),DUB                                                      
         LR    R0,RE                                                            
         GOTOR =V(PRINTER)                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
GETNUM   SR    RF,RF                                                            
         LA    R0,8                MAX VALUES ARE 8 CHARACTERS                  
GETNUM02 CLI   0(R1),C' '          SEARCH FORWARD FOR A SPACE                   
         BE    GETNUM04                                                         
         TM    0(R1),X'F0'                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GETNUM02                                                      
         CLI   0(R1),C' '          MUST END IN A SPACE                          
         BE    *+6                                                              
         DC    H'0'                                                             
GETNUM04 LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R1,RF               POINT BACK TO THE START                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R1,DUB              RETURN VALUE IN R1                           
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
         ENTRY HEXOPT                                                           
HEXOPT   DC    C'N'                                                             
NDXOPT   DC    C'N'                                                             
SAVETRK  DC    XL2'00'                                                          
BUFFERIN DC    V(BUFFERIN)                                                      
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
                                                                                
NUMCYCL  DC    H'1'                                                             
NUMRECS  DC    F'0'                                                             
                                                                                
MAXCYCL  EQU   1024                                                             
MAXRECS  EQU   1000000                                                          
MAXKEY   EQU   255                                                              
MAXCOM   EQU   1000                                                             
MAXCOLS  EQU   255                                                              
MAXBUFS  EQU   255                                                              
                                                                                
COMFACS  DS    0F                                                               
         DC    A(DMGRHK)                                                        
                                                                                
         ENTRY BUFF1                                                            
BUFF1    BUFFD TYPE=B,COLUMNS=EQU0,KEYLEN=EQU0,COMLEN=EQU0,BUFFERS=EQU0         
                                                                                
EQU0     EQU   0                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DMGRHK   NMOD1 WORKL,**DMHK**                                                   
         LA    RE,*+10                                                          
         N     RE,=X'7FFFFFFF'     ENSURE 24-BIT MODE                           
         BSM   0,RE                                                             
                                                                                
         L     RA,=V(CPRINT)                                                    
         LR    R2,R1               SAVE PARAMETER LIST ADDRESS                  
         L     RE,0(R2)            DATAMGR COMMAND                              
         CLC   =C'DADDS',0(RE)                                                  
         BNE   DMGRHK20                                                         
         MVC   ACTION,7(R2)                                                     
         MVC   P+1(5),=C'WTCKD'                                                 
         CLI   ACTION,WTCKD                                                     
         BE    DMGRHK02                                                         
         MVC   P+1(5),=C'WTID '                                                 
         CLI   ACTION,WTID                                                      
         BE    DMGRHK02                                                         
         MVC   P+1(5),=C'RDID '                                                 
         CLI   ACTION,RDID                                                      
         BNE   DMGRHK20                                                         
                                                                                
DMGRHK02 L     RF,20(R2)           RF=A(DISK ADDRESS)                           
         CLI   ACTION,WTCKD                                                     
         BNE   DMGRHK04                                                         
         L     RF,=A(BUFF1)        WTCKD USES DNEXT VALUE                       
         LA    RF,BUFFFILE-BUFFD(RF)                                            
         LA    RF,DNEXT-DTFPHD(RF)                                              
                                                                                
DMGRHK04 MVC   P+7(4),=C'D/A='                                                  
         GOTOR =V(HEXOUT),PARM,(RF),P+11,4,=C'TOG'                              
         GOTOR =V(PRINTER)                                                      
         CLI   ACTION,RDID                                                      
         BE    DMGRHK20                                                         
                                                                                
         GOTOR HEXPRT                                                           
                                                                                
DMGRHK20 LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
         GOTOR =V(DATAMGR),(R2)                                                 
         CLI   ACTION,RDID                                                      
         BNE   DMGRHK22                                                         
                                                                                
         GOTOR HEXPRT                                                           
                                                                                
DMGRHK22 LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
                                                                                
DMGRHKX  MVC   P,SPACES                                                         
         XIT1  ,                                                                
         EJECT                                                                  
HEXPRT   NTR1  ,                                                                
         L     RF,=A(HEXOPT)                                                    
         CLI   0(RF),C'Y'                                                       
         BNE   HEXPRTX                                                          
         MVC   P+1(28),=C'*** Start of buffer print ***'                        
         GOTOR =V(PRINTER)                                                      
         LA    RE,*+10                                                          
         N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
         GOTOR =V(PRINTER)                                                      
         L     R1,=A(BUFF1)                                                     
         ICM   R0,3,BUFFRPRT-BUFFD(R1)                                          
         MH    R0,BUFFLREC-BUFFD(R1)                                            
         L     R3,8(R2)                                                         
                                                                                
HEXPRT02 LR    R4,R0                                                            
         CH    R4,=Y(HEXLEN)                                                    
         BNH   *+8                                                              
         LH    R4,=Y(HEXLEN)                                                    
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
         MVC   HEXWRK1,0(R3)                                                    
         LA    RE,*+10                                                          
         N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
         GOTOR =V(HEXOUT),PARM,HEXWRK1,HEXWRK2,(R4),=C'SEP'                     
         BCTR  R4,R0                                                            
         EX    R4,*+4                                                           
         MVC   P+1(0),HEXWRK1                                                   
         EX    R4,*+4                                                           
         TR    P+1(0),TRTAB                                                     
         GOTOR =V(PRINTER)                                                      
         EX    R4,*+4                                                           
         MVC   P+1(0),HEXWRK2                                                   
         GOTOR =V(PRINTER)                                                      
         LA    RE,HEXWRK2+1(R4)                                                 
         EX    R4,*+4                                                           
         MVC   P+1(0),0(RE)                                                     
         GOTOR =V(PRINTER)                                                      
         LA    R4,1(R4)                                                         
         AR    R3,R4                                                            
         SR    R0,R4                                                            
         BNZ   HEXPRT02                                                         
         MVC   P+1(26),=C'*** End of buffer print ***'                          
         GOTOR =V(PRINTER)                                                      
                                                                                
HEXPRTX  B     DMGRHKX                                                          
         EJECT                                                                  
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 00-0F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 10-1F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 20-2F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 30-3F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4A4B4C4D4E4F' 40-4F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B5A5B5C5D5E5F' 50-5F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B6A6B6C6D6E6F' 60-6F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B7A7B7C7D7E7F' 70-7F                        
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' 80-8F                        
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B' 90-9F                        
         DC    X'4B40E2E3E4E5E6E7E8E94B4B4B4B4B4B' A0-AF                        
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' B0-BF                        
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' C0-CF                        
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B' D0-DF                        
         DC    X'4B40E2E3E4E5E6E7E8E94B4B4B4B4B4B' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' F0-FF                        
                                                                                
         LTORG                                                                  
                                                                                
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
DUB      DS    D                                                                
DUB2     DS    D                                                                
PARM     DS    6F                                                               
C        DS    CL80                                                             
ACTION   DS    X                                                                
HEXLEN   EQU   L'P-3                                                            
HEXWRK1  DS    CL(HEXLEN)                                                       
HEXWRK2  DS    2CL(HEXLEN)                                                      
RECORD   DS    (MAXKEY+MAXCOM+(MAXCOLS*8))X                                     
WORKL    EQU   *-WORKD                                                          
                                                                                
       ++INCLUDE DDBUFFD                                                        
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
                                                                                
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDBUFFTEST12/13/00'                                      
         END                                                                    
