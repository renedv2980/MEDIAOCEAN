*          DATA SET SPREPXD02  AT LEVEL 006 AS OF 05/01/02                      
*PHASE SPXD02A                                                                  
*INCLUDE DEMTIME                                                                
         TITLE 'SPREPXD02 - DANCER PROGRAM/AFFILIATION REPORT'                  
SPXD02   CSECT                                                                  
         NMOD1 0,SPXD02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPXDRA                                                     
         LA    R2,2048(RB)         SECOND BASE REG                              
         LA    R2,2048(R2)                                                      
         USING SPXD02+4096,R2                                                   
         ST    R2,SPXDR2                                                        
         ST    R5,RELO                                                          
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         SPACE 2                                                                
         MVI   PAFIRST,1           INITIALIZE REPORTS                           
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         LA    R0,XDHDHOOK                                                      
         ST    R0,HEADHOOK                                                      
*                                                                               
         MVC   MEDLCHNK,=F'128'                                                 
         XC    MEDNUMWK,MEDNUMWK                                                
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   MEDEXTDM,2                                                       
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQEQUIV,C'Y'                                                     
         MVI   RCSUBPRG,1                                                       
         L     R4,=V(HRTOQH)                                                    
         A     R4,RELO                                                          
         ST    R4,VHRTOQH                                                       
         L     R4,=V(QHTOHR)                                                    
         A     R4,RELO                                                          
         ST    R4,VQHTOHR                                                       
         L     R4,=V(BUFFALOC)                                                  
         A     R4,RELO                                                          
         ST    R4,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R4)                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
         MVI   ESTSW,C'N'                                                       
         MVC   RQSTAFLT,QAFFIL     SET AFFILIATE FILTER                         
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(RF)                                      
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*                                                                               
         SPACE 2                                                                
* SET UP TO EXTRACT PRIMARY DEMOS AND IMPRESSIONS ONLY                          
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
M2SETD   CLI   0(RE),0                                                          
         BE    M2SETD2                                                          
         MVC   31(3,RE),28(RE)     SET FIRST DEMOS EQUAL                        
         MVI   29(RE),C'R'         SET FIRST TO RATING                          
         MVI   32(RE),C'I'         SET SECOND TO IMPRESSIONS                    
         XC    34(6,RE),34(RE)     CLEAR OUT SUBORDINATE DEMOS                  
M2SETD2  AH    RE,PRDBUFLN         NEXT PRODUCT                                 
         BCT   RF,M2SETD                                                        
         ZIC   RE,BPRD                                                          
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     R5,PRDBUFF                                                       
         LA    R5,28(RE,R5)                                                     
*                                                                               
         GOTO1 DEMOCON,DMCB,(2,0(R5)),(2,WORK),(C'S',ADBLOCK),         X        
               (SPOTPROF+9,SPUSRNMS)                                            
         LA    R8,2                                                             
         XC    DNAMES,DNAMES                                                    
         LA    R7,DNAMES                                                        
         LA    R9,WORK                                                          
GETDNAM  OC    0(3,R7),0(R5)                                                    
         BZ    M20B                END                                          
         MVC   3(7,R7),0(R9)                                                    
         LA    R9,7(R9)                                                         
         LA    R5,3(R5)                                                         
         LA    R7,10(R7)                                                        
         BCT   R8,GETDNAM                                                       
M20B     DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         CLI   Q2DPT,C' '                                                       
         BE    *+14                                                             
         CLC   Q2DPT,BDDAYPT                                                    
         BNE   EXIT                                                             
         MVC   CURRPROG,BDPROGT                                                 
         MVC   SVTIMES,BDPROG                                                   
         MVC   SVDAYS,BDDAY                                                     
         DROP  R6                                                               
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
         CLI   KEY+3,X'FF'         REQUEST FOR POL                              
         BNE   M31                                                              
         L     R6,ADBUY            YES - SUPPRESS PIGGYBACK LENGTHS             
         LA    R6,24(R6)                                                        
         USING BDELEM,R6                                                        
         MVI   PSLIST,X'FF'                                                     
         MVC   PSLIST+1(1),BDSEC                                                
         MVC   PSLIST+2(2),=X'FFFF' SET END OF LIST                             
         B     M32                                                              
         DROP  R6                                                               
M31      LA    RE,PSLIST                                                        
M31A     CLC   0(2,RE),=X'FFFF'    CHECK FOR END                                
         BE    M32                                                              
         CLC   0(1,RE),BPRD        PRODUCT OK                                   
         BNE   *+12                 NO - DELETE                                 
         LA    RE,2(RE)                                                         
         B     M31A                                                             
         XC    0(2,RE),0(RE)                                                    
         LA    RE,2(RE)                                                         
         B     M31A                                                             
*                                                                               
M32      LA    R7,2                                                             
         LA    R6,PSLIST                                                        
M323     CLC   0(2,R6),=X'FFFF'    END                                          
         BE    EXIT                                                             
         CLI   0(R6),0                                                          
         BNE   *+12                                                             
M323A    LA    R6,2(R6)                                                         
         B     M323                                                             
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R6)                                                   
         MVC   MEDSPTLN,1(R6)                                                   
MA322A   GOTO1 MEDGETBY,DMCB,(RA),(R7)                                          
*                                                                               
* EXTRACT AND POST ROUTINES                                                     
*                                                                               
         L     R5,MEDAFRST                                                      
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    EXIT                                                             
         SR    RF,RF                                                            
         L     RE,MEDBYD                                                        
         SRDA  RE,31                                                            
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,MEDBYD                                                        
         LA    R9,BFREC                                                         
         USING BF1RCDE,R9                                                       
         MVI   BF1RCDE,1                                                        
         MVC   BF1STA,CURRCALL                                                  
         MVC   BF1AFFL,CURRAFFL                                                 
         MVI   BF1RTYP,1                                                        
         MVC   BF1PROG,CURRPROG                                                 
         MVC   BF1BAND,CURRBAND                                                 
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFDOL,MEDBYD                                                     
         MVC   BFRTG,MEDBY1                                                     
         MVC   BFIMPS,MEDBY2                                                    
         BAS   R8,PUTBUF                                                        
         EJECT                                                                  
* SET UP TIME REPORT AND PUT TO BUFFALO                                         
         GOTO1 VHRTOQH,DMCB,(0,SVT1),STQH                                       
         GOTO1 VHRTOQH,DMCB,(0,SVT2),ENQH                                       
         CLC   ENQH,STQH           CHECK FOR END TIME PRESENT                   
         BNL   *+10                                                             
         MVC   STQH,ENQH            SET IF NOT THERE                            
         ZIC   RF,ENQH                                                          
         CLC   STQH,ENQH           SAME START AND END                           
         BNE   *+8                                                              
         LA    RF,1(RF)            ADJUST FOR END OF THIS QH                    
         ZIC   RE,STQH                                                          
         SR    RF,RE               CALCULATE DURATION                           
         LA    RF,1(RF)            FORCE TO ROUND UP                            
         SRL   RF,1                GET NUMBER OF HALF HOURS                     
         ST    RF,NUMHALF                                                       
         SR    RF,RF                                                            
         ICM   RE,15,BFDOL         GET DOLLARS                                  
         SRDA  RE,31               THEN DIVIDE BY NUMBER OF HALF HOURS          
         D     RE,NUMHALF                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,BFDOL                                                         
         SPACE 2                                                                
         SR    RF,RF                                                            
         ICM  RE,15,BFIMPS         GET IMPS                                     
         SRDA  RE,31               THEN DIVIDE BY NUMBER OF HALF HOURS          
         D     RE,NUMHALF                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,BFIMPS                                                        
         SPACE 2                                                                
         XC    BFKEY,BFKEY         PUT OUT 1 RECORD FOR EACH                    
         MVI   BF2RCDE,5           HALF HOUR                                    
         MVC   BF2AFFL,CURRAFFL                                                 
         MVC   BF2BAND,CURRBAND                                                 
         MVC   BF2DAY,SVDAYS                                                    
         MVI   BF2RTYP,X'01'                                                    
         ZIC   R6,STQH                                                          
         SRL   R6,1                FORCE TO HALF HOUR                           
         SLL   R6,1                                                             
         L     R7,NUMHALF                                                       
M3HHR    STC   R6,BF2STIM                                                       
         BAS   R8,PUTBUF                                                        
         BAS   R8,PRIHEX                                                        
         LA    R6,2(R6)                                                         
         BCT   R7,M3HHR                                                         
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
M4       CLI   MODE,MKTLAST                                                     
         BNE   M5                                                               
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'02',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    M41XIT                                                           
         B     M41A                                                             
M41      L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'02',(RF)),BFREC,1                        
M41A     TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M41XIT                                                           
         BAS   R8,PRIHEX                                                        
         LA    R9,BFREC                                                         
         CLI   BF1RTYP,0           SAVE TOALS                                   
         BNE   *+12                                                             
         BAS   R8,SEEDTOT                                                       
         B     M41                                                              
         BAS   RE,PAREPORT         PRINT MKT LEVEL PROGRAM REPORT               
         B     M41                                                              
         SPACE 2                                                                
M41XIT   L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'02',(RF)),(X'80',1)                    
         B     EXIT                                                             
M5       CLI   MODE,MKTFRST                                                     
         BNE   M6                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
M6       CLI   MODE,PRDFRST                                                     
         BNE   M7                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M7       CLI   MODE,PROCGOAL                                                    
         BNE   M8                                                               
         B     EXIT                                                             
         SPACE 2                                                                
M8       CLI   MODE,STAFRST                                                     
         BNE   M10                                                              
         L     RE,ADSTAT           SET UP DATA FROM STA MASTER RECORD           
         USING STAREC,RE                                                        
         MVC   CURRCALL,STAPRINT                                                
         MVI   CURRAFFL,C'N'       SET TO NETWORK AFFILIATE                     
         CLI   SNETWRK,C'A'                                                     
         BE    *+8                                                              
         CLI   SNETWRK,C'N'                                                     
         BE    *+8                                                              
         CLI   SNETWRK,C'C'                                                     
         BE    *+8                                                              
         CLI   SNETWRK,C'F'                                                     
         BE    *+8                                                              
         MVI   CURRAFFL,C'I'       RESET TO INDEPENDENT IF NOT A,N,OR C         
         MVI   CURRBAND,C'V'       SET TRANSMISSION BAND                        
         CLC   SCHNL(2),=C'14'                                                  
         BL    *+8                                                              
         MVI   CURRBAND,C'U'       ALL OTHERS ARE UHF                           
         B     EXIT                                                             
         SPACE 2                                                                
M10      CLI   MODE,RUNLAST                                                     
         BNE   M12                                                              
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
M12      CLI   MODE,STALAST                                                     
         BNE   M14                                                              
         XC    BFREC,BFREC                                                      
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         B     M12A1                                                            
         SPACE 2                                                                
M12A     L     RF,BUFFBUFF                                                      
         LA    R9,BFREC                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',(RF)),BFREC,1                       
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'                                                     
         BO    M12AXIT                                                          
         CLI   BFREC,X'01'         CORRECT RECORD CODE                          
         BNE   M12AXIT              NO - MUST BE THE END                        
         CLI   BF1RTYP,X'01'       CORRECT LEVEL OF DETAIL                      
         BNE   M12A                 NO - TRY NEXT RECORD                        
M12A1    MVC   SVBFREC,BFREC                                                    
         LA    R9,BFREC                                                         
         USING BFKEYS,R9                                                        
         MVC   BF1STA(4),MKT                                                    
         MVI   BF1RCDE,2                                                        
         MVI   BF1AFFL,X'FF'                                                    
         MVI   BF1BAND,X'FF'                                                    
         BAS   R8,PUTBUF           PUT OUT MARKET DETAILS                       
         SPACE 2                                                                
         MVC   BFREC,SVBFREC                                                    
         MVC   BF1STA(4),=X'FFFFFFFF'                                           
         MVI   BF1RCDE,4                                                        
         BAS   R8,PUTBUF           PUT OUT REPORT DETAILS                       
         SPACE 2                                                                
         MVC   BFREC,SVBFREC                                                    
         MVI   BF1AFFL,0                                                        
         MVI   BF1RTYP,0                                                        
         MVI   BF1PROG,0                                                        
         MVI   BF1BAND,0                                                        
         BAS   R8,PUTBUF           STATION BASE TOTALS                          
         SPACE 2                                                                
         MVC   BF1STA(4),MKT                                                    
         MVI   BF1RCDE,2                                                        
         BAS   R8,PUTBUF           MARKET BASE TOTALS                           
         SPACE 2                                                                
         MVC   BF1STA(4),=X'FFFFFFFF'                                           
         MVI   BF1RCDE,4                                                        
         BAS   R8,PUTBUF           REPORT BASE TOTALS                           
         SPACE 2                                                                
         MVC   BFREC,SVBFREC                                                    
         MVI   BF1AFFL,X'FF'                                                    
         MVI   BF1RTYP,X'FF'                                                    
         MVI   BF1PROG,X'FF'                                                    
         MVI   BF1BAND,X'FF'                                                    
         BAS   R8,PUTBUF           STATION TOTALS                               
         SPACE 2                                                                
         MVC   BF1STA(4),MKT                                                    
         MVI   BF1RCDE,2                                                        
         BAS   R8,PUTBUF           MARKET TOTALS                                
         SPACE 2                                                                
         MVC   BF1STA(4),=X'FFFFFFFF'                                           
         MVI   BF1RCDE,4                                                        
         BAS   R8,PUTBUF           REPORT TOTALS                                
         SPACE 2                                                                
         MVC   BFREC,SVBFREC                                                    
         XC    BFKEY,BFKEY                                                      
         MVI   BF31RCDE,3                                                       
         MVI   BF31TCDE,1                                                       
         MVC   BF31AFFL,CURRAFFL                                                
         MVI   BF31RTYP,1                                                       
         BAS   R8,PUTBUF           AFFILIATE DETAILS                            
         SPACE 2                                                                
         MVI   BF31AFFL,0          AFFILIATE BASE TOTALS                        
         MVI   BF31RTYP,0                                                       
         BAS   R8,PUTBUF                                                        
         SPACE 2                                                                
         MVI   BF31RTYP,X'FF'                                                   
         MVI   BF31AFFL,X'FF'                                                   
         BAS   R8,PUTBUF           AFFILIATE REPORT TOTALS                      
         SPACE 2                                                                
         MVC   BFREC,SVBFREC                                                    
         XC    BFKEY,BFKEY                                                      
         MVI   BF32RCDE,3                                                       
         MVI   BF32TCDE,2                                                       
         MVC   BF32BAND,CURRBAND                                                
         MVI   BF32RTYP,1                                                       
         BAS   R8,PUTBUF                                                        
         SPACE 2                                                                
         MVI   BF32BAND,0                                                       
         MVI   BF32RTYP,0                                                       
         BAS   R8,PUTBUF                                                        
         SPACE 2                                                                
         MVI   BF32RTYP,X'FF'                                                   
         MVI   BF32BAND,X'FF'                                                   
         BAS   R8,PUTBUF                                                        
         MVC   BFREC,SVBFREC                                                    
         B     M12A                                                             
         SPACE 2                                                                
M12AXIT  XC    BFREC,BFREC                                                      
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         B     M12B1                                                            
         SPACE 2                                                                
M12B     L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'                                                     
         BO    M12B2                                                            
         CLI   BFREC,X'01'                                                      
         BNE   M12B2                                                            
M12B1    BAS   R8,PRIHEX                                                        
         LA    R9,BFREC                                                         
         CLI   BF1RTYP,0           SAVE TOALS                                   
         BNE   *+12                                                             
         BAS   R8,SEEDTOT                                                       
         B     M12B                                                             
         BAS   RE,PAREPORT         PRINT STA LEVEL PROGRAM REPORT               
         B     M12B                                                             
         SPACE 2                                                                
M12B2    L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',(RF)),(X'80',1)                    
         MVI   P1,0                FORCE A SPACE                                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
M14      CLI   MODE,PRDLAST                                                     
         BNE   M16                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     RF,BUFFBUFF                                                      
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'04',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    M14AXIT                                                          
         B     M14A1                                                            
M14A     L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'04',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M14AXIT                                                          
M14A1    BAS   R8,PRIHEX                                                        
         LA    R9,BFREC                                                         
         CLI   BF1RTYP,0           SAVE TOALS                                   
         BNE   *+12                                                             
         BAS   R8,SEEDTOT                                                       
         B     M14A                                                             
         MVI   PAFIRST,1                                                        
         BAS   RE,PAREPORT         PRINT TOT LEVEL PROGRAM REPORT               
         B     M14A                                                             
         SPACE 2                                                                
M14AXIT  L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'04',(RF)),(X'80',1)                    
         EJECT                                                                  
* DO STATION TYPE REPORT                                                        
         MVI   RCSUBPRG,2                                                       
         XC    PREV3CDE,PREV3CDE                                                
         MVI   FORCEHED,C'Y'                                                    
         L     RF,BUFFBUFF                                                      
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'03',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    M14BXIT                                                          
         B     M14B1                                                            
M14B     L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'03',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M14BXIT                                                          
M14B1    BAS   R8,PRIHEX                                                        
         LA    R9,BFREC                                                         
         CLC   PREV3CDE,BF31RCDE                                                
         BE    M14B2                                                            
         MVI   P1,0                                                             
         GOTO1 REPORT                                                           
M14B2    MVC   PREV3CDE,BF31RCDE                                                
         CLI   BF31RTYP,0                                                       
         BNE   *+12                                                             
         BAS   R8,SEEDTOT                                                       
         B     M14B                                                             
         BAS   RE,PSREPORT                                                      
         B     M14B                                                             
         SPACE 2                                                                
M14BXIT  L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'03',(RF)),(X'80',1)                    
         EJECT                                                                  
         XC    BFREC,BFREC                                                      
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'05',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         B     M14C1                                                            
         SPACE 2                                                                
M14C     L     RF,BUFFBUFF                                                      
         LA    R9,BFREC                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'05',(RF)),BFREC,1                       
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'05',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'                                                     
         BO    M14CXIT                                                          
         CLI   BFREC,X'05'         CORRECT RECORD CODE                          
         BNE   M14CXIT              NO - MUST BE THE END                        
         CLI   BF2RTYP,X'01'       CORRECT LEVEL OF DETAIL                      
         BNE   M14C                 NO - TRY NEXT RECORD                        
M14C1    MVC   SVBFREC,BFREC                                                    
         MVI   BF2DAY,X'00'                                                     
         MVI   BF2STIM,X'00'                                                    
         MVI   BF2RTYP,X'00'                                                    
         BAS   R8,PUTBUF           AFF/BAND BASE                                
         MVI   BF2DAY,X'FF'                                                     
         MVI   BF2STIM,X'FF'                                                    
         MVI   BF2RTYP,X'FF'                                                    
         BAS   R8,PUTBUF           AFF/BAND TOT                                 
         MVC   BFREC,SVBFREC                                                    
         MVI   BF2BAND,X'FF'       AFF BASE                                     
         MVI   BF2DAY,X'00'                                                     
         MVI   BF2STIM,X'00'                                                    
         MVI   BF2RTYP,X'00'                                                    
         BAS   R8,PUTBUF                                                        
         MVI   BF2AFFL,X'FF'       TOT BASE                                     
         MVI   BF2RTYP,X'00'                                                    
         BAS   R8,PUTBUF                                                        
         MVC   BFREC,SVBFREC                                                    
         MVI   BF2BAND,X'FF'                                                    
         MVI   BF2DAY,X'FF'                                                     
         MVI   BF2STIM,X'FF'                                                    
         MVI   BF2RTYP,X'FF'                                                    
         BAS   R8,PUTBUF           AFF TOT                                      
         MVI   BF2AFFL,X'FF'                                                    
         BAS   R8,PUTBUF           TOT TOT                                      
         MVC   BFREC,SVBFREC                                                    
         MVI   BF2BAND,X'FF'       AFF DETAILS                                  
         MVI   BF2RTYP,X'02'                                                    
         BAS   R8,PUTBUF                                                        
         MVI   BF2AFFL,X'FF'                                                    
         BAS   R8,PUTBUF                                                        
         MVC   BFREC,SVBFREC                                                    
         B     M14C                                                             
M14CXIT  DS    0H'0'                                                            
         SPACE 2                                                                
         L     RF,BUFFBUFF                                                      
         XC    BFREC,BFREC                                                      
         MVI   RCSUBPRG,3                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'05',(RF)),BFREC,1                       
         TM    DMCB+8,X'80'                                                     
         BO    M14DXIT                                                          
         B     M14D1                                                            
M14D     L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(X'05',(RF)),BFREC,1                        
         TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M14DXIT                                                          
M14D1    BAS   R8,PRIHEX                                                        
         LA    R9,BFREC                                                         
         CLI   BF2RTYP,0                                                        
         BNE   *+16                                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   R8,SEEDTOT                                                       
         B     M14D                                                             
         BAS   RE,PTREPORT                                                      
         B     M14D                                                             
         SPACE 2                                                                
M14DXIT  L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'05',(RF)),(X'80',1)                    
         B     EXIT                                                             
         SPACE 2                                                                
M16      B     EXIT                                                             
         EJECT                                                                  
PUTBUF   L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(RF),BFREC                                  
         BR    R8                                                               
         SPACE 2                                                                
PRIHEX   BR    R8                                                               
         GOTO1 HEXOUT,DMCB,BFREC,P,28,0,0                                       
         GOTO1 REPORT                                                           
         BR    R8                                                               
         SPACE 2                                                                
SEEDTOT  MVC   TOTSPOTS,BFSPOTS                                                 
         MVC   TOTDOL,BFDOL                                                     
         MVC   TOTRTG,BFRTG                                                     
         MVC   TOTIMP,BFIMPS                                                    
         BR    R8                                                               
         SPACE 2                                                                
CALCPCT  XC    WORK,WORK                                                        
         OC    DMCB+4(4),DMCB+4                                                 
         BZR   R8                                                               
         SR    RE,RE                                                            
         L     RF,DMCB                                                          
         M     RE,DMCB+8                                                        
         SLDA  RE,1                                                             
         D     RE,DMCB+4                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(3,WORK)                                                    
         CLC   DMCB+8(4),=F'100'                                                
         BER   R8                                                               
         EDIT  (RF),(5,WORK),1                                                  
         BR    R8                                                               
         SPACE 2                                                                
EDITIMP  SR    RE,RE                                                            
         L     RF,FULL                                                          
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(17,WORK)                                              
         BR    R8                                                               
         SPACE 2                                                                
EDITDOL  SR    RE,RE                                                            
         L     RF,FULL                                                          
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         EDIT  (B4,FULL),(17,WORK),,COMMAS=YES                                  
         BR    R8                                                               
SPXDRA   DS    F                                                                
SPXDRB   DS    F                                                                
SPXDRC   DS    F                                                                
SPXDR2   DS    F                                                                
         EJECT                                                                  
PAREPORT NTR1                                                                   
         LA    R4,P1                                                            
         USING P1DEF,R4                                                         
         MVI   RCSUBPRG,1                                                       
         CLI   PAFIRST,1                                                        
         BNE   PA1                                                              
         MVI   PAFIRST,0                                                        
         MVC   STAPRINT+4(3),=C'   '                                            
         TM    BF1STA,X'F0'                                                     
         BNO   *+10                                                             
         MVC   STAPRINT(5),=C'TOTAL'                                            
         MVC   P1STA,STAPRINT                                                   
         MVC   P1AFFL,=C'AFF'                                                   
         CLI   BF1AFFL,C'N'                                                     
         BE    *+10                                                             
         MVC   P1AFFL,=C'IND'                                                   
         CLI   BF1AFFL,X'FF'                                                    
         BNE   *+10                                                             
         MVC   P1AFFL,=C'TOT'                                                   
         CLI   BF1BAND,X'FF'                                                    
         BE    PA1                                                              
         MVI   P1DASH,C'-'                                                      
         MVC   P1BAND,BF1BAND                                                   
PA1      MVC   P1PROG,=CL20' -UNKNOWN'                                          
         MVC   P1PROG(1),BF1PROG                                                
         CLI   BF1PROG,C'A'                                                     
         BNE   *+10                                                             
         MVC   P1PROG,=CL20'ANIMATED'                                           
         CLI   BF1PROG,C'K'                                                     
         BNE   *+10                                                             
         MVC   P1PROG,=CL20'LIVE-KID'                                           
         CLI   BF1PROG,C'F'                                                     
         BNE   *+10                                                             
         MVC   P1PROG,=CL20'LIVE-ALL-FAMILY'                                    
         CLI   BF1PROG,X'FF'                                                    
         BNE   *+10                                                             
         MVC   P1PROG,=CL20'  TOTAL'                                            
         MVC   FULL,BFDOL                                                       
         BAS   R8,EDITDOL                                                       
         MVC   P1DOL,WORK+7                                                     
         MVC   FULL,BFIMPS                                                      
         BAS   R8,EDITIMP                                                       
         MVC   P1IMPS,WORK+9                                                    
         MVC   DMCB(4),BFDOL                                                    
         MVC   DMCB+4(4),TOTDOL                                                 
         MVC   DMCB+8(4),=F'100'                                                
         BAS   R8,CALCPCT                                                       
         MVC   P1PCTDOL,WORK                                                    
         MVC   DMCB(4),BFIMPS                                                   
         MVC   DMCB+4(4),TOTIMP                                                 
         MVC   DMCB+8(4),=F'100'                                                
         BAS   R8,CALCPCT                                                       
         MVC   P1PCTIMP,WORK                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
PSREPORT NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         LA    R4,P1                                                            
         USING P3DEF,R4                                                         
         CLI   BF31TCDE,1          AFFILIATE REPORT                             
         BNE   PS2                                                              
         MVC   P3STYP,=CL12'AFFILIATE'                                          
         CLI   BF31AFFL,C'N'                                                    
         BE    *+10                                                             
         MVC   P3STYP,=CL12'INDEPENDENT'                                        
         CLI   BF31AFFL,X'FF'                                                   
         BNE   *+10                                                             
         MVC   P3STYP,=CL12'TOTAL'                                              
         B     PS3                                                              
         SPACE 2                                                                
PS2      MVC   P3STYP,=CL12'UHF'                                                
         CLI   BF32BAND,C'V'                                                    
         BNE   *+10                                                             
         MVC   P3STYP,=CL12'VHF'                                                
         CLI   BF32BAND,X'FF'                                                   
         BNE   *+10                                                             
         MVC   P3STYP,=CL12'TOTAL'                                              
PS3      DS    0H'0'                                                            
         MVC   FULL,BFDOL                                                       
         BAS   R8,EDITDOL                                                       
         MVC   P3DOL,WORK+7                                                     
         MVC   FULL,BFIMPS                                                      
         BAS   R8,EDITIMP                                                       
         MVC   P3IMP,WORK+6                                                     
         MVC   DMCB(4),BFDOL                                                    
         MVC   DMCB+4(4),TOTDOL                                                 
         MVC   DMCB+8(4),=F'100'                                                
         BAS   R8,CALCPCT                                                       
         MVC   P3PCTDOL,WORK                                                    
         MVC   DMCB(4),BFIMPS                                                   
         MVC   DMCB+4(4),TOTIMP                                                 
         MVC   DMCB+8(4),=F'100'                                                
         BAS   R8,CALCPCT                                                       
         MVC   P3PCTIMP,WORK                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
PTREPORT NTR1                                                                   
         LA    R4,P1                                                            
         USING P4DEF,R4                                                         
         MVC   P4AFFL,=C'AFF'                                                   
         CLI   BF2AFFL,C'N'                                                     
         BE    *+10                                                             
         MVC   P4AFFL,=C'IND'                                                   
         CLI   BF2AFFL,X'FF'                                                    
         BNE   *+10                                                             
         MVC   P4AFFL,=C'TOT'                                                   
         CLI   BF2BAND,X'FF'                                                    
         BE    PT1                                                              
         MVI   P4DASH,C'-'                                                      
         MVC   P4BAND,BF2BAND                                                   
PT1      CLI   BF2DAY,X'FF'                                                     
         BNE   PT1A                                                             
         MVC   P4DAY,=CL8'TOTAL'                                                
         B     PT2                                                              
PT1A     GOTO1 CODAY,DMCB,BF2DAY,P4DAY                                          
PT2      CLI   BF2STIM,X'FF'                                                    
         BNE   PT2A                                                             
         MVC   P4TIME,=CL11'TOTAL'                                              
         B     PT3                                                              
PT2A     MVC   STQH,BF2STIM                                                     
         ZIC   RE,STQH                                                          
         LA    RE,2(RE)                                                         
         STC   RE,ENQH                                                          
         GOTO1 VQHTOHR,DMCB,STQH,(X'00',SVT1)                                   
         GOTO1 VQHTOHR,DMCB,ENQH,(X'00',SVT2)                                   
         GOTO1 UNTIME,DMCB,SVTIMES,P4TIME                                       
PT3      SR    RF,RF                                                            
         ICM   RE,15,BFRTG                                                      
         SRDA  RE,31                                                            
         D     RE,BFSPOTS                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(4,P4AVERTG),1                                              
         MVC   DMCB(4),BFDOL                                                    
         MVC   DMCB+4(4),TOTDOL                                                 
         MVC   DMCB+8,=F'1000'                                                  
         BAS   R8,CALCPCT                                                       
         MVC   P4PCTDOL,WORK                                                    
         MVC   DMCB(4),BFIMPS                                                   
         MVC   DMCB+4(4),TOTIMP                                                 
         MVC   DMCB+8(4),=F'1000'                                               
         BAS   R8,CALCPCT                                                       
         MVC   P4PCTIMP,WORK                                                    
         SPACE 2                                                                
         CLC   QUESTOR(3),=C'TST'  PRINT IMPS AND DOLLARS                       
         BNE   PT4                 FOR TESTING                                  
         MVC   FULL,BFDOL                                                       
         BAS   R8,EDITDOL                                                       
         MVC   P4DOL,WORK+7                                                     
         MVC   FULL,BFIMPS                                                      
         BAS   R8,EDITIMP                                                       
         MVC   P4IMP,WORK+6                                                     
PT4      DS    0H'0'                                                            
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
XDHDHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,H7+42                                                         
         CLI   QAFFIL,C' '                                                      
         BE    XDHD4                                                            
         MVC   0(9,R4),=C'AFFIL=ABC'                                            
         CLI   QAFFIL,C'A'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'CBS'                                                  
         CLI   QAFFIL,C'C'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'NBC'                                                  
         CLI   QAFFIL,C'N'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'FOX'                                                  
         CLI   QAFFIL,C'F'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'IND'                                                  
         CLI   QAFFIL,C'I'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'WB '                                                  
         CLI   QAFFIL,C'W'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'UPN'                                                  
         CLI   QAFFIL,C'U'                                                      
         BE    XDHD2                                                            
         MVC   6(3,R4),=C'???'                                                  
XDHD2    LA    R4,10(R4)                                                        
*                                                                               
XDHD4    CLI   Q2DPT,C' '          TEST DAYPART FILTER                          
         BE    EXIT                                                             
         MVC   0(8,R4),=C'DAYPART=X'                                            
         MVC   8(1,R4),Q2DPT                                                    
*                                                                               
         EJECT                                                                  
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
CURRCALL DS    CL5                                                              
CURRAFFL DS    CL1                                                              
CURRBAND DS    CL1                                                              
CURRPROG DS    CL1                                                              
PREV3CDE DS    CL2                                                              
PAFIRST  DS    CL1                                                              
PTFIRST  DS    CL1                                                              
SVBFREC  DS    CL28                                                             
SVDAYS   DS    C                                                                
SVTIMES  DS    0CL4                                                             
SVT1     DS    CL2                                                              
SVT2     DS    CL2                                                              
STQH     DS    C                                                                
ENQH     DS    C                                                                
RELO     DS    F                                                                
VHRTOQH  DS    F                                                                
VQHTOHR  DS    F                                                                
NUMHALF  DS    F                                                                
TOTSPOTS DS    F                                                                
TOTDOL   DS    F                                                                
TOTRTG   DS    F                                                                
TOTIMP   DS    F                                                                
         SPACE 2                                                                
         DS    0F                                                               
* GENERAL BUFFALO RECORD AREA                                                   
* KEYS VARY BY REPORT / DATA IS CONSTANT ACCROSS REPORTS                        
BFREC    DS    0CL28                                                            
BFKEY    DS    CL12                                                             
BFDATA   DS    0CL16                                                            
BFSPOTS  DS    CL4                                                              
BFDOL    DS    CL4                                                              
BFRTG    DS    CL4                                                              
BFIMPS   DS    CL4                                                              
         SPACE 2                                                                
PSLIST   DS    CL200                                                            
DNAMES   DS    CL14                                                             
         SPACE 2                                                                
*                                                                               
BFKEYS   DSECT                                                                  
* KEY FOR STATION MARKET PROGRAM REPORT                                         
BF1RCDE  DS    CL1                                                              
BF1STA   DS    CL5                                                              
BF1AFFL  DS    CL1                                                              
BF1RTYP  DS    CL1                                                              
BF1PROG  DS    CL1                                                              
BF1BAND  DS    CL1                                                              
         SPACE 2                                                                
         ORG   BF1RCDE                                                          
* KEY FOR DAY/TIME ANALYSIS REPORT                                              
BF2RCDE  DS    CL1                                                              
BF2AFFL  DS    CL1                                                              
BF2BAND  DS    CL1                                                              
BF2DAY   DS    CL1                                                              
BF2STIM  DS    CL1                                                              
BF2RTYP  DS    CL1                                                              
         SPACE 2                                                                
         ORG   BF1RCDE                                                          
* KEY FOR AFFILIATE ANALYSIS REPORT                                             
BF31RCDE DS    CL1                                                              
BF31TCDE DS    CL1                                                              
BF31AFFL DS    CL1                                                              
BF31RTYP DS    CL1                                                              
         SPACE 2                                                                
         ORG   BF1RCDE                                                          
* KEY FOR TRANSMISSION BAND REPORT                                              
BF32RCDE DS    CL1                                                              
BF32TCDE DS    CL1                                                              
BF32BAND DS    CL1                                                              
BF32RTYP DS    CL1                                                              
         EJECT                                                                  
PLDEFINE DSECT                                                                  
P1DEF    DS    0C                                                               
         DS    CL1                                                              
P1STA    DS    CL5                                                              
         DS    CL4                                                              
P1AFFL   DS    CL3                                                              
P1DASH   DS    CL1                                                              
P1BAND   DS    CL1                                                              
         DS    CL2                                                              
P1PROG   DS    CL20                                                             
         DS    CL1                                                              
P1DOL    DS    CL10                                                             
         DS    CL3                                                              
P1PCTDOL DS    CL3                                                              
         DS    CL4                                                              
P1IMPS   DS    CL8                                                              
         DS    CL5                                                              
P1PCTIMP DS    CL3                                                              
         SPACE 2                                                                
         ORG   PLDEFINE                                                         
P3DEF    DS    0C                                                               
P3STYP   DS    CL12                                                             
         DS    CL2                                                              
P3DOL    DS    CL10                                                             
         DS    CL3                                                              
P3PCTDOL DS    CL3                                                              
         DS    CL2                                                              
P3IMP    DS    CL11                                                             
         DS    CL4                                                              
P3PCTIMP DS    CL3                                                              
         SPACE 2                                                                
         ORG   PLDEFINE                                                         
P4DEF    DS    0C                                                               
P4DAY    DS    CL8                                                              
         DS    CL1                                                              
P4TIME   DS    CL11                                                             
         DS    CL3                                                              
P4AFFL   DS    CL3                                                              
P4DASH   DS    CL1                                                              
P4BAND   DS    CL1                                                              
         DS    CL6                                                              
P4AVERTG DS    CL4                                                              
         DS    CL5                                                              
P4PCTDOL DS    CL5                                                              
         DS    CL4                                                              
P4PCTIMP DS    CL5                                                              
         DS    CL2                                                              
P4DOL    DS    CL10                DOLLARS FOR TESTING                          
         DS    CL2                                                              
P4IMP    DS    CL11                IMPS FOR TESTING                             
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=3600,ROWS=1,COLUMNS=4,FLAVOR=BINARY,KEYLIST=(12,A)         
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
Q2DPT    DS    CL1                                                              
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPXD02 05/01/02'                                      
         END                                                                    
