*          DATA SET T20E04     AT LEVEL 012 AS OF 10/20/81                      
*PHASE T20E04C,+0,NOAUTO                                                        
         TITLE 'LIST BOOKS ON THE DEMO FILE'                                    
T20E04   CSECT                                                                  
         NMOD1 0,T20E04                                                         
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         XC    DATALIN,DATALIN                                                  
         LA    R2,DMCHDC0H         SET HEADLINES                                
         OC    STAT,STAT                                                        
         BNZ   H2                                                               
         MVC   FLDDATA+15(06),=C' TOTAL    REGULAR    M-F     COMBO'            
         FOUT  (R2)                                                             
         LA    R2,88(R2)                                                        
H2       MVC   FLDDATA(13),=C'BOOKS ON FILE'                                    
         OC    STAT,STAT                                                        
         BNZ   H3                                                               
         MVC   FLDDATA+15(06),=C'MARKETS   MARKETS  MARKETS  MARKETS'           
H3       FOUT  (R2)                                                             
         LA    R2,88(R2)                                                        
         MVC   FLDDATA(13),=C'-------------'                                    
         OC    STAT,STAT                                                        
         BNZ   *+10                                                             
         MVC   FLDDATA+15(06),=C'-------   -------  -------  -------'           
         FOUT  (R2)                                                             
         LA    R2,176(R2)                                                       
         LA    RE,WORKEND          CLEAR WORK AREA                              
         LA    RF,2100                                                          
         XCEF                                                                   
         LA    R7,WORKEND          SET WORK ADDRESS                             
         LA    R8,15               SET NUMBER OF LINES AVAILABLE                
         LA    R6,0                SET TABLE DISPLACEMENT                       
         XC    KEY,KEY             SET FIRST KEY                                
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),CMEDIA                                                  
         MVI   KEY+2,C'A'                                                       
         CLI   CSOURCE,1                                                        
         BE    *+8                                                              
         MVI   KEY+2,C'N'                                                       
         MVI   KEY+3,X'80'                                                      
         LA    R7,WORKEND                                                       
*        READ HIGH ROUTINE                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA,0                 
         MVC   KEY,IOAREA                                                       
         B     PROC                                                             
GETNXT   MVC   KEYSAVE,KEY                                                      
*                                                                               
*        READ SEQUENTIAL ROUTINE                                                
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA,0                 
         TM    IOAREA+6,X'80'                                                   
         BZ    *+8                                                              
         MVI   IOAREA+6,X'02'                                                   
         MVC   KEY,IOAREA                                                       
         CLC   KEY(6),KEYSAVE      BOOK CHANGE                                  
         BNE   FORMAT               YES - FORMAT LINE                           
PROC     OC    STAT,STAT                                                        
         BNZ   STAONLY             LIST BOOK FOR 1 STATION ONLY                 
         LA    R6,IOAREA                                                        
         USING MLKEY,R6                                                         
PROC2    LA    RE,WORKEND          GET ADDRESS OF MARKET LIST                   
         AH    RE,=H'1200'                                                      
         LH    RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STH   RF,0(RE)                                                         
         LA    R6,KEY                                                           
         MVC   MLSTAT,=X'FFFFFFFFFF'                                            
         OC    STAT,STAT                                                        
         BZ    GETNXT                                                           
         MVI   MLRMKT,X'FF'                                                     
         B     GETNXT                                                           
*                                                                               
STAONLY  LA    R6,IOAREA                                                        
         CLC   MLSTAT(4),STAT                                                   
         BE    PROC2                                                            
         LA    R6,KEY                                                           
         BL    *+12                                                             
         MVI   MLSTAT,X'FF'                                                     
         B     GETNXT                                                           
         MVC   MLSTAT(4),STAT                                                   
         MVI   MLSTAT+4,0                                                       
         B     GETNXT                                                           
*                                                                               
FORMAT   LA    RE,WORKEND                                                       
         AH    RE,=H'1200'                                                      
         OC    0(2,RE),0(RE)       ANY ENTRIES IN TABLE                         
         BZ    CHKEND+4             NO - CHECK FOR OF BOOK RECORDS              
         MVC   HALF,KEYSAVE+3                                                   
         XC    HALF,=X'FFFF'                                                    
*                                                                               
         SR    RE,RE               CONVERT BOOK TO ALPHA                        
         IC    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB+6(2)                                                 
         IC    RE,HALF+1                                                        
         MH    RE,=H'3'                                                         
         LA    RF,MONTAB-3                                                      
         AR    RF,RE                                                            
         LA    RE,WORKEND                                                       
         AH    RE,=H'1200'                                                      
         MVC   0(3,R7),0(RF)                                                    
*                                                                               
         OC    STAT,STAT           STATION FORMAT                               
         BNZ   CHKEND                                                           
         XC    DMCB(24),DMCB                                                    
FORMAT1  OC    0(2,RE),0(RE)       END                                          
         BZ    CHKEND                                                           
         LH    RF,0(RE)                                                         
         LA    R8,17(R7)                                                        
         EDIT  (RF),(4,(R8))                                                    
CHKEND   LA    R7,80(R7)                                                        
         LA    RE,WORKEND          SETUP FOR NEXT BOOK                          
         AH    RE,=H'1200'                                                      
         LA    RF,900                                                           
         XCEF                                                                   
         LA    R6,KEY                                                           
         MVI   MLIND,X'FF'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA                   
         MVC   KEY,IOAREA                                                       
         CLC   KEY(3),KEYSAVE                                                   
         BNE   FOUTBOOK                                                         
         MVC   KEYSAVE,KEY                                                      
         LA    R6,KEY                                                           
         CLI   MLIND,MLINDEQU                                                   
         BE    PROC                                                             
*                                                                               
*        READING IS DONE - NOW PUT LINES ON SCREEN                              
*                                                                               
FOUTBOOK LA    R7,WORKEND                                                       
         LA    R8,15                                                            
FOUTBOK1 MVC   FLDDATA(79),0(R7)                                                
         FOUT  (R2)                                                             
         LA    R2,88(R2)                                                        
         LA    R7,80(R7)                                                        
         CLI   0(R7),0                                                          
         BE    *+8                                                              
         BCT   R8,FOUTBOK1                                                      
         LA    R2,DEMSRCH                                                       
         XMOD1 1                                                                
         B     EXIT                                                             
MONTAB   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DMDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
