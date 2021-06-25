*          DATA SET T20E02     AT LEVEL 013 AS OF 10/07/83                      
*PHASE T20E02C,+0,NOAUTO                                                        
*INCLUDE XSORT                                                                  
SDATAL   EQU   16                  NUMBER OF DATA LINES AVAILABLE               
LINLEN   EQU   88                  LENGTH OF DATA LINE + HEADER                 
DATALN   EQU   80                  LENGTH OF DATA FIELD                         
DTABLEN  EQU   13                                                               
         TITLE 'LIST DEMOS BY ADJUSTMENT TYPE'                                  
T20E02   CSECT                                                                  
         NMOD1 0,T20E02,RR=R8                                                   
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R8,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       L     RC,0(R1)                                                         
         XC    DATALIN,DATALIN                                                  
         LA    R2,DMCHDCHH         SET HEADLINES                                
         MVC   FLDDATA+14(31),=C'DEMOGRAPHICS BY ADJUSTMENT TYPE'               
         FOUT  (R2)                                                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         LA    R6,HUTHUTL          FIND ADJUSTMENT TYPE USED BY AGENCY          
FHTYPE   MVI   HALF,C'A'                                                        
         CLI   CSOURCE,2                                                        
         BNE   *+8                                                              
         MVI   HALF,C'N'                                                        
FHTYPE1  CLC   HALF(1),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,4(R6)                                                         
         B     FHTYPE1                                                          
         LH    R6,2(R6)                                                         
         LA    R6,HUTHUTL(R6)                                                   
FHTYPE3  CLC   AGYALPHA,0(R6)                                                   
         BE    FHTYPE4                                                          
         LA    R6,6(R6)                                                         
         CLC   =C'ZZ',0(R6)                                                     
         BNE   FHTYPE3                                                          
         B     NOADJ                                                            
FHTYPE4  CLI   4(R6),C'1'                                                       
         BE    HUTONLY                                                          
*                                                                               
         LA    R7,HTRTTAB          FIND CORRECT SVI TABLE                       
FSVITAB  CLI   0(R7),0                                                          
         BE    PROGERR             NOT FOUND - PROGRAM ERROR                    
         CLC   0(1,R7),4(R6)                                                    
         BE    *+12                                                             
         LA    R7,129(R7)                                                       
         B     FSVITAB                                                          
*                                                                               
*        HAVE PROPER SVI TABLE - NOW CALCULATE DEMOS APPLIED TO                 
*                                                                               
         LA    R6,2(R7)            SET BEGINNING OF TABLE                       
         ST    R6,FULL                                                          
         L     R7,ADEMTAB                                                       
         LA    R8,WORKEND                                                       
BLDDEM   SR    RE,RE                                                            
         IC    RE,CSOURCE          SET SOURCE                                   
         TM    1(R7),4                                                          
         BO    BLDDEM2                                                          
         L     R6,FULL             SET TABLE ADDRESS                            
         TM    1(R7),X'40'                                                      
         BO    BLDDEM2                                                          
         EX    RE,*+12                                                          
         BZ    BLDDEM2                                                          
         B     *+8                                                              
         TM    1(R7),0   *EXECUTED * CHECK RATING SOURCE                        
         IC    RE,0(R7)                                                         
         AR    R6,RE                                                            
         BCTR  R6,R0               -1 FOR PROPER DISPLACEMENT                   
         MVC   0(1,R8),0(R6)       MOVE SVI TYPE                                
         MVC   1(7,R8),6(R7)       MOVE DEMO NAME                               
         LA    R8,8(R8)            NEXT SLOT                                    
BLDDEM2  LA    R7,DTABLEN(R7)      NEXT DEMO                                    
         CLI   0(R7),X'FF'                                                      
         BNE   BLDDEM                                                           
*                                                                               
*        LIST OF DEMOS HAS BEEN BUILT - PAD BUFFER FOR PROPER SPACING           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R7,WORKEND                                                       
BLDPAD   LA    R6,WORK                                                          
         SR    RE,RE                                                            
         IC    RE,0(R7)                                                         
         AR    R6,RE               COUNT NUMBER OF ENTRIES                      
         IC    RE,0(R6)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R6)                                                         
         LA    R7,8(R7)                                                         
         CLI   1(R7),0             END OF TABLE                                 
         BNE   BLDPAD              NO - CONTIMUE COUNTING                       
*                                                                               
*        PAD EXTRA DATA LINES WITH BLANKS                                       
*                                                                               
         LA    RF,0                                                             
         LA    R6,WORK                                                          
         LA    RE,9                UP TO NINE SVIS                              
BLDPAD2  CLI   0(R6),0                                                          
         BNE   BLDPAD3                                                          
         LA    R6,1(R6)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,BLDPAD2                                                       
         B     ENDPAD                                                           
BLDPAD3  SR    R5,R5                                                            
         IC    R5,0(R6)                                                         
         CH    R5,=H'16'                                                        
         BNH   BLDPAD4                                                          
         SH    R5,=H'16'                                                        
         STC   R5,0(R6)                                                         
         B     BLDPAD2                                                          
BLDPAD4  CH    R5,=H'16'           PAD COLUMNS WITH BLANK ENTRIES               
         BNE   BLDPAD5                                                          
         LA    R6,1(R6)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,BLDPAD2                                                       
         B     ENDPAD                                                           
BLDPAD5  STC   RF,0(R8)                                                         
         MVC   1(7,R8),=C'       '                                              
         LA    R8,8(R8)                                                         
         LA    R5,1(R5)                                                         
         B     BLDPAD4                                                          
*                                                                               
*        LIST IS BUILT WITH APPROPRIATE PADDING NOW SORT                        
*                                                                               
ENDPAD   LA    R8,WORKEND                                                       
         SR    R5,R5                                                            
         CLI   1(R8),0             COUNT ELEMENTS FOR SORT                      
         BE    *+16                                                             
         LA    R8,8(R8)                                                         
         LA    R5,1(R5)                                                         
         B     *-16                                                             
         GOTO1 =V(XSORT),DMCB,(X'00',WORKEND),(R5),8,1,0,RR=RELO                
*                                                                               
*        LIST IS SORTED - NOW GET HEADLINES                                     
*                                                                               
BLDHL    CLC   SCRNPAGE,PREVPAG    SAME PAGE AS PREVIOUS                        
         BNE   HAVPAGE              NO - PROCESS PAGE                           
         SR    RE,RE                YES - NEXT PAGE                             
         IC    RE,SCRNPAGE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,SCRNPAGE                                                      
         STC   RE,PREVPAG                                                       
HAVPAGE  LA    R8,WORKEND          CALCULATE PAGE DISPLACEMENT INTO             
         SR    RE,RE                TABLE                                       
         IC    RE,SCRNPAGE                                                      
         BCTR  RE,R0                                                            
         MH    RE,=H'160'                                                       
         MH    RE,=H'8'                                                         
         AR    R8,RE                                                            
         CLI   1(R8),0                                                          
         BNE   *+12                                                             
         MVI   SCRNPAGE,1                                                       
         B     HAVPAGE                                                          
         ST    R8,FULL                                                          
         LA    R2,DMCHDC0H                                                      
         LA    R9,10               SET HEADLINES                                
SHL      CLI   1(R8),0                                                          
         BE    HLDONE                                                           
         CLI   0(R8),0                                                          
         BNE   SHL2                                                             
         MVC   FLDDATA(7),=C' NO SVI'                                           
         MVC   FLDDATA+LINLEN(7),=C'-------'                                    
SHL3     LA    R8,128(R8)                                                       
         LA    R2,8(R2)                                                         
         BCT   R9,SHL                                                           
         B     HLDONE                                                           
SHL2     LA    RF,SVITAB                                                        
         SR    R7,R7                                                            
         IC    R7,0(R8)                                                         
         BCTR  R7,0                                                             
         MH    R7,=H'7'                                                         
         AR    RF,R7                                                            
         MVC   FLDDATA(7),0(RF)                                                 
         MVC   FLDDATA+LINLEN(7),=C'-------'                                    
         B     SHL3                                                             
*                                                                               
*        HEADLINES ARE DONE FOUT DATA LINES                                     
*                                                                               
HLDONE   L     R8,FULL                                                          
         CLI   1(R8),0                                                          
         BE    FOUTSVI                                                          
         MVI   0(R8),C' '                                                       
         LA    R8,8(R8)                                                         
         B     HLDONE+4                                                         
*                                                                               
FOUTSVI  LA    R2,DMCHDC1H                                                      
         ST    R2,DUB                                                           
         LA    RF,16                                                            
FOUTSVI2 L     R2,DUB                                                           
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,DUB                                                           
         L     R8,FULL                                                          
         LA    R9,10                                                            
FOUTSVI3 MVC   FLDDATA(7),1(R8)                                                 
         LA    R8,128(R8)          NEXT COLUMN                                  
         LA    R2,8(R2)                                                         
         BCT   R9,FOUTSVI3                                                      
         L     R2,DUB                                                           
         FOUT  (R2)                                                             
         L     R8,FULL                                                          
         LA    R8,8(R8)                                                         
         ST    R8,FULL                                                          
         BCT   RF,FOUTSVI2                                                      
EXA      LA    R2,DEMPAGH                                                       
         OI    FLDOIND,X'40'                                                    
         B     EXXMOD                                                           
*                                                                               
*        AGENCY HAS HUT ADJUSTMENTS ONLY                                        
*                                                                               
HUTONLY  SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   FLDDATA(38),=C'ALL DEMOS ARE ADJUSTED BY HOMES FACTOR'           
         B     EXA                                                              
*                                                                               
*        AGENCY HAS NO ADJUSTMENT FACTORS                                       
*                                                                               
NOADJ    SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   FLDDATA(22),=C'NO ADJUSTMENTS ON FILE'                           
         B     EXA                                                              
*                                                                               
*        PROGRAMMER ERROR                                                       
*                                                                               
PROGERR  DC    H'0'                                                             
* RZHUTTABN                                                                     
       ++INCLUDE RZHUTTABN                                                      
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
