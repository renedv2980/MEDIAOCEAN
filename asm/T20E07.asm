*          DATA SET T20E07     AT LEVEL 022 AS OF 10/04/83                      
*PHASE T20E07C,+0,NOAUTO                                                        
         TITLE 'EXPAND AND COMPUTE DEMOS'                                       
T20E07   CSECT                                                                  
         NMOD1 0,T20E07                                                         
         L     RC,0(R1)            A(WORK AREA)                                 
         LA    RC,0(RC)                                                         
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         CLC   SCRNPAGE,PREVPAG    SAME PAGE AS PREVIOUS                        
         BNE   HAVPAG               NO - CONTINUE                               
         IC    R7,SCRNPAGE          YES - BUMP PAGE BY ONE                      
         LA    R7,1(R7)                                                         
         STC   R7,SCRNPAGE                                                      
HAVPAG   MVC   PREVPAG,SCRNPAGE                                                 
         LA    R2,DMENAMH          MOVE DEMO NAMES TO HEADLINES                 
         LA    R4,DEMNAME                                                       
HL1      MVC   FLDDATA(7),0(R4)                                                 
         LA    R2,8(R2)                                                         
         LA    R4,7(R4)                                                         
         CLI   0(R4),0             END OF NAMES                                 
         BNE   HL1                  NO - CONTINUE NAMES                         
         LA    R2,DMENAMH                                                       
         FOUT  (R2)                                                             
         LA    R2,DMEDASHH         UNDERLINE DEMO NAMES                         
         LA    R4,DEMLST                                                        
HL2      MVC   FLDDATA(7),=C'-------'                                           
         LA    R4,3(R4)                                                         
         LA    R2,8(R2)                                                         
         CLI   1(R4),0             END OF DEMOS                                 
         BNE   HL2                  NO - CONTINUE DASHES                        
         LA    R2,DMEDASHH                                                      
         FOUT  (R2)                                                             
*                                                                               
*        IF LESS THAN 6 DEMOS REQUESTED SEND PROGRAM NAME                       
         XC    HALF2,HALF2                                                      
         LA    R4,DEMLST                                                        
         LA    R5,0                                                             
         CLI   1(R4),0                                                          
         BE    *+16                                                             
         LA    R5,1(R5)                                                         
         LA    R4,3(R4)                                                         
         B     *-16                                                             
         CLI   FNO,2               ADJUSTMENTS SCREEN                           
         BH    NOPROG               YES - NO PROGRAM                            
         CH    R5,=H'4'            MORE THAN 5 DEMOS                            
         BH    NOPROG               NO - GET DEMOS                              
         MH    R5,=H'8'             YES - SET DISPLACEMENT                      
         LA    R2,DMENAMH                                                       
         AR    R2,R5                                                            
         MVC   FLDDATA(12),=C'PROGRAM NAME'                                     
         LA    R2,DMEDASHH                                                      
         AR    R2,R5                                                            
         MVC   FLDDATA(12),=C'------------'                                     
         STH   R5,HALF2            SAVE PROGRAM NAME DISPLACEMENT               
         LA    R2,DMEDASHH                                                      
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         EJECT                                                                  
*                                                                               
*        HEADLINES BUILT - NOW LOOK UP DEMOS                                    
*                                                                               
NOPROG   TM    FNO,1               DEMO EXPLODE REQUESTED                       
         BO    EXXMOD               NO - COMPUTE DEMOS                          
         GOTO1 VCALLOV,DMCB,(9,0),(RA)                                          
         TM    4(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE SPGENBUY                                                       
         ORG   IOAREA                                                           
       ++INCLUDE T20EWORK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022T20E07    10/04/83'                                      
         END                                                                    
