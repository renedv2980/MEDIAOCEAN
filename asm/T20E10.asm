*          DATA SET T20E10     AT LEVEL 026 AS OF 05/01/02                      
*PHASE T20E10C,+0,NOAUTO                                                        
*INCLUDE DAYVAL                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE TIMVAL                                                                 
         TITLE 'EDIT DAY AND TIME'                                              
T20E10   CSECT                                                                  
         NMOD1 0,T20E10,RR=R8                                                   
         L     RC,0(R1)            A(WORK AREA)                                 
         LA    RC,0(RC)            CLEAR HIGH ORDER BYTE                        
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R8,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       XC    DUB,DUB                                                          
         MVI   BYTE2,0             RESET ERROR                                  
         LA    R5,16                                                            
EDTDYTM2 BAS   R9,EDTDAY           EDIT DAY                                     
         FOUT  (R2)                                                             
         OI    6(R2),1                                                          
         SR    RE,RE               NEXT FIELD                                   
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         BAS   R9,EDTTIM                                                        
         FOUT  (R2)                                                             
         OI    6(R2),1                                                          
         SR    RE,RE               NEXT FIELD                                   
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         LA    RE,DYTMLST                                                       
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,6(RE)                                                         
         B     *-12                                                             
         MVC   0(6,RE),DUB                                                      
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         TM    FNO,1                                                            
         BZ    EXXMOD                                                           
         CLI   FLDILEN,0                                                        
         BE    EXXMOD                                                           
         BCT   R5,EDTDYTM2                                                      
         EJECT                                                                  
EDTDAY   GOTO1 =V(DAYVAL),DMCB,(FLDILEN,FLDDATA),DUB,DMWORK,RR=RELO             
         LA    R3,INVDAY                                                        
         CLI   DUB,0                                                            
         BNER  R9                                                               
         GOTO1 VDATVAL,DMCB,FLDDATA,DMWORK                                      
         CLI   DMCB+3,0                                                         
         BNE   GETMO                                                            
         STC   R3,BYTE2                                                         
         B     ERROR                                                            
*                                                                               
* DETERMINE BROADCAST MONTH                                                     
GETMO    GOTO1 =V(GETBROAD),DMCB,(X'01',DMWORK),DMWORK+6,VGETDAY,      X        
               VADDAY,RR=RELO                                                   
         PACK  DUB,DMWORK+14(2)       SET ADJUSTMENT MONTH                      
         CVB   RE,DUB                                                           
         STC   RE,DUB+5                                                         
*                                                                               
* SET DAY                                                                       
         GOTO1 VGETDAY,DMCB,DMWORK,DMWORK+6                                     
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,DAYTAB-1(RE)                                                  
         MVC   DUB(1),0(RE)                                                     
         BR    R9                                                               
         EJECT                                                                  
EDTTIM   LA    R1,FLDDATA                                                       
         SR    RE,RE                                                            
         IC    RE,FLDILEN                                                       
         AR    R1,RE                                                            
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         CLI   0(R1),C'0'                                                       
         BL    EDTTIM1                                                          
         CLI   0(R1),C'9'                                                       
         BH    EDTTIM1                                                          
         B     EDTTIM2                                                          
EDTTIM1  MVI   BYTE2,INVTIME                                                    
         B     ERROR                                                            
EDTTIM2  GOTO1 =V(TIMVAL),DMCB,(FLDILEN,FLDDATA),DUB+1,RR=RELO                  
         LA    R3,INVTIME                                                       
         CLI   DMCB,X'FF'                                                       
         BNER  R9                                                               
         STC   R3,BYTE2            SET ERROR                                    
         B     ERROR                                                            
INVDAY   EQU   2                                                                
INVTIME  EQU   2                                                                
DAYTAB   DC    X'40201008040201'                                                
         DS    0D                                                               
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026T20E10    05/01/02'                                      
         END                                                                    
