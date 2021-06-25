*          DATA SET CTCONRLP   AT LEVEL 002 AS OF 06/10/00                      
*PHASE CONRLP,*                                                                 
***********************************************************************         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE SYSTEM ELEMENTS ON *         
* TERMINAL, ID AND PERSONAL AUTH RECORDS TO COPY THE RFP AUTH TO RLP  *         
***********************************************************************         
                                                                                
CONRLP   TITLE '- COPY RFP AUTHORIZATION TO RLP'                                
CONRLP   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*CONRLP*                                                   
         USING WORKD,RC                                                         
         LR    RA,R1               GET W/S POINTER                              
         USING CONWORKD,RA                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         L     R2,AIOAREA                                                       
         LA    R2,4(R2)            POINT TO RECORD KEY                          
                                                                                
         USING CTTREC,R2                                                        
         CLI   CTTKTYP,CTTKTYPQ    TEST TERMINAL RECORD                         
         BE    FIX02                                                            
         CLI   CTTKTYP,CTIKTYPQ    TEST USER-ID RECORD                          
         BE    FIX02                                                            
         CLI   CTTKTYP,CT0KTEQU    TEST PERSONAL AUTH RECORD                    
         BNE   EXIT                                                             
                                                                                
FIX02    TM    CTTSTAT,X'80'       TEST INPUT RECORD IS DELETED                 
         BNZ   EXIT                                                             
                                                                                
         LA    R3,IO               CONSTRUCT NEW RECORD AT IO                   
         USING CTIREC,R3           R3=A(OUTPUT RECORD)                          
         MVC   CTIKEY(CTIDATA-CTIREC),CTTREC                                    
         MVI   FLAG1,0                                                          
                                                                                
         LA    R2,CTTDATA                                                       
OLD      USING CTSYSD,R2                                                        
         LA    R3,CTIDATA                                                       
NEW      USING CTSYSD,R3                                                        
                                                                                
FIX04    CLI   OLD.CTSYSEL,0       TEST END OF RECORD                           
         BE    FIX12                                                            
         CLI   OLD.CTSYSEL,CTSYSELQ                                             
         BNE   FIX08                                                            
                                                                                
         SR    RE,RE               FOUND A SYSTEM ELEMENT                       
         IC    RE,OLD.CTSYSLEN                                                  
         SHI   RE,CTSYSL1Q                                                      
         SRDA  RE,32                                                            
         LA    R0,L'CTSYSPGM                                                    
         DR    RE,R0               RF=N'ENTRIES IN PROGRAM LIST                 
         LTR   RF,RF               TEST ONLY HAS 'ALL' VALUE                    
         BZ    FIX08                                                            
         LA    RE,OLD.CTSYSPGM     RE=A(OLD PROGRAM LIST)                       
         MVI   FLAG2,0                                                          
                                                                                
FIX06    CLI   0(RE),RLPQ          TEST RLP AUTHORIZATION                       
         BE    FIX08               YES - JUST COPY ELEMENT                      
         CLI   0(RE),RFPQ          TEST RFP AUTHORIZATION                       
         BNE   *+14                                                             
         MVC   AUTH,1(RE)          YES - SAVE AUTH AND SET FLAG                 
         MVI   FLAG2,1             SET RFP AUTH FOUND                           
                                                                                
         AHI   RE,L'CTSYSPGM       BUMP TO NEXT AUTHORIZATION                   
         BCT   RF,FIX06                                                         
         CLI   FLAG2,0             TEST RFP AUTH FOUND                          
         BE    FIX08               NO - JUST COPY ELEMENT                       
                                                                                
         SR    RE,RE                                                            
         IC    RE,OLD.CTSYSLEN     COPY ELEMENT OVER AND EXTEND                 
         EX    RE,*+4                                                           
         MVC   NEW.CTSYSD(0),OLD.CTSYSD                                         
         LA    RF,NEW.CTSYSD(RE)                                                
         MVI   0(RF),RLPQ          BUILD RLP AUTHORIZATION                      
         MVC   1(L'AUTH,RF),AUTH                                                
         AHI   RE,L'CTSYSPGM                                                    
         STC   RE,NEW.CTSYSLEN                                                  
         OI    FLAG1,1             SET RECORD EXTENDED                          
         B     FIX10                                                            
                                                                                
FIX08    SR    RE,RE               COPY ELEMENT FROM OLD TO NEW                 
         IC    RE,OLD.CTSYSLEN                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEW.CTSYSD(0),OLD.CTSYSD                                         
                                                                                
FIX10    SR    RE,RE               BUMP TO NEXT ELEMENT ON BOTH                 
         IC    RE,OLD.CTSYSLEN                                                  
         AR    R2,RE                                                            
         IC    RE,NEW.CTSYSLEN                                                  
         AR    R3,RE                                                            
         B     FIX04                                                            
                                                                                
FIX12    CLI   FLAG1,0             TEST ANY CHANGES MADE TO RECORD              
         BE    EXIT                                                             
         MVI   0(R3),0             SET END OF NEW RECORD                        
         LA    R1,1(R3)                                                         
         LA    R3,IO                                                            
         USING CTIREC,R3                                                        
         SR    R1,R3               R1=L'NEW RECORD                              
         CLM   R1,3,=AL2(MAXLEN)   TEST GREATER THAN MAXIMUM ALLOWED            
         BH    EXIT                YES - EXIT                                   
         STCM  R1,3,CTILEN         SET NEW OUTPUT RECORD LENGTH                 
         LA    RF,4(R1)            RF=RECORD LENGTH+4                           
         L     R2,AIOAREA                                                       
         STCM  RF,3,0(R2)          SET NEW TOTAL RECORD LENGTH                  
         LA    R0,4(R2)            MOVE NEW RECORD TO IOAREA                    
         LA    RE,IO                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
RFPQ     EQU   X'2F'                                                            
RLPQ     EQU   X'2D'                                                            
MAXLEN   EQU   1000                                                             
                                                                                
WORKD    DSECT                                                                  
FLAG1    DS    X                                                                
FLAG2    DS    X                                                                
AUTH     DS    XL2                                                              
IO       DS    XL(MAXLEN+256)      MAX RECORD LENGTH PLUS A BIT                 
WORKL    EQU   *-WORKD                                                          
                                                                                
*CTCONWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTCONRLP  06/10/00'                                      
         END                                                                    
