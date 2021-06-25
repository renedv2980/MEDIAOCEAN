*          DATA SET RESPL77    AT LEVEL 018 AS OF 04/14/89                      
*PHASE T80877A,+0                                                               
         TITLE 'T80877 - EDIT FOR O/N TRANSFER'                                 
T80877   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TRED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T808FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         LA    R2,TITSRCEH                                                      
         GOTO1 VALISRCE                                                         
         SPACE                                                                  
** TEMPORARY - ALLOWING THE USE OF SOURCE SRC (PRIMARILY FOR UNIVISION)         
******** CLI   DMSOURCE,C'S'       CAN'T USE SRC HERE                           
******** BNE   *+12                                                             
******** MVI   ERROR,INVSRCE                                                    
******** B     EXIT                                                             
         MVI   MAX,1               BOOK                                         
         LA    R2,TITBOOKH                                                      
         GOTO1 VALIBOOK                                                         
         LA    R2,TITSTATH         STATION                                      
         MVI   OPTION,C'Y'                                                      
         MVC   TITMKT,SPACES                                                    
         OI    TITMKTH+6,X'80'                                                  
         MVI   RDOKOPT,C'Y'                                                     
         GOTO1 VALISTAT                                                         
         LA    R2,TITDPTH                                                       
         GOTO1 ANY                                                              
         XC    DPLIST,DPLIST                                                    
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   E10                                                              
         MVC   DPLIST(16),DPTBL                                                 
         B     E50                                                              
         SPACE 1                                                                
E10      LA    RF,8(R2)            MAKE SURE INPUT IS IN TABLE                  
         ZIC   R1,5(R2)                                                         
E20      LA    RE,DPTBL                                                         
E30      CLC   0(1,RE),0(RF)                                                    
         BE    E40                                                              
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   E30                                                              
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
E40      LA    RF,1(RF)                                                         
         BCT   R1,E20                                                           
         SPACE 1                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)                                                  
         SPACE 2                                                                
E50      LA    R2,TITSTRTH                                                      
         XC    STRTOPT,STRTOPT                                                  
         CLI   5(R2),0                                                          
         BE    E60                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    E60                                                              
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         GOTO1 DATCON,PARAS,(0,WORK),(3,STRTOPT)                                
         SPACE 2                                                                
E60      LA    R2,TITENDH                                                       
         MVC   ENDOPT,=X'FFFFFF'                                                
         CLI   5(R2),0                                                          
         BE    E70                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    E70                                                              
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         GOTO1 DATCON,PARAS,(0,WORK),(3,ENDOPT)                                 
         SPACE 2                                                                
E70      LA    R2,TITBOOKH                                                      
         SPACE 2                                                                
EXIT     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
DPTBL    DC    C'MDERATLWKFNPVSJO' THESE ARE INCLUDED IN ALL LIST               
         DC    C'XYU'              THESE ARE VALID AS INDIVIDUAL DPTS           
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RESPLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESPLD7D                                                       
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
DPLIST   DS    CL20                                                             
         PRINT OFF                                                              
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018RESPL77   04/14/89'                                      
         END                                                                    
