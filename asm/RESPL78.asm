*          DATA SET RESPL78    AT LEVEL 027 AS OF 08/31/95                      
*PHASE T80878A,*                                                                
         TITLE 'T80878 - RESPL78 - EDIT FOR PROJECTION'                         
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESPL78 --- EDIT SCREEN/REQUEST FOR OVERNIGHT PROJECTIOS   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* MAR14/90 (MRR) --- DO NOT ALLOW 'SAME' FOR FROM OR 'TO' BOOK      *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         PRINT NOGEN                                                            
T80878   CSECT                                                                  
         NMOD1 0,**PRED**                                                       
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
         MVI   MAX,1               BOOK                                         
         LA    R2,TITBOOKH                                                      
         GOTO1 VALIBOOK                                                         
         CLC   8(2,R2),=C'SA'                                                   
         BE    EDIT01A                                                          
         MVC   FROM,BOOK                                                        
         LA    R2,FROM                                                          
         TM    0(R2),X'24'       FROM BOOK INVALID IF ESTIMATED (X'20)          
         BZ    EDIT01B              OR PROJECTED(X'04')                         
EDIT01A  EQU   *                                                                
         LA    R2,TITBOOKH                                                      
         MVI   ERROR,INVBOOK                                                    
         B     EXIT                                                             
         SPACE 1                                                                
EDIT01B  EQU   *                                                                
         LA    R2,TITTOH                                                        
         GOTO1 VALIBOOK                                                         
         CLC   8(2,R2),=C'SA'                                                   
         BE    EDIT01C                                                          
         MVC   TO,BOOK                                                          
         LA    R2,TO                                                            
         TM    0(R2),X'08'        'TO BOOK' INVALID IF TIME PERIOD              
         BZ    EDIT01D                                                          
EDIT01C  EQU   *                                                                
         LA    R2,TITTOH                                                        
         MVI   ERROR,INVBOOK                                                    
         B     EXIT                                                             
EDIT01D  EQU   *                                                                
         SPACE 1                                                                
         MVC   BOOK(3),FROM                                                     
         LA    R2,TITUPH                                                        
         GOTO1 ANY                                                              
         XC    DMCB,DMCB           FIND ADDRESS OF UPVAL                        
         MVC   DMCB+4(4),=X'D9000A13'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(R2),UPEL,ACOMFACS                                     
         CLI   DMCB,0                                                           
         BNE   EDIT2                                                            
         MVI   ERROR,235           INVALID UPGRADE                              
         B     EXIT                                                             
         SPACE 1                                                                
EDIT2    LA    RE,UPEL                                                          
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    EDIT10                                                           
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    EDIT10                                                           
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   EDIT10                                                           
         MVC   RAVLNOP2,FROM+1                                                  
         DROP  RE                                                               
         SPACE 1                                                                
EDIT10   LA    R2,TITSTATH         STATION                                      
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
         B     EDIT12                                                           
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
EDIT12   LA    R2,TITSTRTH                                                      
         XC    STRTOPT,STRTOPT                                                  
         CLI   5(R2),0                                                          
         BE    EDIT14                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT14                                                           
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         GOTO1 DATCON,PARAS,(0,WORK),(3,STRTOPT)                                
         SPACE 2                                                                
EDIT14   LA    R2,TITENDH                                                       
         MVC   ENDOPT,=X'FFFFFF'                                                
         CLI   5(R2),0                                                          
         BE    EDIT16                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT16                                                           
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         CLI   PARAS+3,0                                                        
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         GOTO1 DATCON,PARAS,(0,WORK),(3,ENDOPT)                                 
         SPACE 1                                                                
EDIT16   LA    R2,TITDETH          DETAILS                                      
         CLI   5(R2),0                                                          
         BE    EDIT18                                                           
         CLI   8(R2),C'Y'                                                       
         BE    EDIT18                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDIT18                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         SPACE 1                                                                
EDIT18   LA    R2,TITACTH          USE HIGHER OF ACTUAL OR PROJ.                
         CLI   5(R2),0                                                          
         BE    EDIT26                                                           
         CLI   8(R2),C'Y'                                                       
         BE    EDIT26                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDIT26                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         SPACE 2                                                                
EDIT26   LA    R2,TITBOOKH                                                      
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
       ++INCLUDE RESPLD8D                                                       
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
FROM     DS    CL3                                                              
TO       DS    CL3                                                              
UPEL     DS    CL24                                                             
DPLIST   DS    CL20                                                             
         PRINT OFF                                                              
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027RESPL78   08/31/95'                                      
         END                                                                    
