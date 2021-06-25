*          DATA SET NEUTTEST   AT LEVEL 020 AS OF 09/25/00                      
*PHASE NEUTTESA NEUTTEST                                                        
*INCLUDE NEUTILS                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE MOBILE                                                                 
         TITLE 'UTTEST - TESTING FOR UTILITY PROGRAMS'                          
         PRINT NOGEN                                                            
NEUTTEST CSECT                                                                  
         NBASE 0,**UTST**,=V(ELWORK)                                            
*                                                                               
         XC    PROF,PROF                                                        
         XC    FULL,FULL                                                        
*                                                                               
READLOOP GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    DODATES                                                          
*                                                                               
         GOTO1 =V(PRINT),PARA,C-1,=C'BL01'                                      
*                                                                               
RLOOP2   CLC   C(5),=C'DATE1'                                                   
         BNE   RLOOP4                                                           
         MVC   DAT1,C+6                                                         
         GOTO1 =V(DATCON),PARA,(0,C+6),(2,NBCMPSTR)                             
         B     READLOOP                                                         
RLOOP4   CLC   C(5),=C'DATE2'                                                   
         BNE   RLOOP6                                                           
         MVC   DAT2,C+6                                                         
         GOTO1 =V(DATCON),PARA,(0,C+6),(2,NBCMPEND)                             
         B     READLOOP                                                         
RLOOP6   CLC   C(3),=C'PER'                                                     
         BNE   RLOOP8                                                           
         MVC   HALF,C+4                                                         
         B     READLOOP                                                         
RLOOP8   CLC   C(3),=C'DOW'                                                     
         BNE   RLOOP10                                                          
         PACK  DUB,C+4(1)                                                       
         CVB   R1,DUB                                                           
         STC   R1,FULL                                                          
         B     READLOOP                                                         
RLOOP10  CLC   C(4),=C'SPER'                                                    
         BNE   RLOOP12                                                          
         PACK  DUB,C+5(2)                                                       
         CVB   R1,DUB                                                           
         STC   R1,FULL+1                                                        
         B     READLOOP                                                         
RLOOP12  CLC   C(6),=C'BASMON'                                                  
         BNE   RLOOP14                                                          
         PACK  DUB,C+7(2)                                                       
         CVB   R1,DUB                                                           
         STC   R1,FULL+2                                                        
         B     READLOOP                                                         
RLOOP14  CLC   C(6),=C'BASDAY'                                                  
         BNE   RLOOP16                                                          
         PACK  DUB,C+7(2)                                                       
         CVB   R1,DUB                                                           
         STC   R1,FULL+3                                                        
         B     READLOOP                                                         
*                                                                               
RLOOP16  EQU   *                                                                
         MVC   P(20),=C'******* ERROR'                                          
         GOTO1 =V(PRINT),PARA,PLINE,=C'BL01'                                    
         B     XITNT                                                            
*                                                                               
DODATES  L     R1,=V(NEUTILS)                                                   
         ST    R1,AUTIL             A(GETLIST IN R1)                            
         MVI   AUTIL,1                                                          
*                                                                               
         MVC   PROF+8(1),FULL       DOW                                         
         MVC   PROF+6(1),FULL+2     BASE MON                                    
         MVC   PROF+7(1),FULL+3                                                 
         MVC   PROF+2,FULL+1        SPERTYPE                                    
         CLI   HALF+1,C'S'          IF NOT SPEC PER, SET PROPER CODE            
         BE    DODA8                                                            
         CLI   HALF,C'W'            WEEKS                                       
         BNE   DODA2                                                            
         MVI   PROF+2,4                                                         
         B     DODA8                                                            
DODA2    CLI   HALF+1,C'B'                                                      
         BNE   DODA4                                                            
         MVI   PROF+2,0             BRODCAST MOS                                
         B     DODA8                                                            
DODA4    CLI   HALF+1,C'C'                                                      
         BNE   DODA8                                                            
         MVI   PROF+2,2             CALENDAR MOS                                
*                                                                               
DODA8    LA    RF,400                                                           
         XCEF  MONLIST,(RF)                                                     
         LA    RF,400                                                           
         XCEF  MONLIST2,(RF)                                                    
         LA    R1,100                                                           
         ST    R1,NUMMONS                                                       
*                                                                               
         GOTO1 AUTIL,PARA,HALF,NBCMPSTR,NBCMPEND,NUMMONS,MONLIST,FULL           
         GOTO1 =V(MOBILE),PARA,(100,DATES),(PROF+2,MONLIST2),0,PROF             
*                                                                               
         MVC   P+3(5),=C'BEGIN'                                                 
         GOTO1 =V(PRINT),PARA,PLINE,=C'BL01'                                    
*                                                                               
         LA    R2,MONLIST                                                       
         LA    R3,MONLIST2                                                      
LOOP     OC    0(4,R2),0(R2)                                                    
         BZ    LPPR8                                                            
         GOTO1 =V(DATCON),PARA,(2,0(R2)),(5,P+3)                                
         GOTO1 =V(DATCON),PARA,(2,2(R2)),(5,P+18)                               
         CLC   0(4,R2),0(R3)                                                    
         BE    LPPR2                                                            
         GOTO1 =V(DATCON),PARA,(2,0(R3)),(5,P+33)                               
         GOTO1 =V(DATCON),PARA,(2,2(R3)),(5,P+48)                               
LPPR2    GOTO1 =V(PRINT),PARA,PLINE,=C'BL01'                                    
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         XC    P(132),P                                                         
         B     LOOP                                                             
*                                                                               
LPPR8    OC    0(4,R3),0(R3)        CK IF ANY MOBILES LEFT                      
         BZ    XITNT                                                            
         GOTO1 =V(DATCON),PARA,(2,0(R3)),(5,P+33)                               
         GOTO1 =V(DATCON),PARA,(2,2(R3)),(5,P+48)                               
         GOTO1 =V(PRINT),PARA,PLINE,=C'BL01'                                    
         LA    R3,4(R3)                                                         
         XC    P(132),P                                                         
         B     LPPR8                                                            
*                                                                               
XITNT    XC    P(132),P                                                         
         MVC   P+3(8),=C'*****END'                                              
         GOTO1 =V(PRINT),PARA,PLINE,=C'BL01'                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*              STORAGE                                                          
         SPACE 3                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
WORK     DS    CL32                                                             
PARA     DS    6F                                                               
NBCMPSTR DS    CL2                  COMPRESSED START,END                        
NBCMPEND DS    CL2                                                              
NUMMONS  DS    F                                                                
MONLIST  DS    100CL4               LIST OF MONTHS                              
MONLIST2 DS    100CL4               MOBILE LIST OF MONTHS                       
         DS    CL1                                                              
C        DS    CL80                                                             
PLINE    DS    0CL133                                                           
         DS    CL1                                                              
P        DS    CL132                                                            
BLOKLEN  DS    F                                                                
*                                                                               
DATES    DS    0CL12                                                            
DAT1     DS    CL6                                                              
DAT2     DS    CL6                                                              
*                                                                               
DATIN    DS    CL6                                                              
AUTIL    DS    A                                                                
PROF     DS    CL16                 FAKE PROFILE                                
*                                                                               
ELWORK   CSECT                                                                  
         DS    200CL10                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NEUTTEST  09/25/00'                                      
         END                                                                    
