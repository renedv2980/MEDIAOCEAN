*          DATA SET DDQSORTX   AT LEVEL 003 AS OF 05/01/02                      
*PHASE T00A50A                                                                  
         SPACE 1                                                                
*=============================================================*                 
* NOTE THAT THERE IS A CONFLICT BETWEEN 31-BIT TABLE          *                 
* ADDRESSES AND ASCENDING/DESCENDING SORT REQUESTS            *                 
* TO INDICATE YOU ARE A 31-BIT CALLER, SET THE HOB OF RF ON   *                 
* IN THIS CASE, DESCENDING SORTS NEED TO BE REQUESTED IN A    *                 
* MANNER NOT YET DETERMINED.                                  *                 
*=============================================================*                 
         SPACE 2                                                                
*=============================================================*                 
*        PARAMETER LIST                                       *                 
*                                                             *                 
*        PARAM1  BYTE  0      0=ASCENDING, ELSE DESCENDING    *                 
*                BYTES 1-3    A(LIST)                         *                 
*                                                             *                 
*        PARAM2  BYTES 0-3    NO. OF ENTRIES                  *                 
*                                                             *                 
*        PARAM3  BYTE    3    LENGTH OF ENTRY                 *                 
*                                                             *                 
*        PARAM4  BYTE    3    LENGTH OF KEY                   *                 
*                                                             *                 
*        PARAM5  BYTE    3    DISPLACEMENT OF KEY INTO ENTRY  *                 
*=============================================================*                 
         TITLE 'QSORT - FASTER SORT MODULE'                                     
         PRINT NOGEN                                                            
QSORT    CSECT                                                                  
         NMOD1 WORKX-WORKD,**QSRT**                                             
         USING WORKD,RC            RC=A(W/S)                                    
*                                                                               
         MVC   PARMTAB,0(R1)                                                    
         MVI   SORTSW,C'A'         ASSUME ASCENDING SORT                        
         LTR   RF,RF               TEST FOR 31-BIT CALLER                       
         BM    PROC                YES - DESCENDING NOT SUPPORTED               
         CLI   PARMTAB,0           TEST ASCENDING REQUEST                       
         BE    PROC                                                             
         MVI   SORTSW,C'D'                                                      
         EJECT                                                                  
*             PROCESS PARAMETER LIST                                            
*                                                                               
PROC     CLC   NUMELEMS,=F'1'                                                   
         BNH   EXIT                                                             
*                                                                               
         MVC   WORKINS,CHARINS     MOVE CHARACTER INSTRUCTIONS TO WORK          
*                                                                               
PROC1    L     R1,KEYLEN           PREPARE CLC INSTRUCTIONS                     
         BCTR  R1,0                                                             
         STC   R1,CTJV+1                                                        
         STC   R1,CTIV+1                                                        
         L     R2,KEYDISP                                                       
         STC   R2,CTJV+3                                                        
         STC   R2,CTIV+3                                                        
         STC   R2,CTJV+5                                                        
         STC   R2,CTIV+5                                                        
*                                                                               
         L     R1,ELEMLEN          PREPARE MOVE INSTRUCTIONS                    
         BCTR  R1,0                                                             
         STC   R1,VEQTP+1                                                       
         STC   R1,TPEQTJ+1                                                      
         STC   R1,TJEQV+1                                                       
         STC   R1,VEQTJ+1                                                       
         STC   R1,TI1EQTI+1                                                     
         STC   R1,TI1EQV+1                                                      
*                                                                               
         STC   R1,PARTXC1+1        PREPARE XC INSTRUCTIONS                      
         STC   R1,PARTXC2+1                                                     
*                                                                               
         SR    R9,R9               FSORT(0,NUMELEMS-1)                          
         L     RA,NUMELEMS                                                      
         BCTR  RA,0                                                             
         BAS   RE,FSORT                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
*                      FSORT(LOW,HIGH)                                          
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = SCRATCH PAD REGISTER                                              
*        R3 = SCRATCH PAD REGISTER                                              
*        R4 = NOT USED                                                          
*        R5 = J                                                                 
*        R6 = P                    LOCAL VARIABLES                              
*        R7 = Q                                                                 
*        R8 = STACK POINTER                                                     
*        R9 = LOW                  FUNCTION ARGUMENTS                           
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
FSORT    NTR1                                                                   
         LR    R6,R9               MOVE ARGS LOW,HIGH TO LOCAL P,Q              
         LR    R7,RA                                                            
         LA    R8,STACK                                                         
FSORT1   LR    R1,R7               IF Q - P ( 16                                
         SR    R1,R6                                                            
         C     R1,=F'16'                                                        
         BNL   FSORT1A                                                          
         LR    R9,R6               THEN ISORT(P,Q)                              
         LR    RA,R7                                                            
         BAS   RE,ISORT                                                         
         B     FSORT10                                                          
FSORT1A  LA    R5,1(R7)            ELSE J = Q + 1                               
         LR    R9,R6               PARTITION(P,J)                               
         LR    RA,R5                                                            
         BAS   RE,PART                                                          
         LR    R5,RA                                                            
         LR    R1,R5               IF J - P ( Q - J                             
         SR    R1,R6                                                            
         LR    R2,R7                                                            
         SR    R2,R5                                                            
         CR    R1,R2                                                            
         BNL   FSORT2                                                           
         LA    R3,1(R5)            THEN STACK(TOP) = J + 1                      
         ST    R3,0(R8)                                                         
         ST    R7,4(R8)            STACK(TOP+1) = Q                             
         LR    R7,R5               Q = J - 1                                    
         BCTR  R7,0                                                             
         B     FSORT3                                                           
FSORT2   ST    R6,0(R8)            ELSE STACK(TOP) = P                          
         LR    R3,R5               STACK(TOP+1) = J - 1                         
         BCTR  R3,0                                                             
         ST    R3,4(R8)                                                         
         LA    R6,1(R5)                                                         
FSORT3   LA    R8,8(R8)            BUMP STACK TO STACK(TOP+2)                   
         B     FSORT1                                                           
FSORT10  LA    R1,STACK            IF STACK EMPTY                               
         CR    R8,R1                                                            
         BE    EXIT                THEN EXIT                                    
         S     R8,=F'8'            BACK UP STACK TO STACK(TOP-2)                
         L     R6,0(R8)            P = STACK(TOP)                               
         L     R7,4(R8)            Q = STACK(TOP+1)                             
         B     FSORT1                                                           
         EJECT                                                                  
*                      PARTITION(P,J)                                           
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = NOT USED                                                          
*        R4 = NOT USED                                                          
*        R5 = I                    LOCAL VARIABLES                              
*        R6 = TABLE(P)                                                          
*        R7 = TABLE(I)                                                          
*        R8 = TABLE(J)                                                          
*        R9 = P                    FUNCTION ARGUMENTS                           
*        RA = J                                                                 
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
PART     NTR1                                                                   
         L     R6,ATABLE           POINT R6 TO TABLE(P)                         
         LR    R1,R9                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R6,R1                                                            
         LR    R5,R9               I = P                                        
         LR    R7,R6               POINT R7 TO TABLE(I)                         
         L     R8,ATABLE           POINT R8 TO TABLE(J)                         
         LR    R1,RA                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R8,R1                                                            
         EX    R0,VEQTP            V = TABLE(P)                                 
*                                                                               
PART1    BCTR  RA,0                J = J - 1                                    
         S     R8,ELEMLEN          BACK UP TABLE(J)                             
         CLI   SORTSW,C'A'         TEST ASCENDING SORT                          
         BNE   PART1A                                                           
         EX    R0,CTJV             FOR ASCENDING SORT                           
         BH    PART1               REPEAT UNTIL TABLE(J) (= V                   
         B     PART2                                                            
PART1A   EX    R0,CTJV             FOR DESCENDING SORT                          
         BL    PART1               REPEAT UNTIL TABLE(J) )= V                   
*                                                                               
PART2    LA    R5,1(R5)            I = I + 1                                    
         A     R7,ELEMLEN          BUMP TABLE(I)                                
         CR    R5,RA               REPEAT UNTIL I )= J                          
         BNL   PART3                                                            
         CLI   SORTSW,C'A'         TEST ASCENDING SORT                          
         BNE   PART2A                                                           
         EX    R0,CTIV             OR FOR ASCENDING SORT                        
         BNH   PART2               UNTIL TABLE(I) ) V                           
         B     PART3                                                            
PART2A   EX    R0,CTIV             OR FOR DESCENDING SORT                       
         BNL   PART2               UNTIL TABLE(I) ( V                           
*                                                                               
PART3    CR    R5,RA               IF I ( J                                     
         BNL   PART4                                                            
         EX    R0,PARTXC1          SWITCH TABLE(I),TABLE(J)                     
         EX    R0,PARTXC2                                                       
         EX    R0,PARTXC1                                                       
         B     PART1               REPEAT LOOP                                  
*                                                                               
PART4    EX    R0,TPEQTJ           ELSE TABLE(P) = TABLE(J)                     
         EX    R0,TJEQV            TABLE(J) = V                                 
*        DC    H'0'                                                             
         XIT1  REGS=(RA)           RETURN J                                     
         EJECT                                                                  
*                      ISORT(LOW,HIGH)                                          
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = NOT USED                                                          
*        R4 = I                    LOCAL VARIABLES                              
*        R5 = J                                                                 
*        R6 = TABLE(I+1)                                                        
*        R7 = TABLE(I)                                                          
*        R8 = TABLE(J)                                                          
*        R9 = LOW                  FUNCTON ARGUMENTS                            
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
ISORT    NTR1                                                                   
         LA    R5,1(R9)            J = LOW + 1                                  
         L     R8,ATABLE           POINT R8 TO TABLE(J)                         
         LR    R1,R5                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R8,R1                                                            
*                                                                               
ISORT1   CR    R5,RA               WHILE J (= HIGH                              
         BH    EXIT                                                             
         EX    R0,VEQTJ            V = TABLE(J)                                 
         LR    R4,R5               I = J - 1                                    
         BCTR  R4,0                                                             
         LR    R6,R8               POINT R6 TO TABLE(I+1)                       
         LR    R7,R8               POINT R7 TO TABLE(I)                         
         S     R7,ELEMLEN                                                       
*                                                                               
ISORT2   CR    R4,R9               WHILE I )= LOW                               
         BL    ISORT4                                                           
         CLI   SORTSW,C'A'         TEST ASCENDING SORT                          
         BNE   ISORT2A                                                          
         EX    R0,CTIV             AND FOR ASCENDING SORT                       
         BNH   ISORT4              TABLE(I) ) V                                 
         B     ISORT3                                                           
ISORT2A  EX    R0,CTIV             AND FOR DESCENDING SORT                      
         BNL   ISORT4              TABLE(I) ( V                                 
*                                                                               
ISORT3   EX    R0,TI1EQTI          TABLE(I+1) = TABLE(I)                        
         BCTR  R4,0                I = I - 1                                    
         S     R6,ELEMLEN          BACKUP TABLE(I+1)                            
         S     R7,ELEMLEN          BACKUP TABLE(I)                              
         B     ISORT2                                                           
*                                                                               
ISORT4   EX    R0,TI1EQV           TABLE(I+1) = V                               
         LA    R5,1(R5)            J = J + 1                                    
         A     R8,ELEMLEN          BUMP TABLE(J)                                
         B     ISORT1                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SPECIAL CHARACTER INSTRUCTIONS                                         
*                                                                               
CHARINS  MVC   V(0),0(R6)          V = TABLE(P)                                 
         CLC   0(0,R8),V           COMPARE TABLE(J) TO V                        
         CLC   0(0,R7),V           COMPARE TABLE(I) TO V                        
         XC    0(0,R7),0(R8)       SWITCH TABLE(I),TABLE(J)                     
         XC    0(0,R8),0(R7)                                                    
         MVC   0(0,R6),0(R8)       TABLE(P) = TABLE(J)                          
         MVC   0(0,R8),V           TABLE(J) = V                                 
         MVC   V(0),0(R8)          V = TABLE(J)                                 
         MVC   0(0,R6),0(R7)       TABLE(I+1) = TABLE(I)                        
         MVC   0(0,R6),V           TABLE(I+1) = V                               
         EJECT                                                                  
* WORKING STORAGE AREA                                                          
*                                                                               
WORKD    DSECT                                                                  
*                                  V MUST EQUAL ZERO PAST REGISTER C            
V        DS    256C                V IS THE PARTITION ELEMENT                   
*                                                                               
PARMTAB  DS    0CL24                                                            
ATABLE   DS    F                                                                
NUMELEMS DS    F                                                                
ELEMLEN  DS    F                                                                
KEYLEN   DS    F                                                                
KEYDISP  DS    F                                                                
ABACKUP  DS    F                                                                
*                                                                               
STACK    DS    64F                                                              
*                                                                               
WORKINS  DS    0CL60                                                            
VEQTP    DS    3H                  V = TABLE(P)                                 
CTJV     DS    3H                  COMPARE TABLE(J) TO V                        
CTIV     DS    3H                  COMPARE TABLE(I) TO V                        
PARTXC1  DS    3H                  SWITCH TABLE(I),TABLE(J)                     
PARTXC2  DS    3H                                                               
TPEQTJ   DS    3H                  TABLE(P) = TABLE(J)                          
TJEQV    DS    3H                  TABLE(J) = V                                 
VEQTJ    DS    3H                  V = TABLE(J)                                 
TI1EQTI  DS    3H                  TABLE(I+1) = TABLE(I)                        
TI1EQV   DS    3H                  TABLE(I+1) = V                               
*                                                                               
SORTSW   DS    C                   A=ASCENDING SORT/D=DESCENDING                
*                                                                               
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDQSORTX  05/01/02'                                      
         END                                                                    
