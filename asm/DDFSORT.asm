*          DATA SET DDFSORT    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T00A12,+0                                                                
         TITLE 'FSORT - FAST RECURSIVE SORT MODULE'                             
         SPACE 3                                                                
*        PARAMETER LIST                                                         
         SPACE 1                                                                
*        PARAM1  BYTE  0      X'00' = ASCENDING,X'FF' = DESCENDING              
*                BYTES 1-3    A(LIST)                                           
         SPACE 1                                                                
*        PARAM2  BYTES 0-3    NO. OF ENTRIES                                    
         SPACE 1                                                                
*        PARAM3  BYTE    3    LENGTH OF ENTRY                                   
         SPACE 1                                                                
*        PARAM4  BYTE    3    LENGTH OF KEY                                     
         SPACE 1                                                                
*        PARAM5  BYTE    3    DISPLACEMENT OF KEY INTO ENTRY                    
         SPACE 1                                                                
*        PARAM6  BYTES 0-3    ADDRESS OF AREA USED TO BACKUP LIST               
         SPACE 3                                                                
         PRINT NOGEN                                                            
FSORT    CSECT                                                                  
         NMOD1 WORKX-WORKD,**FSRT**                                             
         USING WORKD,RC            RC=A(W/S)                                    
         MVC   PARMTAB,0(R1)                                                    
         EJECT                                                                  
*             PROCESS PARAMETER LIST                                            
*                                                                               
PROC     CLC   NUMELEMS,=F'1'                                                   
         BNH   EXIT                                                             
*                                                                               
         L     R1,KEYLEN           PREPARE CLC INSTRUCTIONS                     
         BCTR  R1,0                                                             
         STC   R1,MERGECLC+1                                                    
         STC   R1,BUBCLC+1                                                      
         L     R2,KEYDISP                                                       
         STC   R2,MERGECLC+3                                                    
         STC   R2,MERGECLC+5                                                    
         STC   R2,BUBCLC+3                                                      
         STC   R2,BUBCLC+5                                                      
*                                                                               
         MVI   MERGEBC+1,X'D0'     SET BRANCHES TO BNH IF ASCENDING             
         MVI   BUBBC+1,X'D0'                                                    
         CLI   PARMTAB,X'00'                                                    
         BE    PROC1                                                            
         MVI   MERGEBC+1,X'B0'     SET BRANCHES TO BNL IF DESCENDING            
         MVI   BUBBC+1,X'B0'                                                    
*                                                                               
PROC1    L     R1,ELEMLEN          PREPARE MOVE INSTRUCTIONS                    
         BCTR  R1,0                                                             
         STC   R1,MERGE3+1                                                      
         STC   R1,MERGE4+1                                                      
*                                                                               
         STC   R1,BUBXC1+1         PREPARE XC INSTRUCTIONS                      
         STC   R1,BUBXC2+1                                                      
         STC   R1,BUBXC3+1                                                      
*                                                                               
         SR    R8,R8               MSORT(0,NUMELEMS-1)                          
         L     RA,NUMELEMS                                                      
         BCTR  RA,0                                                             
         BAS   RE,MSORT                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
*                      MSORT(LOW,HIGH)                                          
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = NOT USED                                                          
*        R2 = NOT USED                                                          
*        R3 = NOT USED                                                          
*        R4 = NOT USED                                                          
*        R5 = LOW                  LOCAL VARIABLES                              
*        R6 = MID                                                               
*        R7 = HIGH                                                              
*        R8 = LOW                  FUNCTION ARGUMENTS                           
*        R9 = MID                                                               
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
MSORT    NTR1                                                                   
         LR    R1,RA               IF HIGH - LOW < 16                           
         SR    R1,R8                                                            
         C     R1,=F'16'                                                        
         BNL   MSORT1                                                           
         BAS   RE,BUBBLE           THEN BUBBLE(LOW,HIGH)                        
         B     EXIT                                                             
*                                  ELSE ...                                     
MSORT1   LR    R5,R8               MOVE ARGUMENTS TO LOCAL REGISTERS            
         LR    R6,R9                                                            
         LR    R7,RA                                                            
         LA    R6,0(R5,R7)         MID = (LOW + HIGH)/2                         
         SRL   R6,1                                                             
         LR    RA,R6               MSORT(LOW,MID)                               
         BAS   RE,MSORT                                                         
         LA    R8,1(R6)            MSORT(MID+1,HIGH)                            
         LR    RA,R7                                                            
         BAS   RE,MSORT                                                         
         LR    R8,R5               MERGE(LOW,MID,HIGH)                          
         LR    R9,R6                                                            
         BAS   RE,MERGE                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                      MERGE(LOW,MID,HIGH)                                      
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = I                    LOCAL VARIABLES                              
*        R4 = J                                                                 
*        R5 = BACKUP()                                                          
*        R6 = TABLE(I)                                                          
*        R7 = TABLE(J)                                                          
*        R8 = LOW                  FUNCTION ARGUMENTS                           
*        R9 = MID                                                               
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
MERGE    NTR1                                                                   
         L     R5,ABACKUP          POINT R5 TO BACKUP(LOW)                      
         LR    R1,R8                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R5,R1                                                            
         ST    R5,BACKLOW                                                       
         L     R6,ATABLE           POINT R6 TO TABLE(LOW)                       
         AR    R6,R1                                                            
         ST    R6,TABLOW                                                        
         L     R7,ATABLE           POINT R7 TO TABLE(MID+1)                     
         LA    R1,1(R9)                                                         
         MH    R1,ELEMLEN+2                                                     
         AR    R7,R1                                                            
         LR    R3,R8               I = LOW                                      
         LA    R4,1(R9)            J = MID + 1                                  
*                                                                               
MERGE1   CR    R3,R9               WHILE I (= MID                               
         BNH   MERGE2                                                           
         CR    R4,RA               OR J (= HIGH                                 
         BH    MERGE10                                                          
*                                                                               
MERGE2   CR    R3,R9               IF I ) MID                                   
         BH    MERGE3                                                           
         CR    R4,RA               OR J (= HIGH AND                             
         BH    MERGE4                                                           
MERGECLC CLC   0(0,R6),0(R7)       TABLE(I),TABLE(J) ARE NOT IN ORDER           
MERGEBC  BC    0,MERGE4                                                         
*                                                                               
MERGE3   MVC   0(0,R5),0(R7)       THEN BACKUP POINTER = TABLE(J)               
         LA    R4,1(R4)            J = J + 1                                    
         A     R7,ELEMLEN          BUMP TABLE(J)                                
         B     MERGE5                                                           
*                                                                               
MERGE4   MVC   0(0,R5),0(R6)       ELSE BACKUP POINTER = TABLE(I)               
         LA    R3,1(R3)            I = I + 1                                    
         A     R6,ELEMLEN          BUMP TABLE(I)                                
*                                                                               
MERGE5   A     R5,ELEMLEN          BUMP BACKUP POINTER TO NEXT ENTRY            
         B     MERGE1                                                           
*                                                                               
MERGE10  L     RF,TABLOW           MOVE BACKUP BACK TO TABLE                    
         L     RE,BACKLOW                                                       
         LA    R1,1(RA)                                                         
         SR    R1,R8                                                            
         MH    R1,ELEMLEN+2                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                      BUBBLE(LOW,HIGH)                                         
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = I                    LOCAL VARIABLES                              
*        R4 = J                                                                 
*        R5 = BACKUP()                                                          
*        R6 = TABLE(I)                                                          
*        R7 = TABLE(J)                                                          
*        R8 = LOW                  FUNCTION ARGUMENTS                           
*        R9 = MID                                                               
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
BUBBLE   NTR1                                                                   
         LR    R3,R8               I = LOW                                      
         L     R6,ATABLE           POINT R6 TO TABLE(I)                         
         LR    R1,R3                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R6,R1                                                            
*                                                                               
BUBBLE1  CR    R3,RA               WHILE I ( HIGH                               
         BNL   EXIT                                                             
         LA    R4,1(R3)            J = I + 1                                    
         LR    R7,R6               POINT R7 TO TABLE(J)                         
         A     R7,ELEMLEN                                                       
*                                                                               
BUBBLE2  CR    R4,RA               WHILE J (= HIGH                              
         BH    BUBBLE4                                                          
*                                                                               
BUBCLC   CLC   0(0,R6),0(R7)       IF TABLE(I),TABLE(J) NOT IN ORDER            
BUBBC    BC    0,BUBBLE3                                                        
BUBXC1   XC    0(0,R6),0(R7)       THEN SWITCH THEM                             
BUBXC2   XC    0(0,R7),0(R6)                                                    
BUBXC3   XC    0(0,R6),0(R7)                                                    
*                                                                               
BUBBLE3  LA    R4,1(R4)            J = J + 1                                    
         A     R7,ELEMLEN          BUMP TABLE(J)                                
         B     BUBBLE2                                                          
*                                                                               
BUBBLE4  LA    R3,1(R3)            I = I + 1                                    
         A     R6,ELEMLEN          BUMP TABLE(I)                                
         B     BUBBLE1                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE AREA                                                          
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
PARMTAB  DS    0CL24                                                            
ATABLE   DS    F                                                                
NUMELEMS DS    F                                                                
ELEMLEN  DS    F                                                                
KEYLEN   DS    F                                                                
KEYDISP  DS    F                                                                
ABACKUP  DS    F                                                                
*                                                                               
BACKLOW  DS    A                                                                
TABLOW   DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023DDFSORT   05/01/02'                                      
         END                                                                    
