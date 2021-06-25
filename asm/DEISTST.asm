*          DATA SET DEISTST    AT LEVEL 016 AS OF 06/11/01                      
*PHASE DEISTSTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'SAMPLE PROGRAM'                                                 
DEISTST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*SAMPLE*,=V(REGSAVE)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DEISTST),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         LA    R7,MYAREA                                                        
         USING MYD,R7                                                           
         MVC   F1,=F'1'                                                         
*                                                                               
         LA    R8,MYAREA                                                        
         USING MYD,R8                                                           
         MVC   F1,=F'1'                                                         
*                                                                               
         LA    R7,MYAREA                                                        
         USING MYD,R7                                                           
         MVC   F2,=F'2'                                                         
*                                                                               
         LA    R2,1(R3,R4)                                                      
         LA    R2,1(,R4)                                                        
         LA    R2,1(R3)                                                         
         LA    R2,1                                                             
*                                                                               
         AHI   R0,1                                                             
         AHI   R0,-1                                                            
         LHI   R0,0                                                             
         MHI   R0,32767                                                         
         MHI   R0,-32768                                                        
         CHI   R0,1                                                             
         CHI   R0,-1                                                            
         TMH   R0,X'0000'                                                       
         TMH   R0,X'8000'                                                       
         TML   R0,X'FFFF'                                                       
         TMH   R0,X'0008'                                                       
*                                                                               
         BRCT  R3,MAIN                                                          
         BRAS  RE,MAIN                                                          
         BRXH  R1,R6,MAIN                                                       
         BRXLE R1,R6,MAIN                                                       
*                                                                               
         BCT   R3,MAIN                                                          
         BAS   RE,MAIN                                                          
         BXH   R1,R6,MAIN                                                       
         BXLE  R1,R6,MAIN                                                       
*                                                                               
         J     *-8                                                              
         JE    *-8                                                              
         JNP   *-8                                                              
         JNOP  LOOP                                                             
         JNE   LOOP                                                             
         JP    LOOP                                                             
*                                                                               
         B     *-8                                                              
         BE    *-8                                                              
         BNP   *-8                                                              
         NOP   LOOP                                                             
         BNE   LOOP                                                             
         BP    LOOP                                                             
*                                                                               
         MVC   TITLE(14),=C'SAMPLE PROGRAM'                                     
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
         MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
         BASR  RE,RF               PRINT THE FIRST LINE                         
*                                                                               
         BAS   RE,MYSUB            CALL USELESS SUBROUTINE                      
*                                                                               
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
LOOP     MVC   P(10),0(R3)         PUT STUDENT NAME INTO PRINT LINE             
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF               PRINT A LINE                                 
         LA    R3,L'STUDTAB(R3)    BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
*                                                                               
         MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
* EXIT THE PROGRAM                                                              
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
MYSUB    NTR1                                                                   
*                                                                               
*        DO NOTHING                                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
DMCB     DS    6F                                                               
         SPACE 2                                                                
         DS    0D                                                               
MYAREA   DS    (MYDLQ)X                                                         
*                                                                               
* THE FOLLWING TABLE SHOWS A SEVERAL WAYS OF DEFINING A LENGTH,                 
* EACH OF WHICH RESULTS IN A LENGTH OF 10.                                      
*                                                                               
STUDTAB  DS    0CL10                                                            
         DC    C'FRANK     '       I PADDED THIS NAME OUT MYSELF                
         DC    CL10'CHRISTOPHER'   NAME IS TRUNCATED!                           
         DC    CL(L'STUDTAB)'RAJIV'  THIS IS A "SOFT" LENGTH                    
         DC    CL10'MOHAMAD   '    DON'T NEED BOTH PADDING AND LENGTH!          
         DC    CL10'YELENA'        ASSEMBLER PADS THIS ONE OUT FOR YOU          
         DC    X'FF'               END-OF-TABLE MARKER                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
F1       DS    F                                                                
F2       DS    F                                                                
F3       DS    F                                                                
F4       DS    F                                                                
MYDLQ    EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DEISTST   06/11/01'                                      
         END                                                                    
