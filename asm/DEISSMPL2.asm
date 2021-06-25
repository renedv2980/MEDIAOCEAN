*          DATA SET DEISSMPL2  AT LEVEL 029 AS OF 08/17/00                      
*PHASE DEISSMPA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'SAMPLE PROGRAM'                                                 
SAMPLE   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*SAMPLE*,=V(REGSAVE)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         MVC   TITLE(14),=C'SAMPLE PROGRAM'                                     
*                                                                               
         MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
         GOTO1 =V(PRINTER)         PRINT THE FIRST LINE                         
*                                                                               
* PRINT THE STUDENT NAMES                                                       
*                                                                               
         SR    R4,R4               INITIALIZE STUDENT COUNTER                   
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
LOOP     MVC   P(10),0(R3)         PUT STUDENT NAME INTO PRINT LINE             
         GOTO1 =V(PRINTER)         PRINT ONE NAME                               
         AHI   R4,1                INCREMENT STUDENT COUNTER                    
         LA    R3,L'STUDTAB(R3)    BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
*                                                                               
         LR    RA,R4               USE A DIFFERENT REGISTER, FOR FUN            
         CVD   RA,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSG1+10(1),DUB      PUT NUMBER OF STUDENTS INTO MESSAGE          
         MVC   P(L'MSG1),MSG1                                                   
         GOTO1 =V(PRINTER)         PRINT THE MESSAGE                            
*                                                                               
         MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         GOTO1 =V(PRINTER)         PRINT FINAL MESSAGE                          
*                                                                               
* EXIT THE PROGRAM                                                              
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(SAMPLE),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
MSG1     DC    C'THERE ARE X STUDENTS IN THE CLASS'                             
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
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029DEISSMPL2 08/17/00'                                      
         END                                                                    
