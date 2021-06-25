*          DATA SET DEISSMPL   AT LEVEL 028 AS OF 09/06/00                      
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
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(SAMPLE),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN     DS    0H                                                               
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
*                                                                               
* THE FOLLWING TABLE SHOWS A SEVERAL WAYS OF DEFINING A LENGTH,                 
* EACH OF WHICH RESULTS IN A LENGTH OF 10.                                      
*                                                                               
STUDTAB  DS    0CL10                                                            
         DC    CL10'ARUP       '   NAME IS TRUNCATED!                           
         DC    CL(L'STUDTAB)'TIM'  THIS IS A "SOFT" LENGTH                      
         DC    C'ALEX      '       I PADDED THIS NAME OUT MYSELF                
         DC    CL10'HENRY     '    DON'T NEED BOTH PADDING AND LENGTH!          
         DC    CL10'JOHN'          ASSEMBLER PADS THIS ONE OUT FOR YOU          
         DC    CL10'YURIY'         ASSEMBLER PADS THIS ONE OUT FOR YOU          
         DC    X'FF'               END-OF-TABLE MARKER                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DEISSMPL  09/06/00'                                      
         END                                                                    
