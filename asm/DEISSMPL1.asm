*          DATA SET DEISSMPL1  AT LEVEL 039 AS OF 10/23/01                      
*PHASE DEISSMP9                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE OKSUB                                                                  
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
*********MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
*********BASR  RE,RF               PRINT THE FIRST LINE                         
*                                                                               
         BAS   RE,MYSUB            CALL USELESS SUBROUTINE                      
*                                                                               
         GOTO1 =V(OKSUB),DMCB,STUDTAB,1,STUDTBLQ,L'NAME,0                       
*                                                                               
         XC    X'54'(2,RF),X'54'(RF)                                            
*                                                                               
         GOTO1 =V(OKSUB),DMCB,STUDTAB,5,STUDTBLQ,L'NAME,0                       
*                                                                               
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
         USING STUDTABD,R3                                                      
LOOP     MVC   PRNTNAME,NAME       PUT STUDENT NAME INTO PRINT LINE             
         MVC   PRNTPHON,PHONE      PUT PHONE NUMBER INTO PRINT LINE             
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF               PRINT A LINE                                 
         LA    R3,STUDTBLQ(R3)     BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
*                                                                               
*********MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 =V(OKSUB),DMCB,STUDTAB,6,STUDTBLQ,12,0                           
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
STUDTAB  DS    0X                                                               
*&&DO                                                                           
         DC    CL10'ALLEN',C'6125'                                              
         DC    CL10'OANA',C'6724'                                               
         DC    CL10'HAN',C'6747'                                                
         DC    CL10'MOSHE',C'6127'                                              
         DC    CL10'NEVENA',C'6674'                                             
         DC    CL10'MAN',C'6679'                                                
*&&                                                                             
         DC    X'FF'               EOT                                          
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
STUDTABD DSECT                                                                  
NAME     DS    CL10                                                             
PHONE    DS    CL4                                                              
STUDTBLQ EQU   *-STUDTABD                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
         ORG   P                                                                
PRNTNAME DS    CL10                                                             
         DS    CL5                                                              
PRNTPHON DS    CL4                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039DEISSMPL1 10/23/01'                                      
         END                                                                    
