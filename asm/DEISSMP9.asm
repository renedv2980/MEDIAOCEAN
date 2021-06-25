*          DATA SET DEISSMP9   AT LEVEL 016 AS OF 09/25/00                      
*PHASE DEISSMP9                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE CARDS                                                                  
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
         EJECT                                                                  
MAIN     DS    0H                                                               
*                                                                               
         MVC   TITLE(15),=C'WIN THE LOTTERY'                                    
*                                                                               
* PROCESS THE DATA                                                              
*                                                                               
         BAS   RE,READCRDS         READ THE INPUT CARDS                         
*                                                                               
         BAS   RE,PRINTIT          PRINT THE NAMES AND WINNINGS                 
*                                                                               
* EXIT THE PROGRAM                                                              
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
READCRDS NTR1                                                                   
*                                                                               
         LA    R3,STUDTAB          POINT TO START OF STUDENT TABLE              
         USING STUDTABD,R3                                                      
NEXTCARD GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    DOCARDSX            YES                                          
*                                                                               
         BAS   RE,GETMONEY         CONVERT THE CARD INTO BINARY CENTS           
         MVC   MONEY,FULL          SAVE THE VALUE IN THE TABLE                  
         LA    R3,STUDTBLQ(R3)     BUMP TO NEXT STUDENT                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   NEXTCARD            NO, READ ANOTHER NUMBER                      
         DROP  R3                                                               
*                                                                               
DOCARDSX XIT1                                                                   
         SPACE 5                                                                
GETMONEY NTR1                                                                   
*                                                                               
         LA    R2,CARD+L'CARD-1    POINT R2 TO END OF CARD                      
         CLI   0(R2),C' '                                                       
         BH    *+10                                                             
         BCTR  R2,0                BACK UP TO LAST SIGNIFICANT CHAR.            
         B     *-10                                                             
         LA    R2,1(R2)            POINT R2 TO FIRST BLANK                      
*                                                                               
         LA    R0,CARD                                                          
         SR    R2,R0               R2 = NO. OF SIGNIFICANT CHARACTERS           
         BCTR  R2,0                FOR EX                                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD(0)                                                      
         CVB   R0,DUB              R0 = WINNINGS (IN DOLLARS)                   
         MHI   R0,100              R0 = WINNINGS (IN PENNIES)                   
         ST    R0,FULL             RETURN THE VALUE TO THE CALLER               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
PRINTIT  NTR1                                                                   
*                                                                               
         MVC   P(31),=C'THE STUDENTS IN THIS CLASS ARE:'                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R3,STUDTAB          POINT TO THE STUDENT TABLE                   
         USING STUDTABD,R3                                                      
LOOP     MVC   PRTNAME,NAME        PRINT THE STUDENT'S INFO                     
         MVC   PRTPHONE,PHONE                                                   
         EDIT  MONEY,PRTMONEY,2,FLOAT=$,COMMAS=YES                              
         GOTO1 =V(PRINTER)                                                      
         LA    R3,STUDTBLQ(R3)     BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LOOP                NO                                           
         DROP  R3                                                               
*                                                                               
         MVC   P(18),=C'THAT''S ALL, FOLKS!'                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL17                                                             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
CARD     DS    CL80                                                             
*                                                                               
STUDTAB  DS    0F                                                               
         DC    CL(L'NAME)'ALEX',C'6497',F'0'                                    
         DC    CL(L'NAME)'HENRY',C'6587',F'0'                                   
         DC    CL(L'NAME)'JOHN',C'6531',F'0'                                    
         DC    CL(L'NAME)'TIM',C'6306',F'0'                                     
         DC    CL(L'NAME)'YURIY',C'6581',F'0'                                   
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 5                                                                
STUDTABD DSECT                                                                  
NAME     DS    CL10                                                             
PHONE    DS    CL4                                                              
MONEY    DS    F                                                                
STUDTBLQ EQU   *-STUDTABD                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 5                                                                
DPRINT   DSECT                                                                  
         ORG   P                                                                
PRTNAME  DS    XL10                                                             
         DS    CL2                                                              
PRTPHONE DS    CL4                                                              
         DS    CL2                                                              
PRTMONEY DS    CL16                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DEISSMP9  09/25/00'                                      
         END                                                                    
