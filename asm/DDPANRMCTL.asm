*          DATA SET DDPANRMCTL AT LEVEL 001 AS OF 04/28/09                      
*PHASE PANRMCTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'RELOS: GENERATE PAN#2 CONTROL CARDS'                            
PANRMCT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PANRMCT,=V(REGSAVE)                                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'++',CARD         IS IT A ++ CARD?                             
         BE    *+6                                                              
         DC    H'0'                WHAT DID PANACEA GENERATE?                   
*                                                                               
         MVC   UPDDATA,CARD+9      POSIT ++UPDATE                               
         LA    R3,CARD+9           ADVANCE PAST ++UPDATE                        
         CLC   =C'++UPDATE ',CARD  TRUE?                                        
         BE    PANV10              YES: PROCEED                                 
*                                                                               
         MVC   ADDDATA,CARD+6      ASSUME ++ADD                                 
         LA    R3,CARD+6           ADVANCE PAST ++ADD                           
         CLC   =C'++ADD ',CARD     TRUE?                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE EITHER ++UPDATE OR ++ADD             
*                                                                               
PANV10   DS    0H                  R3 = A(START OF RM MEMBER NAME)              
         CLI   0(R3),C' '                                                       
         BNE   *+12                FOUND FIRST NONBLANK                         
         LA    R3,1(R3)            ADVANCE POINTER                              
         B     PANV10                                                           
*                                                                               
         LA    R4,1(R3)            BUMP PAST END OF NAME                        
         CLI   0(R4),C','                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         SR    R4,R3               LENGTH OF NAME                               
         BCTR  R4,0                                                             
         MVC   RELOMEM,SPACES      ASSURE TRAILING BLANKS                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   RELOMEM(0),0(R3)    RM MEMBER NAME                               
*                                                                               
*  CONSTRUCT ++ADD CARD IN CASE MEMBER IS NOT FOUND.                            
*                                                                               
         CLC   =C'++ADD ',CARD     DO WE ALREADY HAVE A ++ADD ?                 
         BE    PANV40                                                           
*                                                                               
         MVC   ADDDATA,SPACES      CLEAR EXTRANEOUS DATA                        
         MVC   ADDNAME,RELOMEM     INSERT THE NAME                              
         LA    R5,ADDNAME+1(R4)    FIRST TRAILING BLANK                         
         MVC   0(7,R5),=CL7',OBJECT'                                            
*                                                                               
PANV40   DS    0H                                                               
*                                                                               
*  CONSTRUCT ++UPDATE CARD IN CASE MEMBER IS FOUND.                             
*                                                                               
         CLC   =C'++UPDATE ',CARD  IS IT ALREADY ++UPDATE?                      
         BE    PANV50                                                           
*                                                                               
         MVC   UPDDATA,SPACES      CLEAR EXTRANEOUS DATA                        
         MVC   UPDNAME,RELOMEM     INSERT THE NAME                              
         LA    R5,UPDNAME+1(R4)    FIRST TRAILING BLANK                         
         MVC   0(6,R5),=CL6',0,ALL'                                             
*                                                                               
PANV50   DS    0H                                                               
         MVC   MSG1NM,RELOMEM                                                   
         MVC   P(MSG1LQ),MSG1                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  (SYSPUNCH,OUTPUT)                                                
*                                                                               
         MVC   OUTREC,UPDREC       POSIT MEMBER FOUND                           
         MVC   MSG2FND,=C' WAS FOUND.    '                                      
         GOTO1 =V(PANIC),DMCB,(X'20',=C'READ'),=C'DIR',RELOMEM,CARD             
         TM    DMCB+8,X'10'                                                     
         BZ    WRTPUNCH            YES: RETURN                                  
*                                                                               
         MVC   OUTREC,ADDREC       REPLACE OUTREC WITH ++ADD                    
         MVC   RETCODE,=F'1'       SET "NOT FOUND" RETURN CODE                  
         MVC   MSG2FND,=C' WAS NOT FOUND.'                                      
*                                                                               
WRTPUNCH DS    0H                                                               
         MVC   OUTREC+72(8),SPACES (CLEAR PANACEA'S GARBAGE)                    
         PUT   SYSPUNCH,OUTREC     WRITE OUT CONTROL CARD                       
         CLOSE SYSPUNCH                                                         
*                                                                               
         MVC   MSG2NM,RELOMEM                                                   
         MVC   P(MSG2LQ),MSG2                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   DELMEM,RELOMEM                                                   
         OPEN  (DELETE,OUTPUT)                                                  
         PUT   DELETE,DELREC       WRITE OUT CONTROL CARD                       
         CLOSE DELETE                                                           
*                                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
DMCB     DS    6F                                                               
*                                                                               
RETCODE  DC    F'0'                RETURN CODE                                  
*                                                                               
CARD     DC    CL80' '             INPUT CARD FROM PANACEA                      
*                                                                               
ADDREC   DC    C'++ADD '           GENERATED ++ADD CARD                         
ADDDATA  DS    CL74                                                             
         ORG   ADDREC+6                                                         
ADDNAME  DS    CL10                                                             
         ORG                                                                    
*                                                                               
UPDREC   DC    C'++UPDATE '        GENERATED ++UPDATE CARD                      
UPDDATA  DS    CL71                                                             
         ORG   UPDREC+9                                                         
UPDNAME  DS    CL10                                                             
         ORG                                                                    
*                                                                               
DELREC   DC    C'++DELETE NAME='   IN CASE THERE'S A "PARTIAL ADD"              
DELMEM   DS    CL10                                                             
DELDATA  DC    CL56' '                                                          
*                                                                               
OUTREC   DC    CL80' '                                                          
*                                                                               
MSG1     DC    C'PANRMCT-01: SEARCHING FOR '                                    
MSG1NM   DS    CL10                                                             
MSG1LQ   EQU   *-MSG1                                                           
*                                                                               
MSG2     DC    C'PANRMCT-02: '                                                  
MSG2NM   DS    CL10                                                             
MSG2FND  DS    C' WAS NOT FOUND.'                                               
MSG2LQ   EQU   *-MSG2                                                           
*                                                                               
RELOMEM  DS    CL10                "RM" MEMBER NAME                             
*                                                                               
SYSPUNCH DCB   DDNAME=SYSPUNCH,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
DELETE   DCB   DDNAME=DELETE,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM                
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DDPAN0UPD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDPANRMCTL04/28/09'                                      
         END                                                                    
