*          DATA SET DDPANRSTR  AT LEVEL 081 AS OF 08/15/00                      
*PHASE PANRSTRA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'CREATE PARAMETER CARDS FOR PAN REPLACE COMMAND'                 
PANRSTR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PANRSTR,=A(R13CHAIN)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         OPEN  (TEMPDS,OUTPUT)                                                  
*                                                                               
NEXT     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'       READ MEMBER NAME              
         CLC   CARD(2),=C'/*'                                                   
         BE    OKAY                                                             
*                                                                               
         LA    R2,PCARD+10         POINT TO MEMBER NAME FIELD IN PCARD          
*                                                                               
         CLC   =C'MEMBER=',CARD                                                 
         BNE   ERRNAME                                                          
*                                                                               
         SR    RE,RE               COUNT CHARACTERS                             
         LA    RF,CARD+7                                                        
         CLI   0(RF),C' '          SCAN FORWARD TO FIRST BLANK                  
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     *-16                                                             
*                                                                               
         LTR   RE,RE               ZERO CHAR?                                   
         BZ    ERRNAME                                                          
*                                                                               
         CHI   RE,10               MORE THAN 10 CHAR?                           
         BH    ERRNAME                                                          
*                                                                               
         CLC   LASTMEM,CARD+7                                                   
         BNL   ERRNAME2            NOT IN ALPHABETICAL ORDER                    
*                                                                               
         MVC   LASTMEM,CARD+7                                                   
         MVC   0(10,R2),CARD+7     MOVE MEMBER NAME TO PCARD                    
         AR    R2,RE                                                            
         MVC   0(2,R2),=C',,'                                                   
         AHI   R2,2                POINT TO VERSION FIELD IN PCARD              
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'         READ VERSION #              
         CLC   =C'VERSION=',CARD                                                
         BNE   ERRVERS                                                          
*                                                                               
         SR    RE,RE               COUNT CHARACTERS                             
         LA    RF,CARD+8                                                        
         CLI   0(RF),C' '          SCAN FORWARD TO FIRST BLANK                  
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     *-16                                                             
*                                                                               
         LTR   RE,RE               ZERO CHAR?                                   
         BZ    ERRVERS                                                          
*                                                                               
         CHI   RE,2                MORE THAN 2 CHAR?                            
         BH    ERRVERS                                                          
*                                                                               
         LA    R1,CARD+8                                                        
         CLI   0(R1),C'0'                                                       
         BE    ERRVERS                                                          
*                                                                               
CHKNUM   CLI   0(R1),C'0'                                                       
         BL    ERRVERS                                                          
         CLI   0(R1),C'9'                                                       
         BH    ERRVERS                                                          
         AHI   R1,1                                                             
         BCT   RE,CHKNUM                                                        
*                                                                               
         MVC   0(2,R2),CARD+8                                                   
*                                                                               
         PUT   TEMPDS,PCARD                                                     
         MVC   P(L'PCARD),PCARD                                                 
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
OKAY     MVC   P(L'MSG3),MSG3                                                   
         GOTO1 =V(PRINTER)                                                      
         SR    R5,R5               EXIT WITH ZERO COMPLETIONCODE                
*                                                                               
EXIT     CLOSE TEMPDS                                                           
         XBASE RC=(R5)                                                          
*                                                                               
ERRNAME  MVC   P(L'MSG1),MSG1                                                   
         GOTO1 =V(PRINTER)                                                      
         LHI   R5,8                                                             
         B     EXIT                                                             
ERRNAME2 MVC   P(L'MSG4),MSG4                                                   
         GOTO1 =V(PRINTER)                                                      
         LHI   R5,8                                                             
         B     EXIT                                                             
ERRVERS  MVC   P(L'MSG2),MSG2                                                   
         GOTO1 =V(PRINTER)                                                      
         LHI   R5,8                                                             
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
TEMPDS   DCB   DDNAME=SYSPANIN,MACRF=PM,DSORG=PS,LRECL=80,RECFM=FB              
PCARD    DC    CL80'++REPLACE '                                                 
CARD     DS    CL80                                                             
DMCB     DS    8F                                                               
DUB      DS    D                                                                
LASTMEM  DC    CL10' '             MEMBER NAME LAST READ                        
MSG1     DC    C'INVALID/MISSING MEMBER NAME - JOB FAILED'                      
MSG2     DC    C'INVALID/MISSING VERSION NUMBER - JOB FAILED'                   
MSG3     DC    C'JOB COMPLETED SUCCESSFULLY'                                    
MSG4     DC    C'MEMBER NAMES MUST BE IN ALPHABETICAL ORDER - JOB FAILE+        
               D'                                                               
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081DDPANRSTR 08/15/00'                                      
         END                                                                    
