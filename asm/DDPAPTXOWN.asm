*          DATA SET DDPAPTXOWN AT LEVEL 003 AS OF 07/01/09                      
*PHASE PAPTXOWA                                                                 
***********************************************************************         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                *** THIS MODULE IS DEAD ***                          *         
*                                                                     *         
* IT WAS ONLY USED BY THE PAPTBKIN (PANAPT "BACKIN" PROCEDURE), WHICH *         
* IS NO LONGER SUPPORTED.                                             *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'PANAPT: EXTRACT OWNER ID FROM MOVE REQUEST'                     
PAPTXOWN CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTXOWN,=V(REGSAVE)                                           
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         XC    RTRNCD,RTRNCD            POSIT PROD LEVEL                        
*                                                                               
         OPEN  MOVEREQ                                                          
         OPEN  (USERLIST,OUTPUT)                                                
*                                                                               
         LA    R3,IO+4                                                          
         USING APAMMDES,R3                                                      
*                                                                               
MAIN     DS    0H                                                               
         GET   MOVEREQ,IO                                                       
*                                                                               
         CLC   =C'01',DESTYPE           TYPE 1?                                 
         BNE   MAIN                     NO: FLUSH IT                            
*                                                                               
         MVC   USERMR,DESNUMB                                                   
         MVC   USERID,DESADDID          EXTRACT USERID                          
         PUT   USERLIST,USERREC         WRITE TO LIST                           
*                                                                               
         MVC   P(L'USERREC),USERREC     PRINT OUTPUT RECORD                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   DESCLSN,=C'STGE'         IS THE CURRENT LEVEL STGE?              
         BNE   *+10                     NO                                      
         MVC   RTRNCD,=F'1'             RC = 1: INDICATES STGE LEVEL            
*                                                                               
         MVC   P(15),=C'CURRENT LEVEL: '                                        
         MVC   P+15(L'DESCLSN),DESCLSN  PRINT CURRENT LEVEL                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE RC=RTRNCD                                                        
         DROP  R3,RA                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
RTRNCD   DC    F'0'          RETURN CODE                                        
*                                                                               
USERREC  DC    CL80' '                                                          
         ORG   USERREC                                                          
USERMR   DC    CL6' '                                                           
         DC    CL1' '                                                           
USERID   DC    CL8' '                                                           
         ORG                                                                    
         SPACE 3                                                                
* NOTE: NO EODAD IS PROVIDED. WE SHOULD NEVER HIT THE EOF: THERE MUST           
*       ALWAYS BE AN "01" RECORD!                                               
MOVEREQ  DCB   DDNAME=MOVEREQ,DSORG=PS,LRECL=1152,RECFM=VB,MACRF=GM             
*                                                                               
USERLIST DCB   DDNAME=USERLIST,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
         SPACE 3                                                                
IO       DS    CL2000                                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         APAMMDES                                                               
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDPAPTXOWN07/01/09'                                      
         END                                                                    
