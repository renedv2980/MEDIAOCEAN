*          DATA SET OLDDSGEN   AT LEVEL 022 AS OF 02/27/97                      
*          DATA SET DSGEN      AT LEVEL 045 AS OF 09/25/90                      
**********************************************************************          
*                         DSECT FILE PARSE                           *          
*                           VERSION 1.0                              *          
**********************************************************************          
*                                                                               
*PHASE DSGEN                                                                    
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'DSGEN - DSECT FILE PARSER'                                      
DSGEN    CSECT                                                                  
*                                                                               
**************************** INITS **********************************           
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,DSGEN,=V(REGSAVE),RA                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
**************************** MAIN **********************************            
*                                                                               
* USES REGISTERS:                                                               
* R2:                                                                           
* R3:                                                                           
* R4:                                                                           
* R5:                                                                           
* R6:                                                                           
* R7:                                                                           
* R8:                                                                           
* R9:                                                                           
*                                                                               
         OPEN  (DSECTIN,INPUT,OBJFIL,OUTPUT)                                    
         LA    R2,DLINE                                                         
         USING SRFADSEC,R2                                                      
*                                                                               
         MVC   OBLINE,SPACES                                                    
         MVI   OBLINE,X'02'        ESD CARD                                     
         MVC   OBLINE+1(3),=C'ESD'                                              
         MVC   OBLINE+10(2),=H'16'                                              
         MVC   OBLINE+14(2),=H'01'                                              
         MVC   OBLINE+16(8),PHASENM                                             
         MVC   OBLINE+24(4),=F'0'                                               
         MVC   OBLINE+29,0                                                      
* ?? WHAT'S SUPPOSE TO BE IN R3??                                               
*        STCM  R3,3,OBLINE+30                                                   
         MVC   OBLINE+30,0                                                      
         MVC   OBLINE+72(8),=C'00000001'                                        
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
*                                                                               
         SR    R4,R4                                                            
         LA    R5,2                                                             
*                                                                               
RDLOOP   DS    0H                                                               
*                                                                               
         GET   DSECTIN,DLINE       STRIP OUT JUNK                               
         MVC   SRFADDSP,DLINE+3    GET DISPLACEMENT                             
         MVC   DSEQU,DLINE+29      GET EQU'S                                    
         MVC   SRFADLBL,DLINE+41   GET LABEL                                    
*                                  GET DEFINITION                               
         CLC   DLINE+50(2),=CL2'DS'                                             
         BNE   *+8                 LOOKS FOR `DS`                               
         MVI   SRFADDEF,C'S'                                                    
         CLC   DLINE+52(3),=CL3'ECT'                                            
         BNE   *+8                 LOOKS FOR `ECT` IN `DSECT`                   
         MVI   SRFADDEF,C'D'                                                    
         CLC   DLINE+50(2),=CL2'DC'                                             
         BNE   *+8                                                              
         MVI   SRFADDEF,C'C'                                                    
         CLC   DLINE+50(3),=CL3'EQU'                                            
         BNE   *+8                                                              
         MVI   SRFADDEF,C'E'                                                    
         CLC   DLINE+50(3),=CL3'ORG'                                            
         BNE   *+8                                                              
         MVI   SRFADDEF,C'O'                                                    
*                                                                               
         MVC   SRFADTYP,DLINE+56   GET TYPE                                     
         MVC   SRFADCMT,DLINE+76   GET COMMENTS                                 
*                                                                               
         GOTO1 =V(HEXIN),DMCB,SRFADDSP,DSPCHK,4                                 
         L     R6,DMCB+12          IS THIS A DS,DC, OR DSECT?                   
         C     R6,=F'0'                                                         
         BNE   DOIT                YES, PRINT THE CARD                          
         GOTO1 =V(HEXIN),DMCB,DSEQU,EQUCHK,5                                    
         L     R6,DMCB+12          IF NOT, IS THIS A EQU OR ORG?                
         C     R6,=F'0'                                                         
         BE    NXTPRT              YES=PRINT,NO=BUMP TO NEXT                    
         XC    SRFADTYP,SRFADTYP                                                
         MVC   SRFADTYP(5),DSEQU   REPLACE TYPE WITH VALUE                      
*                                                                               
DOIT     DS    0H                  PRINT THE CARD                               
*                                                                               
         MVI   OBLINE,X'02'                                                     
         MVC   OBLINE+1(3),=C'TXT'                                              
         ST    R4,DUB                                                           
         MVC   OBLINE+5(3),DUB+1                                                
         MVC   OBLINE+14(2),=H'1'                                               
         CVD   R5,DUB                                                           
         MVC   OBLINE+72(4),=C'0000'                                            
         UNPK  OBLINE+76(4),DUB                                                 
         OI    OBLINE+79,X'F0'                                                  
         MVC   OBLINE+10(2),=H'56'                                              
         MVC   OBLINE+16(SRFADDSL),SRFADDSP                                     
         PUT   OBJFIL,OBLINE                                                    
NXTPRT   MVC   OBLINE,SPACES                                                    
         LA    R4,56(R4)                                                        
         LA    R5,1(R5)                                                         
         B     RDLOOP                                                           
*                                                                               
DSEOF    DS    0H                  END CARD                                     
         MVI   OBLINE,X'02'                                                     
         MVC   OBLINE+1(3),=C'END'                                              
         CVD   R5,DUB                                                           
         MVC   OBLINE+72(4),=C'0000'                                            
         UNPK  OBLINE+76(4),DUB                                                 
         OI    OBLINE+79,X'F0'                                                  
         PUT   OBJFIL,OBLINE                                                    
         MVC   OBLINE,SPACES                                                    
*                                  NAME CARD                                    
         MVC   OBLINE(80),=CL80' NAME '                                         
         LA    R4,OBLINE+6                                                      
         MVC   0(8,R2),PHASENM                                                  
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *-8                                                              
         MVC   0(3,R4),=C'(R)'                                                  
         PUT   OBJFIL,OBLINE                                                    
*                                                                               
         CLOSE (DSECTIN)                                                        
         CLOSE (OBJFIL)                                                         
*                                                                               
EXBASE   XBASE                                                                  
*                                                                               
**************************** DEFS *********************************             
*                                                                               
DSECTIN  DCB   DDNAME=DSECTIN,DSORG=PS,RECFM=FBM,LRECL=121,            +        
               BLKSIZE=7986,MACRF=GM,EODAD=DSEOF                                
OBJFIL   DCB   DDNAME=OBJFIL,DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=80,    +        
               MACRF=PM                                                         
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL20                                                             
*                                                                               
EOFMARK  DC    CL2'/*'             EOF MARK OF JCL CARDS                        
DLINE    DC    CL121' '            HOLDS ONE INPUT LINE                         
OBLINE   DC    CL80' '             HOLDS ONE OUTPUT LINE                        
PHASENM  DC    CL8'T15E01'                                                      
DSEQU    DC    CL5' '              CHECKS FOR EQU`S                             
DSPCHK   DS    F                   CHECK IF DISPL. HEX                          
EQUCHK   DS    F                   CHECK IF EQU. HEX                            
FLAG1    DS    F                   SIGNALS IF DISPL. HEX                        
FLAG2    DS    F                   SIGNALS IF EQU. HEX                          
*                                                                               
         SPACE 3                                                                
DUMPLIST DS    0F                                                               
         DC    A(DSGEN,65000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
*                                                                               
       ++INCLUDE SRFADDSECT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022OLDDSGEN  02/27/97'                                      
         END                                                                    
