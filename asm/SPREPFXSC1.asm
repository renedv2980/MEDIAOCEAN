*          DATA SET SPREPFXSC1 AT LEVEL 033 AS OF 10/26/99                      
*PHASE SPFX02S                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*   TITLE     : DO A PRINT ON SOME WILA ESTIMATE RECORDS                        
*                                                                               
*   REG USAGE : R0 - WORK.                                                      
*               R1 - WORK.                                                      
*               R2 -                                                            
*               R3 -                                                            
*               R4 -                                                            
*               R5 -                                                            
*               R6 -                                                            
*               R7 -                                                            
*               R8 - 2ND BASE REGISTER.                                         
*               R9 - 2ND REG USED BY SPWORKD.                                   
*               RA - 1ST REG USED BY SPWORKD.                                   
*               RB - BASE REGISTER.                                             
*               RC -                                                            
*               RD - REGISTER D CHAIN.                                          
*               RE -                                                            
*               RF -                                                            
***********************************************************************         
         TITLE 'SPREPFXSC1 - DISPLAY WILA CLT/PRD/EST AND DATES'                
***********************************************************************         
*=========================== MAIN PROGRAM ============================*         
*                                                                               
SPFX02L  CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02L                                                          
         NMOD1 0,SPFX02L,R8                                                     
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
REQF     DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R1,TEMPIO                                                        
         ST    R1,AREC                                                          
*                                                                               
REQF10   MVC   KEY(2),=X'0012'     THAT'S WHERE WE START                        
         GOTO1 HIGH                                                             
         B     REQF30                                                           
*                                                                               
REQF20   GOTO1 SEQ                                                              
*                                                                               
REQF30   CLC   KEYSAVE(2),KEY      WE STILL ON X'0012' RIGHT?                   
         BNE   REQFX                                                            
*                                                                               
         CLI   KEY+4,0                                                          
         BE    REQF20              IT'S A CLIENT RECORD                         
*                                                                               
         CLI   KEY+7,0                                                          
         BE    REQF20              IT'S A PRODUCT RECORD                        
*                                                                               
         CLC   KEY+8(5),=5X'00'    DO WE HAVE AN ESTIMATE REC?                  
         BNE   REQF20                                                           
*                                                                               
         L     R2,AREC                                                          
         USING ESTD,R2                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
*                                                                               
         CLI   EDAYMENU,C'0'                                                    
         BNE   REQF50                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(4,=C'01/00/99'),(0,STRTDATE)                        
         GOTO1 DATCON,DMCB,(4,=C'12/31/00'),(0,ENDDATE)                         
*                                                                               
         CLC   ESTART,STRTDATE                                                  
         BL    REQF20                                                           
         CLC   ESTART,ENDDATE                                                   
         BH    REQF20                                                           
*                                                                               
         CLC   EKEYPRD,=C'POL'                                                  
         BNE   REQF20                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         GOTO1 CLUNPK,DMCB,EKEYCLT,P+30                                         
         MVC   P+35(3),EKEYPRD                                                  
         EDIT  EKEYEST,(2,P+40)                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(11,P+45)                                 
         GOTO1 DATCON,DMCB,(0,EEND),(11,P+55)                                   
         GOTO1 REPORT                                                           
*                                                                               
REQF50   MVI   KEY+8,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     REQF30                                                           
*                                                                               
REQFX    GOTO1 AENDREQ                                                          
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MISC                                                                   
SVESTNUM DS    CL1                                                              
STRTDATE DS    CL6                                                              
ENDDATE  DS    CL6                                                              
TEMPIO   DS    XL2000                                                           
***********************************************************************         
ESTD     DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPMODES                                                     
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPREPWORKD                                                     
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPREPFXSC110/26/99'                                      
         END                                                                    
