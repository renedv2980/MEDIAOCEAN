*          DATA SET DDPAPTOPS  AT LEVEL 008 AS OF 10/02/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTOPSA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
         TITLE 'TRIGGER OPS/MVS TO START PAPTSTC FOR MOVE REQUEST'              
*                                                                               
*======================================================================         
*                                                                               
* THIS MODULE IS EXECUTED DURING A PANAPT "SUB" ACTION VIA THE APIK200          
* SKELETON. APIK200 SUPPLIES A COMMA-DELIMITED "PARM=" STRING                   
* (AVAILABLE VIA R1). THESE PARAMETERS ARE PARSED AND REFORMATTED BY            
* THIS PROGRAM, AND THE REFORMATTED PARAMETERS ARE APPENDED TO A                
* CONSOLE MESSAGE WHICH TRIGGERS AN OPS/MVS RULE. THE PAPTSTC STARTED           
* TASK IS THEN LAUNCHED, AND MOVE REQUEST PROCESSING BEGINS.                    
*                                                                               
* IN ADDITION TO THE PARAMETERS THAT COME FROM APIK200, THIS PROGRAM            
* ALSO GENERATES ADDITIONAL PARAMETERS AND APPENDS THEM TO THE CONSOLE          
* MESSAGE.                                                                      
*                                                                               
*======================================================================         
*                                                                               
PAPTOPS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTOPS,=V(REGSAVE)                                            
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         LH    R2,0(R1)            PARM LENGTH                                  
*                                                                               
* FORMAT OF COMMA-DELIMITED PARAMETER STRING IS:                                
*  PANAPT_ENVIRONMENT                                                           
*  MR#                                                                          
*  SUBMITTER_USERID                                                             
*  ITMF TICKET SUBMITTER                                                        
*  ITMF JIRA ISSUE #                                                            
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),2(R1)       PARMS FROM APIK200                           
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),('MAX_PARMSIN',SCANBLK)             
         CLI   DMCB+4,MAX_PARMSIN  NO. OF PARMS EXPECTED FROM APIK200           
         JH    *+2                 TOO MANY PARAMETERS!                         
*                                                                               
         MVC   ENV,SCANENV         1ST PARAMETER                                
         MVC   MR,SCANMR           2ND PARAMETER                                
         MVC   USR,SCANUSR         3RD PARAMETER                                
         MVC   ITMFWHO,SCANITMF    4TH PARAMETER                                
         MVC   ITMFISS#,SCANISS#   5TH PARAMETER                                
*                                                                               
         GOTO1 =V(UNSCAN),DMCB,('NUMPARMS',UNSBLOCK),(C'C',OPERPARM)            
         CLI   DMCB,0              DID EVERYTHING FIT?                          
         BE    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         LA    R2,EOT              POINT BEYOND END OF MESSAGE                  
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                 BACK UP TO FIRST NON-BLANK                   
         LA    R2,1(R2)            POINT JUST PAST END OF MESSAGE               
*                                                                               
         L     RF,X'10'(,0)        COMMUNICATION VECTOR TABLE                   
         L     RF,CVTSMCA-CVT(,RF) SYSTEM MANAGEMENT CONTROL AREA               
         LA    RF,SMCASID-SMCABASE(,RF)    CPU ID (SMF)                         
         MVC   0(7,R2),=C',CPUID='                                              
         MVC   7(3,R2),0(RF)       ASSUME 3-CHARACTER CPU ID                    
         LA    R2,10(,R2)                                                       
*                                                                               
         LA    RF,OPERMSGL         START OF MESSAGE                             
         SR    R2,RF               MESSAGE LENGTH                               
         STH   R2,OPERMSGL                                                      
         CHI   R2,OPERMSGQ         MESSAGE IS TOO LONG?                         
         BNH   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         LA    R3,OPERMSGL         PUT MESSAGE TO CONSOLE                       
         WTO   TEXT=(R3)                                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
CARD     DC    CL80' '                                                          
*                                                                               
         DS    0D                                                               
         DC    C'*OPERMSG'         EYE-CATCHER                                  
OPERMSGL DS    H                   LENGTH FOR WTO                               
         DC    C'PAPTSTC2'         OPS/MVS MESSAGE ID                           
         DC    C' '                REQUIRED DELIMITER                           
OPERPARM DS    CL80                PARM FOR OPS/MVS                             
OPERMSGQ EQU   *-OPERMSGL                                                       
EOT      DC    X'FF'                                                            
         SPACE 3                                                                
* SCANNER BLOCK                                                                 
*                                                                               
SCANBLK  DS    0D                                                               
         DS    XL12                                                             
SCANENV  DS    CL20                ENVIRONMENT                                  
         DS    XL12                                                             
SCANMR   DS    CL20                MR #                                         
         DS    XL12                                                             
SCANUSR  DS    CL20                USERID                                       
         DS    XL12                                                             
SCANITMF DS    CL20                ITMF TICKET CREATOR                          
         DS    XL12                                                             
SCANISS# DC    CL20'0'             ITMF JIRA ISSUE # (DEFAULT = 0)              
MAX_PARMSIN EQU   (*-SCANBLK)/(12+20)                                           
         SPACE 3                                                                
* UNSCAN BLOCK                                                                  
*                                                                               
UNSBLOCK DS    0D                                                               
*                                                                               
* THESE PARAMETERS COME FROM APIK200                                            
*                                                                               
         DC    CL10'ENV'                                                        
ENV      DC    CL10' '                                                          
         DC    CL10'MR'                                                         
MR       DC    CL10' '                                                          
         DC    CL10'USR'                                                        
USR      DC    CL10' '                                                          
         DC    CL10'ITMFWHO'                                                    
ITMFWHO  DC    CL10'XXXX'          DEFAULT TO A FAKE TSO USERID                 
         DC    CL10'ITMFISS#'                                                   
ITMFISS# DC    CL10' '                                                          
NUMPARMS EQU   (*-UNSBLOCK)/(10+10)                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         CVT   DSECT=YES                                                        
         IEESMCA                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDPAPTOPS 10/02/17'                                      
         END                                                                    
