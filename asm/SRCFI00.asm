*          DATA SET SRCFI00    AT LEVEL 045 AS OF 05/01/02                      
*PHASE T16000A                                                                  
         PRINT NOGEN                                                            
         TITLE '$CFI (T16000) -- EC DATE/TIME STAMP'                            
*                                                                               
* THIS IS AN ONLINE PROGRAM TO DATE/TIME STAMP REP CONTRACTS FOR EC             
*                                                                               
SRCFI00  CSECT                                                                  
         NMOD1 WORKX-WORKD,*$CF*,R8,RR=R3                                       
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
*                                                                               
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          A(SYSFACS)                                   
*                                                                               
         MVC   ATIA,SRPAR6         A(TIA)                                       
         MVC   AUTL,SRPAR3         A(UTL)                                       
         L     RA,ATIA                                                          
         USING T160FFD,RA          A(TWA)                                       
         L     R6,SRPAR4                                                        
         USING COMFACSD,R6         A(COMFACS)                                   
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         DROP  R6                                                               
         BAS   RE,CTRLSET                                                       
         BNZ   SCFI0800            EXIT:  CC NOT ZERO                           
         BAS   RE,RETRVCON         GET THE CONTRACT                             
         BZ    SCFI0040            RECORD FOUND                                 
****     MVC   CFICF2+40(16),=C'RECORD NOT FOUND'                               
****     LA    R2,CFICF2H          SET TRANSMIT, CURSOR BITS                    
****     OI    6(R2),X'C0'                                                      
         B     SCFI0900                                                         
SCFI0040 EQU   *                                                                
****     MVC   CFICF2+40(12),=C'RECORD FOUND'                                   
****     LA    R2,CFICF2H          SET TRANSMIT, CURSOR BITS                    
****     OI    6(R2),X'C0'                                                      
         BAS   RE,UPDATEEC         UPDATE THE EC DATE ELEMENT                   
         B     SCFI0900                                                         
SCFI0800 EQU   *                                                                
****     MVC   CFICF2+40(12),=C'BAD SWITCH  '                                   
****     LA    R2,CFICF2H          SET TRANSMIT, CURSOR BITS                    
****     OI    6(R2),X'C0'                                                      
SCFI0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.                          
*                                                                               
CTRLSET  NTR1                                                                   
         L     R3,AUTL                                                          
         MVI   4(R3),X'0A'         SET UTL SE TO CTFILE                         
*****>> GOTO1 VDATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',               +         
*****>>        =C'NCTFILE X',IOAREA,0                                           
         XC    WORK,WORK                                                        
         MVI   WORK,C'5'           FIND CONTROL FILE ACCESS RECORD              
*                                                                               
*  DON'T HAVE 'AGENCY' AT THIS TIME.  EXAMINE THE INPUT TO SEE IF               
*     #CFI DATA IS FOLLOWED BY PROPER MESSAGE.  TAKE CODE FROM THERE.           
*                                                                               
         LA    R2,CFICF1H                                                       
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    CTRL0050            NO  - DON'T DO ANYTHING                      
         LA    R2,8(R2)            YES - BUMP TO DATA IN FIELD                  
         USING SLINE#1,R2                                                       
         CLC   SLINMSG,=C'MSG='    EC TIME-STAMP MESSAGE?                       
         BNE   CTRL0050            NO  - DON'T DO ANYTHING                      
         MVC   WORK+23,SLINNREP    YES - INSERT POWER CODE FROM MSG             
         PRINT GEN                                                              
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,IOAREA                  
         PRINT NOGEN                                                            
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         LA    R1,IOAREA                                                        
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
         L     R4,FULL             RESET A(X'21' ELEMENT)                       
*                                     WITH REP UTL CODE                         
         PRINT GEN                                                              
         GOTO1 VSWITCH,DMCB,(3(R4),X'FFFFFFFF'),0                               
         PRINT NOGEN                                                            
         MVC   4(1,R3),3(R4)       OVERRIDE CONTROL FILE UTL                    
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    CTRL0060            YES - EXIT CC = ZERO                         
         CLI   4(R1),2             TEST FOR SYSTEM NOP                          
         BE    CTRL0040            KILL FOR NOW                                 
CTRL0040 EQU   *                   NOP/NOT SWITCHED OKAY                        
         DC    H'0'                                                             
CTRL0050 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
CTRL0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  RETRVCON:  RETRIEVE THE CONTRACT RECORD, AND UPDATE THE                      
*    APPROPRIATE FIELDS                                                         
*                                                                               
RETRVCON NTR1                                                                   
         LA    R2,CFICF1           A(FIRST DATA FIELD)                          
         USING SLINE#1,R2                                                       
         PACK  DUB(8),SLINCON#(8)  PACK THE CONTRACT NUMBER                     
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         XC    KEY,KEY             CLEAR KEY AREA                               
         MVI   KEY,X'8C'           SET KEY TYPE                                 
         MVC   KEY+21(2),SLINNREP  SET REP IN KEY                               
         MVC   KEY+23(4),WORK      INSERT CONTRACT NUM (9'S COMP)               
         PRINT GEN                                                              
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
         CLC   KEY(27),KEYSAVE     SET RETURN CONDITION CODE                    
         BNE   RETR0900            KEY NOT FOUND:  EXIT                         
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,IOAREA, X        
               IOAREA2                                                          
         PRINT NOGEN                                                            
RETR0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  UPDATEEC:  RETRIEVE AND UPDATE THE EC ELEMENT, WHICH WAS ADDED               
*    WHEN THE ORDER WAS ORIGINALLY EC'D.                                        
*                                                                               
UPDATEEC NTR1                                                                   
         MVI   UPDATSW,C'N'        SET REWRITE NOT NEEDED                       
         LA    R2,IOAREA                                                        
         USING RCONRECD,R2                                                      
         LA    R6,RCONELEM         FIND X'15' ELEMENT                           
         DROP  R2                                                               
UPDA0020 EQU   *                                                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BE    UPDA0100            YES - EXIT                                   
         CLI   0(R6),X'15'         EC DATE ELEMENT?                             
         BNE   UPDA0020            NO  - GO BACK FOR NEXT                       
         USING RCONECEL,R6                                                      
         LA    R2,CFICF1           A(1ST LINE OF INPUT)                         
         USING SLINE#1,R2                                                       
         XC    WORK,WORK           CLEAR DATE AREA                              
         MVC   WORK(2),SLINSDAT    SET UP MMDDYY                                
         MVC   WORK+2(2),SLINSDAT+3                                             
         MVC   WORK+4(2),SLINSDAT+6                                             
         GOTO1 VDATVAL,DMCB,WORK,WORK+10                                        
         OC    DMCB(4),DMCB        ERROR/NOT PRESENT?                           
         BZ    UPDA0040            YES - DON'T DO ANYTHING                      
         GOTO1 VDATCON,DMCB,(0,WORK+10),(2,RCONECDS)                            
         MVC   RCONECTS(2),SLINSTIM                                             
*                                  INSERT HOURS OF TIME STORED                  
         MVC   RCONECTS+2(2),SLINSTIM+3                                         
*                                  INSERT MINUTES OF TIME STORED                
         MVI   UPDATSW,C'Y'        SET REWRITE NEEDED                           
         DROP  R2                                                               
UPDA0040 EQU   *                                                                
         LA    R2,CFICF2           A(2ND LINE OF INPUT)                         
         USING SLINE#2,R2                                                       
         XC    WORK,WORK           CLEAR DATE AREA                              
         MVC   WORK(2),SLINTDAT    SET UP MMDDYY                                
         MVC   WORK+2(2),SLINTDAT+3                                             
         MVC   WORK+4(2),SLINTDAT+6                                             
         GOTO1 VDATVAL,DMCB,WORK,WORK+10                                        
         OC    DMCB(4),DMCB        ERROR/NOT PRESENT?                           
         BZ    UPDA0060            YES - DON'T DO ANYTHING                      
         GOTO1 VDATCON,DMCB,(0,WORK+10),(2,RCONECDX)                            
         MVC   RCONECTX(2),SLINTTIM                                             
*                                  INSERT HOURS OF TIME XFERRED                 
         MVC   RCONECTX+2(2),SLINTTIM+3                                         
*                                  INSERT MINUTES OF TIME XFERRED               
         MVI   UPDATSW,C'Y'        SET REWRITE NEEDED                           
         DROP  R2                                                               
UPDA0060 EQU   *                                                                
         CLI   UPDATSW,C'Y'        REWRITE NEEDED?                              
         BNE   UPDA0100            NO                                           
         GOTO1 VDATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,IOAREA, X        
               IOAREA2                                                          
UPDA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   LOCAL VALUES                                                                
*                                                                               
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
RELO     DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
FULL     DS    F                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
HALF     DS    H                                                                
BYTE     DS    C                                                                
UPDATSW  DS    C                   REWRITE NEEDED FLAG                          
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
VSWITCH  DS    A                                                                
VHELLO   DS    A                                                                
VDATCON  DS    A                                                                
VDATVAL  DS    A                                                                
VCALLOVL DS    A                                                                
VLOCKET  DS    A                                                                
WORK     DS    CL64                                                             
IOAREA   DS    1024C                                                            
IOAREA2  DS    1024C                                                            
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   DSECT COVERING DATA IN SCREEN LINES 1 AND 2                                 
*                                                                               
SLINE1   DSECT                                                                  
SLINE#1  DS    0CL60                                                            
SLINID   DS    CL4    +0           ALWAYS 0199                                  
         DS    CL2    +4           FILL                                         
SLINMSG  DS    CL4    +6           ALWAYS MSG=                                  
         DS    CL1    +10          FILL                                         
SLINEC   DS    CL3    +11          ALWAYS EC:                                   
SLINNREP DS    CL2    +14          NATIONAL REP ID                              
         DS    CL1    +16          FILL                                         
SLINSTAT DS    CL4    +17          STATION: 1ST 4 CHARS                         
         DS    CL1    +21          FILL                                         
SLINLREP DS    CL2    +22          LOCAL REP ID                                 
         DS    CL1    +24          FILL                                         
SLINCON# DS    CL8    +25          CONTRACT NUMBER                              
         DS    CL2    +33          FILL                                         
SLINSTOR DS    CL7    +35          ALWAYS STORED=                               
         DS    CL1    +42          FILL                                         
SLINSDAT DS    CL8    +43          STORED DATE: MM/DD/YY                        
         DS    CL1    +51          FILL                                         
SLINSTIM DS    CL8    +52          STORED TIME: HH:MM:SS                        
*                                                                               
SLINE2   DSECT                                                                  
SLINE#2  DS    0CL36                                                            
         DS    CL3                 FILL                                         
SLINTRAN DS    CL5                 ALWAYS SENT=                                 
         DS    CL1                 FILL                                         
SLINTDAT DS    CL8                 SENT DATE: MM/DD/YY                          
         DS    CL1                 FILL                                         
SLINTTIM DS    CL8                 SENT TIME: HH:MM:SS                          
SLINJDS# DS    CL8                 JDS CONTRACT NUMBER                          
         DS    CL2                 ALWAYS $!                                    
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
* FAFACTS                                                                       
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
       ++INCLUDE SRCFIFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SRCFI00   05/01/02'                                      
         END                                                                    
