*          DATA SET SPEZPARSE  AT LEVEL 132 AS OF 10/10/11                      
*PHASE T00A2EA                                                                  
*                                                                               
         TITLE 'T00A2E - EASI RECORD PARSER'                                    
*                                                                               
***********************************************************************         
*                                                                               
* ON ENTRY: P1 - A(RECORD)                                                      
*           P2 - A(FLDTAB) SEE EZFLDTAB FOR DEFINITIONS                         
*           P2 - HOB = NUMBER OF 5-BYTE FIELDS (SEE EZFLDTAB)                   
*                                                                               
*                                                                               
***********************************************************************         
*    EZPARSE - SEPARATES SEMICOLON-DELIMITED RECORD INTO FIELDS                 
***********************************************************************         
*                                                                     *         
*   ON ENTRY                                                          *         
*   -------------                                                     *         
*   PARAMETER 1            A(RECORD)                                  *         
*                                                                     *         
*   PARAMETER 2  BYTE 0    MAX NUMBER OF ENTRIES IN FLDTAB            *         
*                BYTE 1-3  A(FLDTAB), SEE EZFLDTAB FOR DEFINITION     *         
*                                                                     *         
*   ON EXIT                                                           *         
*   -------------                                                     *         
*   FLDTAB FILLED IN, EQUAL CONDITION CODE                            *         
*   OR                                                                *         
*   UNEQUAL CONDITION CODE IN CASE OF ERRORS                          *         
*   PARAMETER 3  BYTE 0    ERROR CODE                                 *         
*                                                                     *         
*   X'80' - FLDTAB FULL                                               *         
*   X'40' - EMPTY RECORD                                              *         
*                                                                     *         
*   REGISTER USAGE                                                    *         
*   --------------                                                    *         
*   R3           FIELD START POINTER                                  *         
*   R4           FIELD END POINTER                                    *         
*   R5           FIELD COUNTER                                        *         
*   R6           FLDTAB POINTER                                       *         
*   R9           EOR POINTER                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
T00A2E   RSECT                                                                  
         NMOD1 EZ52WKLQ,**0A2E**                                                
         LR    R7,RC                                                            
         USING EZ52WKD,R7                                                       
*                                                                               
* SAVE PASSES PARAMETERS                                                        
         ST    R1,APARM                                                         
*                                                                               
* SAVE ADDRESS OF RECORD                                                        
         MVC   AREC,0(R1)                                                       
*                                                                               
* SET FIELD START POINTER                                                       
         L     R3,0(R1)            REC START                                    
         LA    R3,4(R3)            MOVE PAST RECORD LENGTH                      
*                                                                               
* SET FIELD END POINTER                                                         
         LR    R4,R3               INITIALIZE TO FIELD START                    
*                                                                               
* INITIALIZE FIELD COUNTER                                                      
         XR    R5,R5               FIELD COUNTER                                
*                                                                               
* SET FLDTAB POINTER                                                            
         L     R6,4(R1)            A(FLDTAB)                                    
         LA    R6,0(R6)            CLEAR HOB                                    
         USING FLDTABD,R6                                                       
*                                                                               
* SAVE MAX NUMBER OF FIELD TABLE ENTRIES                                        
         MVC   FTABLEN,4(R1)       NUMBER OF FIELDS IN FLDTAB                   
*                                                                               
* CALCULATE AND SAVE END OF RECORD                                              
         L     RE,0(R1)            A(RECORD)                                    
         XR    RF,RF                                                            
*                                                                               
         MVI   ERRCODE,ERREMPTQ    PRE-SET EMPTY RECORD ERROR                   
         ICM   RF,3,0(RE)          RECORD LENGTH                                
         BZ    PARSEERR                                                         
*                                                                               
         AR    RE,RF               A(EOR)                                       
         ST    RE,AEOR                                                          
*                                                                               
         SHI   R6,FTLENQ                                                        
         BRAS  RE,CKFLDCTR         SET TABLE POINTER+FLD COUNTER                
*                                                                               
* CLEAR THE FIELD TABLE                                                         
         L     RE,4(R1)            A(FLDTAB)                                    
         LLC   RF,4(R1)            NUM OF FIELDS IN TABLE                       
         MHI   RF,FTLENQ                                                        
         XCEFL                                                                  
*                                                                               
* INITIALIZE FLAGS BYTE                                                         
         MVI   FLAGS,X'00'                                                      
*                                                                               
* CHECK IF INV HEADER (A.K.A. '31') RECORD, AND SET APPROPRIATE FLAG            
         CLC   =C'31',0(R4)                                                     
         BNE   *+8                                                              
         OI    FLAGS,FLG31Q        INDICATE WE HAVE INVOICE HDR REC             
*                                                                               
PARSE10  DS    0H                                                               
         BRAS  RE,ISEOR            SEE IF REACHED EOR                           
         BE    PARSEX                                                           
*                                                                               
         CLI   0(R4),FDELIMQ       REACHED A FIELD DELIMITER?                   
         BE    *+12                YES - GO THROUGH EOF LOGIC                   
*                                                                               
         LA    R4,1(R4)            OTHERWISE ADVANCE TO NEXT CHARACTER          
         B     PARSE10                                                          
*                                                                               
* REACHED EOF HERE                                                              
*                                                                               
* CALCULATE AND SAVE FIELD DISPLACEMENT                                         
         LR    RF,R3                                                            
         S     RF,AREC                                                          
         STCM  RF,3,FTFLDDSP       SAVE DISP TO FIELD IN FLDTAB                 
*                                                                               
* CALCULATE AND SAVE FIELD LENGTH                                               
         LR    RF,R4               A(FIELD END POINTER)                         
         SR    RF,R3               MINUS A(FIELD START)                         
         STC   RF,FTFLDLEN         SAVE FIELD LENGTH IN FLDTAB                  
*                                                                               
PARSE30  DS    0H                                                               
         LA    R4,1(R4)            ADVANCE EOF POINTER                          
         LR    R3,R4               SET FIELD START POINTER                      
         BRAS  RE,ISEOR            SEE IF REACHED EOR                           
         BE    PARSEX                                                           
*                                                                               
         BRAS  RE,CKFLDCTR         SET TABLE POINTER+FLD COUNTER                
*                                                                               
* ON INV HEADER RECORDS FIELDS 2 AND 37 ARE 12-BYTE BINARY                      
* CHECK IF WE HAVE TO PROCESS THOSE FIELDS NEXT                                 
*                                                                               
         TM    FLAGS,FLG31Q        INVOICE HEADER RECORD?                       
         BZ    PARSE10             NO - PROCEED AS USUAL                        
         CHI   R5,2                PROCESSED 1ST FIELD JUST NOW?                
         BE    *+12                YES, SPECIAL CODE FOR NEXT FIELD             
         CHI   R5,37               PROCESSED 36TH FIELD JUST NOW?               
         BNE   PARSE10             NO - PROCEED AS USUAL                        
*                                                                               
* SPECIAL CODE FOR FIELDS 2, 37                                                 
         LA    R4,12(R4)           MOVE PAST 12-BYTE SV FIELD                   
         B     PARSE10             EVALUATE NEXT CHARACTER                      
*                                                                               
* RETURN ERROR                                                                  
PARSEERR DS    0H                                                               
         L     R1,APARM                                                         
         MVC   8(1,R1),ERRCODE                                                  
         B     NEQXIT                                                           
*                                                                               
PARSEX   DS    0H                                                               
         MVI   ERRCODE,ERREMPTQ    PRE-SET EMPTY RECORD ERROR                   
         LTR   R5,R5               FIELD COUNTER - ANY FIELDS FOUND?            
         BZ    PARSEERR            NO - WE HAVE EMPTY RECORD                    
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
CKFLDCTR AHI   R5,1                INCREMENT FIELD COUNTER                      
         MVI   ERRCODE,ERRTABQ     PRE-SET TABLE FULL ERROR                     
         CLM   R5,1,FTABLEN        FIELD TABLE FULL?                            
         BH    PARSEERR            YES - ERROR                                  
         LA    R6,FTLENQ(R6)       NO - ADVANCE TABLE POINTER                   
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
ISEOR    NTR1  BASE=*,LABEL=*                                                   
         C     R4,AEOR                                                          
         JNL   EQXIT                                                            
         CLI   0(R4),RDELIMQ                                                    
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
EZ52WKD  DSECT                                                                  
APARM    DS    A                                                                
AREC     DS    A                                                                
AEOR     DS    A                                                                
FTABLEN  DS    X                                                                
*                                                                               
ERRCODE  DS    X                                                                
ERRTABQ  EQU   X'80'               FLDTAB FULL                                  
ERREMPTQ EQU   X'40'               RECORD EMPTY                                 
*                                                                               
FLAGS    DS    X                                                                
FLG31Q   EQU   X'80'               31 RECORD                                    
*                                                                               
* RECORD AND FIELD DELIMITERS                                                   
FDELIMQ  EQU   X'5E'               SEMICOLON, FIELD DELIMITER                   
RDELIMQ  EQU   X'15'                                                            
*                                                                               
EZ52WKLQ EQU   *-EZ52WKD                                                        
*                                                                               
       ++INCLUDE EZFLDTAB                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132SPEZPARSE 10/10/11'                                      
         END                                                                    
