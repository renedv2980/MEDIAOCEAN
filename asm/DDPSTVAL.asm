*          DATA SET DDPSTVAL   AT LEVEL 032 AS OF 03/06/13                      
*PHASE T00A6BA                                                                  
         SPACE 2                                                                
* 05MAR13  BPLA  HST VALUES FOR P.E.ISLAND (PE)                                 
* 23APR10  BPLA  ENTRIES FOR ONTARIO AND BRITISH COLUMBIA HST                   
*                THEY WILL BECOME EFFECTIVE JUL/10                              
* 30OCT97  MHER  ADD A VALUE OF * FOR PQ, WHICH IS USED BY                      
*                QUEBEC AGENCIES TO PAY PST BUT NOT BILL IT                     
* 27FEB97  MHER  ALLOW PST CODE OF H FOR HARMONIZED SALES TAX                   
* 22MAR97  MHER  DEFINE VALID CODES BY PROVINCE                                 
         SPACE 1                                                                
*================================================================*              
* VALIDATE/FORMATE PROVINCIAL SERVICE TAX CODES                  *              
* INPUT IS OF THE FORM P1=C1,P2=C2,...                           *              
* WHERE P IS A PROVINCE CODE AND C IS A GST CODE                 *              
*                                                                *              
* PROVINCE CODES ARE  BC = BRITISH COLUMBIA                      *              
*                     AL = ALBERTA                               *              
*                     SA = SASKATCHEWAN                          *              
*                     MA = MANITOBA                              *              
*                     ON = ONTARIO                               *              
*                     PQ = QUEBEC                                *              
*                     NB = NEW BRUNSWICK                         *              
*                     NS = NOVA SCOTIA                           *              
*                     PE = PRINCE EDWARD ISLAND                  *              
*                     NF = NEWFOUNDLAND                          *              
* VALIDATE PRODUCES A 10 BYTE OUTPUT ELEMENT                     *              
* FORMAT PRODUCES AN OUTPUT STRING FROM A 10 BYTE STRING         *              
*================================================================*              
         TITLE 'PSTVAL - VALIDATE/FORMAT PROVINCIAL SERVICE TAX CODES'          
PSTVAL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**PSTVAL,CLEAR=YES                                   
         USING WORKD,RC                                                         
         L     RA,0(R1)                                                         
         USING PSTBLKD,RA                                                       
         L     R9,PSTACOM                                                       
         USING COMFACSD,R9                                                      
*                                                                               
         CLI   PSTACT,PSTVALQ      TEST VALIDATE                                
         BNE   FMT                                                              
         XC    PSTERR,PSTERR                                                    
         XC    PSTERFLD,PSTERFLD                                                
         XC    PSTERDSP,PSTERDSP                                                
         XC    TOTDISP,TOTDISP                                                  
         LA    R4,BLK                                                           
         XC    0(200,R4),0(R4)                                                  
         XC    200(120,R4),200(R4)                                              
         GOTO1 CSCANNER,DMCB,PSTADIN,(X'8A',BLK)                                
*                            MAX 10 OUTPUT FIELDS + RETURN FLD DISP             
         LA    R4,BLK                                                           
         USING SCAND,R4                                                         
         ZIC   R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    INVERR                                                           
*                                                                               
VAL10    CLI   SCLEN1,2            LHS MUST BE 2 CHARS LONG                     
         BNE   PRVERR                                                           
         CLI   SCLEN2,1            RHS MUST BE 1 CHAR LONG                      
         BNE   CDERR                                                            
*                                                                               
         LA    R3,PRVTAB           POINT TO PROVINCE/CODES TABLE                
         SR    R1,R1                                                            
*                                                                               
VAL20    CLI   0(R3),X'FF'                                                      
         BE    PRVERR                                                           
         CLC   0(2,R3),SCDATA1                                                  
         BE    VAL30                                                            
         LA    R1,1(R1)                                                         
         LA    R3,L'PRVTAB(R3)                                                  
         B     VAL20                                                            
*                                                                               
VAL30    LA    RE,2(R3)            POINT TO TABLE OF VALID CODES                
         LA    RF,L'PRVTAB-2       MAX VALID CODES                              
*                                                                               
VAL40    CLC   0(1,RE),SCDATA2                                                  
         BE    VAL50                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VAL40                                                         
         B     CDERR                                                            
*                                                                               
VAL50    L     R3,PSTADOUT         A(10 BYTE OUTPUT FLD)                        
         AR    R3,R1               DISP TO PROVINCE                             
         MVC   0(1,R3),SCDATA2     MOVE IN CODE                                 
*                                                                               
         LA    R4,SCANNEXT                                                      
         ZIC   R1,SCLEN1           RHS                                          
         ZIC   R3,SCLEN2           +LHS                                         
         AR    R1,R3                                                            
         AH    R1,TOTDISP                                                       
         LA    R1,2(R1)            + DELIMITER & COMMA                          
         STH   R1,TOTDISP                                                       
         BCT   R2,VAL10                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
FMT      DS    0H                                                               
         CLI   PSTACT,PSTFMTQ      TEST FORMAT                                  
         BNE   ACTERR                                                           
         L     R3,PSTADIN          A(10 BYTE INPUT FLD)                         
         L     R2,PSTADOUT         A(64 BYTE OUTPUT FLD)                        
         LA    R1,10               10 BYTES TO FORMAT                           
*                                                                               
FMT10    CLI   0(R3),0             DON'T PRINT FOR X'00'                        
         BE    FMT50                                                            
*                                                                               
FMT30    LA    R5,10                                                            
         SR    R5,R1                                                            
         MH    R5,=AL2(L'PRVTAB)   X ENTRY WIDTH                                
         LA    R5,PRVTAB(R5)       POINT TO PROVINCE TABLE ENTRY                
*                                                                               
         MVC   0(2,R2),0(R5)       OUTPUT PROVINCE                              
         MVI   2(R2),C'='                                                       
         MVC   3(1,R2),0(R3)       OUTPUT CODE                                  
         MVI   4(R2),C','                                                       
         LA    R2,5(R2)                                                         
*                                                                               
FMT50    LA    R3,1(R3)            BUMP TO NEXT PROVINCE                        
         BCT   R1,FMT10                                                         
*                                                                               
         BCTR  R2,0                BACK UP TO LAST OUTPUT CHAR                  
         C     R2,PSTADOUT         WAS THERE ANY OUTPUT                         
         BL    XIT                                                              
         CLI   0(R2),C','          CLEAR UP TRAILING COMMA                      
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ERROR EXITS                                                            
*                                                                               
PRVERR   LA    R4,BLK                                                           
         USING SCAND,R4                                                         
         MVI   PSTERR,PSTESRCQ                                                  
         MVC   PSTERDSP,TOTDISP    DISPLACEMENT INTO FIELD OF ERROR             
         B     XIT                                                              
*                                                                               
CDERR    LA    R4,BLK                                                           
         USING SCAND,R4                                                         
         MVI   PSTERR,PSTECDQ                                                   
         SR    R1,R1               DISPLACEMENT INTO FIELD OF ERROR             
         ICM   R1,3,TOTDISP                                                     
         LA    R1,3(R1)                                                         
         STCM  R1,3,PSTERDSP                                                    
         B     XIT                                                              
*                                                                               
ACTERR   MVI   PSTERR,PSTEACTQ                                                  
         B     XIT                                                              
*                                                                               
INVERR   MVI   PSTERR,PSTEINVQ                                                  
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
PRVTAB   DS    0CL8                                                             
         DC    C'BC',CL6'XZH '                                                  
         DC    C'AL',CL6'    '                                                  
         DC    C'SA',CL6'    '                                                  
         DC    C'MA',CL6'    '                                                  
         DC    C'ON',CL6'XZH '                                                  
         DC    C'PQ',CL6'XZQS*'                                                 
         DC    C'NB',CL6'XZH '                                                  
         DC    C'NS',CL6'XZH '                                                  
         DC    C'PE',CL6'XZH '                                                  
         DC    C'NF',CL6'XZH '                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DMCB     DS    6A                                                               
TOTDISP  DS    H                                                                
BLK      DS    XL320                                                            
WORKX    EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
*              DSECT TO COVER 32-BYTE SCAN BLOCK                                
         SPACE 1                                                                
SCAND    DSECT                                                                  
SCLEN1   DS    XL1  L'FIELD (OR L'FIRST HALF OF DIVIDED FIELD).                 
SCLEN2   DS    XL1  L'SECOND HALF OF DIVIDED FIELD OR ZERO.                     
SCVAL1   DS    XL1  VALIDITY BITS (X'80'=NUMERIC X'40'=ALPHA X'20'=HEX)         
SCVAL2   DS    XL1  VALIDITY BITS FOR SECOND HALF OF DIVIDED FIELDS.            
SCDISP1  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF LHS                   
         ORG   *-1                                                              
SCBIN1   DS    F    BINARY VALUE OF VALID NUMERIC FIELDS.                       
SCDISP2  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF RHS                   
         ORG   *-1                                                              
SCBIN2   DS    F    BINARY VALUE OF SECOND HALF OF DIVIDED FIELDS.              
SCDATA1  DS    CL10 LEFT JUSTIFIED FIELD DATA PADDED WITH SPACES.               
SCDATA2  DS    CL10 DATA FOR SECOND HALF OF DIVIDED FIELDS.                     
SCANNEXT EQU   *  (NOTE - UNDIVIDED FIELDS MAY BE UP TO 20 CHARACTERS.)         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032DDPSTVAL  03/06/13'                                      
         END                                                                    
