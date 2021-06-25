*          DATA SET ACADFORM   AT LEVEL 029 AS OF 09/26/12                      
*PHASE ADFORMB                                                                  
         TITLE 'ADFORM - FREEFORM ADDRESS FORMATTER'                            
         PRINT NOGEN                                                            
**********************************************************************          
* PARMS:  IF NO SENDING LENGTH, THEN JUST RETURN COUNTRY TABLE ADDR  *          
* 1= L'SENDING FIELD/ADDR OF SENDING FIELD                           *          
* 2= L'RECEIVING CITY FIELD/ADDR OF RECVG CITY FIELD                 *          
* 3= ADDRESS OF 2 POS. STATE RECVG FIELD                             *          
* 4= L'POSTAL CODE/ADDR OF RECVG POSTAL CODE FIELD                   *          
* 5= L'COUNTRY CODE/ADDR OF RECVG COUNTRY CODE FIELD                 *          
* 6= L'COUNTRY NAME/ADDR OF RECVG COUNTRY NAME FIELD                 *          
*                                                                    *          
* ERROR CODE PASSED BACK IN PARM 1:                                  *          
* X'00'= IT'S ALL GOOD.                                              *          
* X'01'= FOREIGN ADDRESS DETECTED. ONLY COUNTRY FILLED IN. NO OTHER  *          
*        VALIDATION DONE AND ALL OTHER FIELDS BLANK.                 *          
* X'02'= STATE CODE DID NOT MATCH ZIP CODE, STATE CODE CHANGED       *          
*        TO MATCH ZIP CODE.                                          *          
* X'08'= ZIP CODE LENGTH ERROR. NOT 5 OR 9 OR LONGER THAN            *          
*        RECEIVING FIELD (X'80' IS ALSO TURNED ON)                   *          
* X'80'= SERIOUS ERROR OR COUNTRY TABLE RETURN                       *          
**********************************************************************          
         EJECT                                                                  
ADFORM   CSECT                                                                  
         NMOD1 8,*ADFORM*,R9                                                    
*                                                                               
         ST    R1,PARAM            SAVE PARM LIST ADDR                          
         CLI   0(R1),0        NO SEND LENGTH THEN RETURN COUNTRY TAB            
         BNE   ADFRM05                                                          
         LA    R2,CTRYTAB                                                       
         STCM  R2,15,0(R1)                                                      
         B     ERRX                                                             
*                                                                               
ADFRM05  ZIC   R3,0(R1)            SEND LENGTH                                  
         L     R5,0(R1)                                                         
         ST    R5,SEND             SEND ADDR                                    
         NI    SEND,0              MAKE SURE IT'S A GOOD ADDRESS                
         MVC   0(4,R1),=F'0'                                                    
*                                                                               
         ZIC   R4,4(R1)                                                         
         STH   R4,CYL              CITY LENGTH                                  
         L     R5,4(R1)                                                         
         ST    R5,CITY             CITY ADDR                                    
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPCES                                                    
*                                                                               
         L     R4,8(R1)                                                         
         ST    R4,STATE            STATE ADDR                                   
         MVC   0(2,R4),=C'  '                                                   
*                                                                               
         L     R5,12(R1)                                                        
         ST    R5,ZIP              POSTAL CODE ADDR                             
         ZIC   R4,12(R1)                                                        
         STH   R4,ZPL              POSTAL CODE LENGTH                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPCES                                                    
*                                                                               
         L     R5,16(R1)                                                        
         ST    R5,CTRY             COUNTRY ADDRESS                              
         ZIC   R4,16(R1)                                                        
         STH   R4,CTRYL            COUNTRY LENGTH                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPCES                                                    
*                                                                               
         XC    CTRYNML,CTRYNML     ASSUME NO NAME REQUESTED                     
         ICM   R4,1,20(R1)                                                      
         BZ    ADFRM10                                                          
         STH   R4,CTRYNML          COUNTRY NAME LENGTH                          
         L     R5,20(R1)                                                        
         ST    R5,CTRYNM           COUNTRY NAME                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPCES                                                    
*                                                                               
ADFRM10  L     R4,SEND             GET TO BACK OF SEND                          
         CLI   0(R4),0             IF BINARY ZEROS, THEN ERROR EXIT             
         BE    ERRX                                                             
         AR    R4,R3                                                            
*********************************************************************           
*        COUNTRY CODE                                                           
*********************************************************************           
         SR    R5,R5                                                            
CTRY02   BCTR  R4,0                                                             
         C     R4,SEND                                                          
         BNH   ERRX                                                             
         CLI   0(R4),C' '            BACKUP PAST SPACES                         
         BE    CTRY02                                                           
*                                                                               
CTRY10   CLI   0(R4),C'A'            HIT A LETTER THAN MUST BE COUNTRY          
         BL    CTRY40                OTHERWISE MUST BE POSTAL CODE              
         CLI   0(R4),C'Z'                                                       
         BH    CTRY40                                                           
*                                                                               
CTRY15   BCTR  R4,0                  BACK UP UNTIL POSTAL CODE                  
CTRY16   C     R4,SEND                                                          
         BNH   ERRX                                                             
         AHI   R5,1                  KEEP TRACK OF COUNTRY LENGTH               
         CLI   0(R4),C' '                                                       
         BNE   CTRY15                                                           
*                                                                               
CTRY20   AHI   R4,1                  AT POSTAL CODE OR SPACE                    
         BCTR  R5,0                  ADJUST LENGTH WITH BUMPING                 
*                                                                               
         USING CTRYTABD,R2                                                      
         LA    R2,CTRYTAB                                                       
*                                                                               
CTRY21   CLI   CTRYC,X'FF'           TEST END OF COUNTRY TABLE                  
         BE    CTRY23                                                           
*                                                                               
         CHI   R5,1                  DOES IT EQUAL LENGTH OF CNTRYC-1           
         BH    CTRY25                IF MORE THEN USE FULL NAME                 
         BNE   CTRY23                                                           
*                                                                               
         CLC   CTRYC,0(R4)           TEST EQUAL TO 2 CHAR CNTRY CODE            
         BE    CTRY30                YES, SAVE IT                               
*                                                                               
CTRY22   ZIC   R0,CTRYNL             PICK UP LENGTH OF NAME                     
         AR    R2,R0                 BUMP PASSED THE NAME                       
         AHI   R2,CTRYOLQ                                                       
         B     CTRY21                                                           
*                                                                               
CTRY23   AHI   R4,-1                 SPACE BEFORE CHECK                         
         AHI   R5,1                  KEEP TRACK OF COUNTRY LENGTH               
         AHI   R4,-1                                                            
         CLI   0(R4),C' '            TWO SPACES IN A ROW?                       
         BE    ERRX                  YES, ERROR                                 
         B     CTRY16                                                           
*                                                                               
CTRY25   ZIC   R0,CTRYNL             LENGTH OF COUNTRY NAME IN TABLE            
                                                                                
         SHI   R0,1                                                             
         CR    R5,R0                                                            
         BNE   CTRY22                                                           
         EX    R5,*+8                COMPARE TO FULL COUNTRY NAME               
         B     *+10                                                             
         CLC   0(0,R4),CTRYN                                                    
         BNE   CTRY22                                                           
*                                                                               
CTRY30   L     R5,CTRY               COUNTRY OUT FIELD                          
         LH    R3,CTRYL                                                         
*                                                                               
         ZIC   R0,CTRYNL                                                        
         CR    R3,R0                                                            
         BNH   CTRY31                CNTRY NAME MAX CHARACTERS                  
         LA    R3,L'CTRYN            CNTRY NAME LNGTH FOR EX MOVE               
CTRY31   CHI   R3,L'CTRYC            CNTRY OUT LNGTH<3 THEN 3 CHAR CODE         
         BH    CTRY35                ELSE FULL NAME                             
         BCTR  R3,0                  DECREMENT FOR EX MOVE                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),CTRYC                                                    
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,CTRYNML          DO THEY WANT COUNTRY NAME BACK?            
         BZ    CTRY50                                                           
         L     R5,CTRYNM             COUNTRY NAME OUT FIELD                     
         ZIC   R1,CTRYNL             PICK UP LENGTH OF NAME                     
         CR    R3,R1                                                            
         BNH   *+8                                                              
         LR    R3,R1                                                            
*                                                                               
CTRY35   BCTR  R3,0                  DECREMENT FOR EX MOVE                      
         EX    R3,*+8                GET FULL COUNTRY NAME                      
         B     *+10                                                             
         MVC   0(0,R5),CTRYN                                                    
         B     CTRY50                                                           
*                                                                               
CTRY40   BCTR  R4,0                  BUMP TO NEXT TO LAST DIGIT IN PC           
         C     R4,SEND                                                          
         BNH   ERRX                                                             
         CLI   0(R4),C'0'                                                       
         BNL   CTRY42                MUST BE US POSTAL                          
*                                                                               
         AHI   R4,1                  BUMP TO END OF POSTAL CODE                 
         L     R5,CTRY               COUNTRY OUT FIELD                          
         CLI   0(R5),C' '                                                       
         BE    CTRY41                                                           
         CLI   0(R5),C'C'                                                       
         BE    CPC01                                                            
         B     ERRX                                                             
CTRY41   MVC   0(L'CTRYC,R5),=C'CA'                                             
         ICM   R3,3,CTRYNML          DO THEY WANT COUNTRY NAME BACK?            
         BZ    CPC01                                                            
         L     R5,CTRYNM             COUNTRY NAME OUT FIELD                     
         BCTR  R3,0                  DECREMENT FOR EX MOVE                      
         CH    R3,=H'5'                                                         
         BNH   *+8                                                              
         LA    R3,5                                                             
         EX    R3,*+8                GET FULL COUNTRY NAME                      
         B     *+10                                                             
         MVC   0(0,R5),=C'CANADA'                                               
         B     CPC01                                                            
*                                                                               
CTRY42   AHI   R4,1                  BUMP TO END OF POSTAL CODE                 
         L     R5,CTRY               COUNTRY OUT FIELD                          
         CLI   0(R5),C' '                                                       
         BE    CTRY43                                                           
         CLI   0(R5),C'U'                                                       
         BE    ZIP01                                                            
         B     ERRX                                                             
CTRY43   MVC   0(L'CTRYC,R5),=C'US'                                             
         ICM   R3,3,CTRYNML          DO THEY WANT COUNTRY NAME BACK?            
         BZ    ZIP01                                                            
         L     R5,CTRYNM             COUNTRY NAME OUT FIELD                     
         BCTR  R3,0                  DECREMENT FOR EX MOVE                      
         CH    R3,=H'12'                                                        
         BNH   *+8                                                              
         LA    R3,12                                                            
         EX    R3,*+8                GET FULL COUNTRY NAME                      
         B     *+10                                                             
         MVC   0(0,R5),=C'UNITED STATES'                                        
         B     ZIP01                                                            
*                                    END UP HERE IF COUNTRY WAS FOUND           
CTRY50   CLC   CTRYC,=C'US'          VALIDATE US?                               
         BE    CTRY60                YES                                        
         CLC   CTRYC,=C'CA'          OR CANADIAN?                               
         BNE   FORX                  NO                                         
*                                                                               
CTRY60   BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         BE    CTRY60                                                           
         CLI   0(R4),X'F0'                                                      
         BL    ERRX                                                             
         B     CTRY40                                                           
*                                                                               
         DROP  R2                                                               
*********************************************************************           
*        CANADIAN POSTAL CODE                                                   
*********************************************************************           
CPC01    SR    R5,R5                                                            
*                                                                               
         BAS   RE,CPC22                NUMBER                                   
         BCTR  R4,0                                                             
         BAS   RE,CPC23                LETTER                                   
         BCTR  R4,0                                                             
         BAS   RE,CPC22                NUMBER                                   
         BCTR  R4,0                                                             
         BAS   RE,CPC21                SPACE                                    
         BCTR  R4,0                                                             
         BAS   RE,CPC23                LETTER                                   
         BCTR  R4,0                                                             
         BAS   RE,CPC22                NUMBER                                   
         BCTR  R4,0                                                             
         BAS   RE,CPC23                LETTER                                   
         C     R4,SEND                                                          
         BNH   ERRX                                                             
*                                                                               
         L     R6,ZIP                                                           
         MVC   0(7,R6),0(R4)                                                    
*                                                                               
         BCTR  R4,0                                                             
         BAS   RE,CPC21                SPACE                                    
         B     PROV01                                                           
*                                                                               
CPC21    CLI   0(R4),C' '                                                       
         BNE   ERRX                                                             
         BR    RE                                                               
*                                                                               
CPC22    CLI   0(R4),X'F0'                                                      
         BL    ERRX                                                             
         CLI   0(R4),X'F9'                                                      
         BH    ERRX                                                             
         BR    RE                                                               
*                                                                               
CPC23    CLI   0(R4),C'A'                                                       
         BL    ERRX                                                             
         CLI   0(R4),C'Z'                                                       
         BH    ERRX                                                             
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
*        US ZIP CODE                                                            
*********************************************************************           
ZIP01    SR    R5,R5                                                            
*                                                                               
ZIP05    CLI   0(R4),C'-'          FOR 9 DIGIT ZIP CODES                        
         BE    ZIP11                                                            
         CLI   0(R4),X'F0'                                                      
         BL    ZIP15                                                            
         CLI   0(R4),X'F9'                                                      
         BNH   ZIP10                                                            
         B     ZIP15                                                            
*                                                                               
ZIP10    LA    R5,1(R5)                                                         
ZIP11    BCTR  R4,0                                                             
         B     ZIP05                                                            
*                                                                               
ZIP15    CH    R5,=H'5'                                                         
         BE    ZIP20                                                            
         CH    R5,=H'9'                                                         
         BNE   ERRX                                                             
*                                                                               
ZIP20    LH    R6,ZPL                                                           
         CR    R5,R6                                                            
         BH    ERRZLX                                                           
         L     R6,ZIP                                                           
         LR    RF,R4                                                            
         B     ZIP32                                                            
         SPACE                                                                  
ZIP30    LA    R6,1(R6)                                                         
ZIP31    LA    RF,1(RF)                                                         
ZIP32    CLI   1(RF),C'-'          FOR 9 DIGIT ZIP CODE.                        
         BE    ZIP31                                                            
         MVC   0(1,R6),1(RF)                                                    
         BCT   R5,ZIP30                                                         
         B     STATE1                                                           
*********************************************************************           
*        STATE                                                                  
*********************************************************************           
PROV01   BCTR  R4,0                                                             
         C     R4,SEND                                                          
         BNH   ERRX                                                             
         CLI   0(R4),C'A'                                                       
         BL    PROV01                                                           
         CLI   0(R4),C'Z'                                                       
         BH    PROV01                                                           
         L     R5,STATE                                                         
         MVC   1(1,R5),0(R4)                                                    
*                                                                               
         BCTR  R4,0                                                             
         CLI   0(R4),C'A'                                                       
         BL    ERRX                                                             
         CLI   0(R4),C'Z'                                                       
         BH    ERRX                                                             
         MVC   0(1,R5),0(R4)                                                    
*                                                                               
         LA    R1,CANPTBL                                                       
         L     R2,ZIP                                                           
PROV10   CLC   0(1,R1),0(R2)                                                    
         BE    PROV15                                                           
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    PROV18                                                           
         B     PROV10                                                           
PROV15   CLC   1(2,R1),0(R5)                                                    
         BE    PROV20                                                           
PROV18   L     R3,PARAM                                                         
         LHI   R6,2                                                             
         ST    R6,0(R3)                                                         
         MVC   0(2,R5),1(R1)                                                    
         LA    R4,2(R4)              THOSE LAST 2 BYTES.                        
*                                                                               
PROV20   B     CITY1                                                            
*********************************************************************           
*        STATE                                                                  
*********************************************************************           
STATE0   BCTR  R4,0                                                             
STATE1   CLI   0(R4),C'A'                                                       
         BL    STATE0                                                           
         CLI   0(R4),C'Z'                                                       
         BH    STATE0                                                           
         L     R5,STATE                                                         
         MVC   1(1,R5),0(R4)                                                    
STATE2   BCTR  R4,0                                                             
         CLI   0(R4),C'A'                                                       
         BL    STATE2                                                           
         CLI   0(R4),C'Z'                                                       
         BH    STATE2                                                           
         MVC   0(1,R5),0(R4)                                                    
*                                                                               
STATE3   LA    R1,USSTTBL                                                       
         L     R2,ZIP                                                           
STATE4   CLC   3(3,R1),0(R2)                                                    
         BNL   STATE5                                                           
         LA    R1,10(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    ERRX                                                             
         B     STATE4                                                           
STATE5   CLC   0(3,R1),0(R2)                                                    
         BH    ERRX                                                             
         CLC   0(2,R5),6(R1)                                                    
         BE    STATE6                                                           
         L     R3,PARAM                                                         
         OI    0(R3),X'02'    ERROR STATE DOES NOT MATCH ZIP                    
         MVC   0(2,R5),6(R1)                                                    
*                          IF INPUT IS NOT A STATE, THEN DON'T IGNORE           
         LA    R4,2(R4)    THOSE LAST 2 BYTES.                                  
STATE6   B     CITY1                                                            
*********************************************************************           
*        CITY                                                                   
*********************************************************************           
CITY1    BCTR  R4,0                                                             
         C     R4,SEND                                                          
         BL    XIT                                                              
         CLI   0(R4),C' '          BUMP PAST SPACES AND COMMA                   
         BE    CITY1                                                            
         CLI   0(R4),C','                                                       
         BE    CITY1                                                            
         SR    R6,R6                                                            
         L     R5,SEND                                                          
CITY2    BCTR  R4,0                                                             
         LA    R6,1(R6)                                                         
         CR    R4,R5                                                            
         BNL   CITY2                                                            
         CH    R6,CYL                                                           
         BH    ERRX                                                             
         L     R2,CITY                                                          
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),1(R4)                                                    
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
ERRX     L     R1,PARAM           ERROR - NOT ABLE TO VALIDATE                  
         OI    0(R1),X'80'                                                      
         B     XIT                                                              
*                                                                               
ERRZLX   L     R1,PARAM           ZIP CODE LENGTH ERROR                         
         OI    0(R1),X'80'+X'08'                                                
         ST    R5,4(R1)           ACTUAL LENGTH IN PARM 2                       
         LH    R5,ZPL             CORRECT LENGTH IN PARM 3                      
         ST    R5,8(R1)                                                         
         B     XIT                                                              
*                                                                               
FORX     L     R1,PARAM           EXIT FOR FOREIGN ADDRESS                      
         OI    0(R1),X'01'                                                      
         B     XIT                                                              
*********************************************************************           
*        ADDRESSES AND LOCAL STORAGE                                            
*********************************************************************           
SEND     DS    A                                                                
CITY     DS    A                                                                
STATE    DS    A                                                                
ZIP      DS    A                                                                
CTRY     DS    A                                                                
CTRYNM   DS    A                                                                
PARAM    DS    A                                                                
CYL      DS    H                   CITY LENGTH                                  
ZPL      DS    H                   ZIP LENGTH                                   
CTRYL    DS    H                   COUNTRY LENGTH                               
CTRYNML  DS    H                   COUNTRY NAME LENGTH                          
SPCES    DC    CL50' '                                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        COUNTRY TABLE                                               *          
*          POS 1-18 EIGHTEEN CHARACTER COUNTRY NAME                  *          
*          19-20    TWO CHARACTER COUNTRY CODE                       *          
*          21       VALIDATION NEEDED OR FOREIGN COUNTRY INDICATOR   *          
**********************************************************************          
CTRYTAB  DS    0C                                                               
         DC    CL2'AF',AL1(11),C'AFGHANISTAN'                                   
         DC    CL2'AX',AL1(13),C'ALAND ISLANDS'                                 
         DC    CL2'AL',AL1(07),C'ALBANIA'                                       
         DC    CL2'DZ',AL1(07),C'ALGERIA'                                       
         DC    CL2'AS',AL1(14),C'AMERICAN SAMOA'                                
         DC    CL2'AD',AL1(07),C'ANDORRA'                                       
         DC    CL2'AO',AL1(06),C'ANGOLA'                                        
         DC    CL2'AI',AL1(08),C'ANGUILLA'                                      
         DC    CL2'AQ',AL1(10),C'ANTARCTICA'                                    
         DC    CL2'AG',AL1(07),C'ANTIGUA'                                       
         DC    CL2'AR',AL1(09),C'ARGENTINA'                                     
         DC    CL2'AM',AL1(07),C'ARMENIA'                                       
         DC    CL2'AW',AL1(05),C'ARUBA'                                         
         DC    CL2'AU',AL1(09),C'AUSTRALIA'                                     
         DC    CL2'AT',AL1(07),C'AUSTRIA'                                       
         DC    CL2'AZ',AL1(10),C'AZERBAIJAN'                                    
         DC    CL2'BL',AL1(16),C'SAINT BARTHELEMY'                              
         DC    CL2'BS',AL1(07),C'BAHAMAS'                                       
         DC    CL2'BH',AL1(07),C'BAHRAIN'                                       
         DC    CL2'BD',AL1(10),C'BANGLADESH'                                    
         DC    CL2'BB',AL1(08),C'BARBADOS'                                      
         DC    CL2'BY',AL1(07),C'BELARUS'                                       
         DC    CL2'BE',AL1(07),C'BELGIUM'                                       
         DC    CL2'BZ',AL1(06),C'BELIZE'                                        
         DC    CL2'BJ',AL1(05),C'BENIN'                                         
         DC    CL2'BM',AL1(07),C'BERMUDA'                                       
         DC    CL2'BT',AL1(06),C'BHUTAN'                                        
         DC    CL2'BO',AL1(07),C'BOLIVIA'                                       
         DC    CL2'BQ',AL1(07),C'BONAIRE'                                       
         DC    CL2'BA',AL1(22),C'BOSNIA AND HERZEGOVINA'                        
         DC    CL2'BW',AL1(08),C'BOTSWANA'                                      
         DC    CL2'BV',AL1(13),C'BOUVET ISLAND'                                 
         DC    CL2'BR',AL1(06),C'BRAZIL'                                        
         DC    CL2'IO',AL1(30),C'BRITISH INDIAN OCEAN TERRITORY'                
         DC    CL2'BN',AL1(17),C'BRUNEI DARUSSALAM'                             
         DC    CL2'BG',AL1(08),C'BULGARIA'                                      
         DC    CL2'BF',AL1(12),C'BURKINA FASO'                                  
         DC    CL2'BI',AL1(07),C'BURUNDI'                                       
         DC    CL2'KH',AL1(08),C'CAMBODIA'                                      
         DC    CL2'CM',AL1(08),C'CAMEROON'                                      
         DC    CL2'CA',AL1(06),C'CANADA'                                        
         DC    CL2'CV',AL1(10),C'CAPE VERDE'                                    
         DC    CL2'KY',AL1(14),C'CAYMAN ISLANDS'                                
         DC    CL2'CF',AL1(24),C'CENTRAL AFRICAN REPUBLIC'                      
         DC    CL2'TD',AL1(04),C'CHAD'                                          
         DC    CL2'CL',AL1(05),C'CHILE'                                         
         DC    CL2'CN',AL1(05),C'CHINA'                                         
         DC    CL2'CX',AL1(16),C'CHRISTMAS ISLAND'                              
         DC    CL2'CC',AL1(23),C'COCOS (KEELING) ISLANDS'                       
         DC    CL2'CO',AL1(08),C'COLOMBIA'                                      
         DC    CL2'KM',AL1(07),C'COMOROS'                                       
         DC    CL2'CG',AL1(05),C'CONGO'                                         
         DC    CL2'CD',AL1(37),C'CONGO, THE DEMOCRATIC REPUBLIC OF THE'         
         DC    CL2'CK',AL1(12),C'COOK ISLANDS'                                  
         DC    CL2'CR',AL1(10),C'COSTA RICA'                                    
         DC    CL2'CI',AL1(12),C'COTE DIVOIRE'                                  
         DC    CL2'HR',AL1(07),C'CROATIA'                                       
         DC    CL2'CU',AL1(04),C'CUBA'                                          
         DC    CL2'CW',AL1(07),C'CURACAO'                                       
         DC    CL2'CY',AL1(06),C'CYPRUS'                                        
         DC    CL2'CZ',AL1(14),C'CZECH REPUBLIC'                                
         DC    CL2'DK',AL1(07),C'DENMARK'                                       
         DC    CL2'DJ',AL1(08),C'DJIBOUTI'                                      
         DC    CL2'DM',AL1(08),C'DOMINICA'                                      
         DC    CL2'DO',AL1(18),C'DOMINICAN REPUBLIC'                            
         DC    CL2'GB',AL1(07),C'ENGLAND'                                       
         DC    CL2'EC',AL1(07),C'ECUADOR'                                       
         DC    CL2'EG',AL1(05),C'EGYPT'                                         
         DC    CL2'SV',AL1(11),C'EL SALVADOR'                                   
         DC    CL2'GQ',AL1(17),C'EQUATORIAL GUINEA'                             
         DC    CL2'ER',AL1(07),C'ERITREA'                                       
         DC    CL2'EE',AL1(07),C'ESTONIA'                                       
         DC    CL2'ET',AL1(08),C'ETHIOPIA'                                      
         DC    CL2'FK',AL1(16),C'FALKLAND ISLANDS'                              
         DC    CL2'FO',AL1(13),C'FAROE ISLANDS'                                 
         DC    CL2'FJ',AL1(04),C'FIJI'                                          
         DC    CL2'FI',AL1(07),C'FINLAND'                                       
         DC    CL2'FR',AL1(06),C'FRANCE'                                        
         DC    CL2'GF',AL1(13),C'FRENCH GUIANA'                                 
         DC    CL2'PF',AL1(16),C'FRENCH POLYNESIA'                              
         DC    CL2'TF',AL1(27),C'FRENCH SOUTHERN TERRITORIES'                   
         DC    CL2'GA',AL1(05),C'GABON'                                         
         DC    CL2'GM',AL1(06),C'GAMBIA'                                        
         DC    CL2'DE',AL1(07),C'GERMANY'                                       
         DC    CL2'GH',AL1(05),C'GHANA'                                         
         DC    CL2'GI',AL1(09),C'GIBRALTAR'                                     
         DC    CL2'GE',AL1(07),C'GEORGIA'                                       
         DC    CL2'GR',AL1(06),C'GREECE'                                        
         DC    CL2'GL',AL1(09),C'GREENLAND'                                     
         DC    CL2'GD',AL1(07),C'GRENADA'                                       
         DC    CL2'GP',AL1(10),C'GUADELOUPE'                                    
         DC    CL2'GU',AL1(04),C'GUAM'                                          
         DC    CL2'GT',AL1(09),C'GUATEMALA'                                     
         DC    CL2'GG',AL1(08),C'GUERNSEY'                                      
         DC    CL2'GN',AL1(06),C'GUINEA'                                        
         DC    CL2'GW',AL1(13),C'GUINEA-BISSAU'                                 
         DC    CL2'GY',AL1(06),C'GUYANA'                                        
         DC    CL2'HT',AL1(05),C'HAITI'                                         
         DC    CL2'HM',AL1(33),C'HEARD ISLAND AND MCDONALD ISLANDS'             
         DC    CL2'VA',AL1(29),C'HOLY SEE (VATICAN CITY STATE)'                 
         DC    CL2'HK',AL1(09),C'HONG KONG'                                     
         DC    CL2'HU',AL1(07),C'HUNGARY'                                       
         DC    CL2'HN',AL1(08),C'HONDURAS'                                      
         DC    CL2'IS',AL1(07),C'ICELAND'                                       
         DC    CL2'IN',AL1(05),C'INDIA'                                         
         DC    CL2'ID',AL1(09),C'INDONESIA'                                     
         DC    CL2'IE',AL1(07),C'IRELAND'                                       
         DC    CL2'IL',AL1(06),C'ISRAEL'                                        
         DC    CL2'IT',AL1(05),C'ITALY'                                         
         DC    CL2'JM',AL1(07),C'JAMAICA'                                       
         DC    CL2'JO',AL1(06),C'JORDAN'                                        
         DC    CL2'JP',AL1(05),C'JAPAN'                                         
         DC    CL2'KZ',AL1(10),C'KAZAKHSTAN'                                    
         DC    CL2'KE',AL1(05),C'KENYA'                                         
         DC    CL2'KI',AL1(08),C'KIRIBATI'                                      
         DC    CL2'KP',AL1(38)                                                  
         DC    C'KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF'                       
         DC    CL2'KR',AL1(18),C'KOREA, REPUBLIC OF'                            
         DC    CL2'KW',AL1(06),C'KUWAIT'                                        
         DC    CL2'KG',AL1(10),C'KYRGYZSTAN'                                    
         DC    CL2'LA',AL1(04),C'LAOS'                                          
         DC    CL2'LV',AL1(06),C'LATVIA'                                        
         DC    CL2'LB',AL1(07),C'LEBANON'                                       
         DC    CL2'LS',AL1(07),C'LESOTHO'                                       
         DC    CL2'LR',AL1(07),C'LIBERIA'                                       
         DC    CL2'LY',AL1(05),C'LIBYA'                                         
         DC    CL2'LI',AL1(13),C'LIECHTENSTEIN'                                 
         DC    CL2'LT',AL1(09),C'LITHUANIA'                                     
         DC    CL2'LU',AL1(10),C'LUXEMBOURG'                                    
         DC    CL2'MO',AL1(05),C'MACAO'                                         
         DC    CL2'MK',AL1(09),C'MACEDONIA'                                     
         DC    CL2'MG',AL1(10),C'MADAGASCAR'                                    
         DC    CL2'MW',AL1(06),C'MALAWI'                                        
         DC    CL2'MY',AL1(08),C'MALAYSIA'                                      
         DC    CL2'MV',AL1(08),C'MALDIVES'                                      
         DC    CL2'ML',AL1(04),C'MALI'                                          
         DC    CL2'MT',AL1(05),C'MALTA'                                         
         DC    CL2'MH',AL1(16),C'MARSHALL ISLANDS'                              
         DC    CL2'MQ',AL1(19),C'MARTINIQUE (FRENCH)'                           
         DC    CL2'MR',AL1(10),C'MAURITANIA'                                    
         DC    CL2'MU',AL1(09),C'MAURITIUS'                                     
         DC    CL2'YT',AL1(07),C'MAYOTTE'                                       
         DC    CL2'MX',AL1(06),C'MEXICO'                                        
         DC    CL2'FM',AL1(10),C'MICRONESIA'                                    
         DC    CL2'MD',AL1(07),C'MOLDOVA'                                       
         DC    CL2'MC',AL1(06),C'MONACO'                                        
         DC    CL2'MN',AL1(08),C'MONGOLIA'                                      
         DC    CL2'MS',AL1(10),C'MONTSERRAT'                                    
         DC    CL2'MA',AL1(07),C'MOROCCO'                                       
         DC    CL2'MZ',AL1(10),C'MOZAMBIQUE'                                    
         DC    CL2'MM',AL1(07),C'MYANMAR'                                       
         DC    CL2'NI',AL1(09),C'NICARAGUA'                                     
         DC    CL2'NL',AL1(11),C'NETHERLANDS'                                   
         DC    CL2'AN',AL1(20),C'NETHERLANDS ANTILLES'                          
         DC    CL2'NZ',AL1(11),C'NEW ZEALAND'                                   
         DC    CL2'NO',AL1(06),C'NORWAY'                                        
         DC    CL2'PA',AL1(06),C'PANAMA'                                        
         DC    CL2'PE',AL1(04),C'PERU'                                          
         DC    CL2'PH',AL1(11),C'PHILIPPINES'                                   
         DC    CL2'PK',AL1(08),C'PAKISTAN'                                      
         DC    CL2'PL',AL1(06),C'POLAND'                                        
         DC    CL2'PT',AL1(08),C'PORTUGAL'                                      
         DC    CL2'PR',AL1(11),C'PUERTO RICO'                                   
         DC    CL2'RS',AL1(06),C'SERBIA'                                        
         DC    CL2'RU',AL1(06),C'RUSSIA'                                        
         DC    CL2'SA',AL1(12),C'SAUDI ARABIA'                                  
         DC    CL2'SG',AL1(09),C'SINGAPORE'                                     
         DC    CL2'ZA',AL1(12),C'SOUTH AFRICA'                                  
         DC    CL2'ES',AL1(05),C'SPAIN'                                         
         DC    CL2'SE',AL1(06),C'SWEDEN'                                        
         DC    CL2'CH',AL1(11),C'SWITZERLAND'                                   
         DC    CL2'TH',AL1(08),C'THAILAND'                                      
         DC    CL2'TT',AL1(19),C'TRINIDAD AND TOBAGO'                           
         DC    CL2'TW',AL1(06),C'TAIWAN'                                        
         DC    CL2'TR',AL1(06),C'TURKEY'                                        
         DC    CL2'AE',AL1(20),C'UNITED ARAB EMIRATES'                          
         DC    CL2'AE',AL1(06),C'U.A.E.'                                        
         DC    CL2'GB',AL1(14),C'UNITED KINGDOM'                                
         DC    CL2'US',AL1(13),C'UNITED STATES'                                 
         DC    CL2'UY',AL1(07),C'URUGUAY'                                       
         DC    CL2'VE',AL1(33),C'VENEZUELA, BOLIVARIAN REPUBLIC OF'             
         DC    CL2'VG',AL1(24),C'VIRGIN ISLANDS (BRITISH)'                      
         DC    CL2'VI',AL1(20),C'VIRGIN ISLANDS (USA)'                          
         DC    X'FFFF',AL1(1),X'FF'                                             
*********************************************************************           
*        STATE TABLE                                                            
*           POS 1-3 FIRST 3 ZIP DIGITS FROM                                     
*               4-6 FIRST 3 ZIP DIGITS TO                                       
*               7-8 STATE NAME ABBR (OFFICIAL US POSTAL CODE)                   
*               9-10 STATE CODE                                                 
**********************************************************************          
USSTTBL  DS    0H                                                               
         DC    CL10'006009PRPR'                                                 
         DC    CL10'008008VIVI'                                                 
         DC    CL10'010027MA25'                                                 
         DC    CL10'028029RI44'                                                 
         DC    CL10'030038NH33'                                                 
         DC    CL10'039049ME23'                                                 
         DC    CL10'050059VT50'                                                 
         DC    CL10'060069CT09'                                                 
         DC    CL10'070089NJ34'                                                 
         DC    CL10'090149NY36'                                                 
         DC    CL10'150196PA42'                                                 
         DC    CL10'197199DE10'                                                 
         DC    CL10'200200DC11'                                                 
         DC    CL10'201201VA51'                                                 
         DC    CL10'202205DC11'                                                 
         DC    CL10'206219MD24'                                                 
         DC    CL10'220246VA51'                                                 
         DC    CL10'247268WV54'                                                 
         DC    CL10'270289NC37'                                                 
         DC    CL10'290299SC45'                                                 
         DC    CL10'300319GA13'                                                 
         DC    CL10'320349FL12'                                                 
         DC    CL10'350369AL01'                                                 
         DC    CL10'370385TN47'                                                 
         DC    CL10'386397MS28'                                                 
         DC    CL10'398399GA13'    A 2ND GA ENTRY                               
         DC    CL10'400427KY21'                                                 
         DC    CL10'430458OH39'                                                 
         DC    CL10'460479IN18'                                                 
         DC    CL10'480499MI26'                                                 
         DC    CL10'500528IA19'                                                 
         DC    CL10'530549WI55'                                                 
         DC    CL10'550567MN27'                                                 
         DC    CL10'570577SD46'                                                 
         DC    CL10'580588ND38'                                                 
         DC    CL10'590599MT30'                                                 
         DC    CL10'600629IL17'                                                 
         DC    CL10'630658MO29'                                                 
         DC    CL10'660679KS20'                                                 
         DC    CL10'680693NE31'                                                 
         DC    CL10'700714LA22'                                                 
         DC    CL10'716729AR05'                                                 
         DC    CL10'730749OK40'                                                 
         DC    CL10'750799TX48'                                                 
         DC    CL10'800816CO08'                                                 
         DC    CL10'820831WY56'                                                 
         DC    CL10'832838ID16'                                                 
         DC    CL10'840847UT49'                                                 
         DC    CL10'850865AZ04'                                                 
         DC    CL10'870884NM35'                                                 
         DC    CL10'889898NV32'                                                 
         DC    CL10'900966CA06'                                                 
         DC    CL10'967968HI15'                                                 
         DC    CL10'969969GU66'                                                 
         DC    CL10'970979OR41'                                                 
         DC    CL10'980994WA53'                                                 
         DC    CL10'995999AK02'                                                 
         DC    X'FF'                                                            
*********************************************************************           
*        PROVINCE TABLE                                                         
*           POS 1 FIRST LETTER OF POSTAL CODE                                   
*               2-3 OFFICIAL PROVINCE CODE                                      
**********************************************************************          
CANPTBL  DS    0H                                                               
         DC    CL3'ANF'             NEWFOUNDLAND                                
         DC    CL3'BNS'             NOVA SCOTIA                                 
         DC    CL3'CPE'             PRINCE EDWARD ISLAND                        
         DC    CL3'ENB'             NEW BRUNSWICK                               
         DC    CL3'GQC'             QUEBEC                                      
         DC    CL3'HQC'             QUEBEC                                      
         DC    CL3'JQC'             QUEBEC                                      
         DC    CL3'KON'             ONTARIO                                     
         DC    CL3'LON'             ONTARIO                                     
         DC    CL3'MON'             ONTARIO                                     
         DC    CL3'NON'             ONTARIO                                     
         DC    CL3'PON'             ONTARIO                                     
         DC    CL3'RMB'             MANITOBA                                    
         DC    CL3'SSK'             SASKATCHEWAN                                
         DC    CL3'TAB'             ALBERTA                                     
         DC    CL3'VBC'             BRITISH COLUMBIA                            
         DC    CL3'XNT'             NORTHWEST TERRITORIES, NUNAVUT              
         DC    CL3'YYT'             YUKON                                       
         DC    X'FF',C'**'          INVALID POSTAL CODE PREFIX                  
**********************************************************************          
*          ADDRESS FORMATTING COUNTRY TABLE DSECT                               
**********************************************************************          
       ++INCLUDE ACADFORMD                                                      
**********************************************************************          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACADFORM  09/26/12'                                      
         END                                                                    
