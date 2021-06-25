*          DATA SET SPACNVALX  AT LEVEL 020 AS OF 05/01/02                      
*PHASE T00A44A                                                                  
         SPACE 2                                                                
***********************************************************************         
* PARAM1 BYTE(S) 1-3 = A(SPACNVAL INTERFACE BLOCK)                    *         
***********************************************************************         
         TITLE 'SPACNVAL - SPOT ACN NUMBER VALIDATION'                          
SPACNVAL CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPACNVAL,RA,CLEAR=YES,RR=R8                          
*                                                                               
         USING WORKD,RC            RC = A(W/S)                                  
         L     R9,0(R1)                                                         
         USING SPAVBLKD,R9         R9 = A(INTERFACE BLOCK)                      
         L     R8,SPAVATWA                                                      
         USING TWAD,R8             R8 = A(TWA)                                  
*                                                                               
         L     RF,SPAVACMF                                                      
         MVC   VSWITCH,CSWITCH-COMFACSD(RF)                                     
         MVC   VDATAMGR,CDATAMGR-COMFACSD(RF)                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   SPAVERR,0           PRESET ERROR CODE                            
         B     VA01                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* READ CONTROL FILE TO LOCATE CCUSA ACCTG FILE *                                
*                                                                               
VA01     OC    VSWITCH,VSWITCH    TEST OFFLINE                                  
         BZ    VA12                                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+15(10),=CL10'CCUSA'                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,IOAREA                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(25),IOAREA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         LA    R1,IOAREA+28                                                     
VA05     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'21'         TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   2(R1),X'06'         TEST ACCOUNTING                              
         BE    VA10                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VA05                                                             
*                                                                               
VA10     MVC   SPAVASYS,3(R1)      SAVE ACC SYSTEM AND                          
         MVC   SPAVACOM,4(R1)       COMPANY CODE                                
*                                                                               
         CLC   TWAORIG,=X'0011'    ** SPECIAL FOR SJR                           
         BNE   *+12                **                                           
         MVI   SPAVASYS,X'66'      **                                           
         MVI   SPAVACOM,X'F9'      **                                           
*                                                                               
         CLC   TWAORIG,=AL2(CCUSAID)    TEST ID = CCUSA                         
         BE    VA12                                                             
         CLC   TWAORIG,=AL2(COKEAT)     TEST ID = COKEAT                        
         BNE   VA25                                                             
*                                                                               
VA12     DS    0H                                                               
         LA    R0,8                COUNT NUMBER OF DIGITS PRESENT               
         LA    R1,SPAVCIFC                                                      
VA15     CLI   0(R1),C' '                                                       
         BNH   VA20                                                             
         CLI   0(R1),C'0'                                                       
         BL    ACNERR1                                                          
         CLI   0(R1),C'9'                                                       
         BH    ACNERR1                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VA15                                                          
*                                                                               
VA20     LA    RE,8                                                             
         SR    RE,R0               GIVES LENGTH PRESENT                         
         BZ    ACNERR1                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SPAVCIFC(0)     * EXECUTED *                                 
         CP    DUB,=P'0'                                                        
**NOP**  BE    ACNERR1             MH  09FEB96                                  
         OI    DUB+7,X'0F'                                                      
         UNPK  SPAVAAGY,DUB                                                     
         B     VA65                                                             
         SPACE 1                                                                
* READ ID RECORD FROM CONTROL FILE *                                            
*                                                                               
VA25     XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+23(2),TWAORIG                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,IOAREA                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(25),IOAREA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         LA    R1,IOAREA+28                                                     
VA30     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'         TEST DESC ELEMENT                            
         BE    VA35                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VA30                                                             
*                                                                               
VA35     CLC   =C'CC',2(R1)        TEST ID LOOKS LIKE COKE                      
         BNE   VA40                                                             
         MVC   SPAVAAGY,4(R1)      SAVE AGENCY NUMBER                           
         B     VA65                                                             
*                                                                               
VA40     CLC   =C'RM',2(R1)        TEST ID LOOKS LIKE REGIONAL MKTG             
         BNE   VA50                                                             
* MAKE SURE THREE DIGITS FOLLOW                                                 
         LA    RE,4(R1)            POINT TO FIRST DIGIT                         
         LA    RF,3                                                             
VA45     CLI   0(RE),C'0'                                                       
         BL    ACNERR1                                                          
         CLI   0(RE),C'9'                                                       
         BH    ACNERR1                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VA45                                                          
         MVC   SPAVAAGY,4(R1)      MOVE AGENCY NUMBER FROM ID                   
         B     VA65                                                             
*                                                                               
VA50     ZIC   RE,1(R1)            GET ELEMENT LENGTH                           
         LA    RE,0(RE,R1)         POINT TO NEXT ELEM                           
         BCTR  RE,0                BACK UP TO END OF THIS ELEM                  
         CLI   0(RE),C' '          TEST BLANK                                   
         BNH   *-6                                                              
         BCTR  RE,0                BACK UP 1 MORE TO POINT TO OFFICE            
*                                                                               
         LA    R4,CCAGYLST                                                      
*                                                                               
VA55     CLC   0(2,R4),SPAVAGY     MATCH ALPHA AGENCY                           
         BNE   VA60                                                             
         CLC   2(2,R4),0(RE)       MATCH OFFICE                                 
         BNE   VA60                                                             
         MVC   SPAVAAGY,4(R4)                                                   
         B     VA65                                                             
*                                                                               
VA60     LA    R4,7(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   VA55                                                             
         B     ACNERR1                                                          
*                                                                               
VA65     XC    SPAVDACN,SPAVDACN                                                
         ICM   R1,15,SPAVAACN      TEST ACN NUMBER ENTERED                      
         BZ    VA75                                                             
         OC    0(12,R1),0(R1)                                                   
         BZ    VA75                                                             
         MVC   SPAVDACN,0(R1)      YES - SAVE OVERRIDE ACN NUMBER               
         SPACE 1                                                                
* MAKE SURE ACN IS 5 DIGITS *                                                   
*                                                                               
         LA    R0,5                                                             
VA70     CLI   0(R1),C'0'                                                       
         BL    ACNERR2                                                          
         CLI   0(R1),C'9'                                                       
         BH    ACNERR2                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VA70                                                          
         CLI   0(R1),C' '                                                       
         BH    ACNERR2                                                          
*                                                                               
VA75     DS    0H                  SWITCH TO ACC SYSTEM                         
         OC    VSWITCH,VSWITCH                                                  
         BZ    VA78                                                             
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SPAVASYS    SET SYSTEM NUMBER                            
         GOTO1 VSWITCH,DMCB                                                     
         CLI   4(R1),2             TEST ACC NOT OP                              
         BE    ACNERR3                                                          
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VA78     OC    SPAVDACN,SPAVDACN   TEST OVERRIDE ACN ENTERED                    
         BNZ   VA80                                                             
         SPACE 1                                                                
* READ VEHICLE RECORD FOR DEFAULT ACN NUMBER *                                  
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),SPAVACOM    SET COMPANY                                  
         MVC   WORK+1(2),=C'3M'                                                 
         MVI   WORK+3,C'S'                                                      
         MVC   WORK+4(1),SPAVMED                                                
         MVC   WORK+5(4),SPAVSTA                                                
         MVI   WORK+9,C'-'                                                      
         MVC   WORK+10(1),SPAVSTA+4                                             
         MVI   WORK+11,C'V'        ASSUME TV                                    
         CLI   WORK+10,C'T'                                                     
         BE    *+8                                                              
         MVI   WORK+11,C'M'                                                     
         BAS   RE,ACCHIGH                                                       
         CLC   WORK(42),IOAREA                                                  
         BNE   ACNERR4                                                          
*                                                                               
         MVI   ELCODE,X'23'        FIND ACN NUMBER ELEMENT                      
         BAS   RE,GETACNEL                                                      
         BNE   ACNERR5                                                          
*                                                                               
         MVC   SPAVDACN(5),2(R1)   SAVE DEFAULT ACN NUMBER                      
         OC    SPAVDACN,SPACES     FILL WITH BLANKS                             
         SPACE 1                                                                
* VALIDATE ACN AND AGENCY ON SE LEDGER *                                        
*                                                                               
VA80     MVC   WORK,SPACES                                                      
         MVC   WORK(1),SPAVACOM    SET COMPANY                                  
         MVC   WORK+1(2),=C'SE'                                                 
         MVC   WORK+3(5),SPAVDACN  SET ACN NUMBER                               
         GOTO1 ACCHIGH                                                          
         CLC   WORK(42),IOAREA                                                  
         BNE   ACNERR2             SET INVALID ACN ERR                          
         SPACE 1                                                                
* DIG OUT BOTTLER NAME AND SAVE *                                               
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETACNEL                                                      
         BE    *+8                                                              
         LA    R1,=20C'*'                                                       
         MVC   SPAVNAME,2(R1)                                                   
*                                                                               
         MVC   WORK+8(3),SPAVAAGY                                               
         GOTO1 ACCHIGH                                                          
         CLC   WORK(42),IOAREA                                                  
         BNE   ACNERR6                                                          
* DIG OUT ACCOUNT STATUS ELEMENT                                                
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETACNEL                                                      
         BNE   ACNERR6                                                          
         USING RSTELD,R1                                                        
         TM    RSTSTAT,X'20'                                                    
         BO    ACNERR6             ACCOUNT IS LOCKED                            
         DROP  R1                                                               
*                                                                               
         ICM   RF,15,VSWITCH                                                    
         BZ    VAX                                                              
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
VAX      B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINES                                                                   
*                                                                               
GETACNEL LA    R1,IOAREA+49                                                     
         SR    R0,R0                                                            
ACNEL2   CLC   ELCODE,0(R1)                                                     
         BER   RE                  RETURN WITH CC EQ                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   ACNEL2                                                           
         LTR   RE,RE               RETURN WITH CC NOT EQ                        
         BR    RE                                                               
*                                                                               
ACCHIGH  LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',WORK,IOAREA                 
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
ACNERR1  LA    R1,LACNMSG1         BAD INTERFACE NUMBER                         
         LA    R0,SPAVERR1                                                      
         B     ACNERRX                                                          
ACNERR2  LA    R1,LACNMSG2         BAD ACN NUMBER                               
         LA    R0,SPAVERR2                                                      
         B     ACNERRX                                                          
ACNERR3  LA    R1,LACNMSG3         ACC SYSTEM NOT OP                            
         LA    R0,SPAVERR3                                                      
         B     ACNERRX                                                          
ACNERR4  LA    R1,LACNMSG4         VEHICLE NOT VALID                            
         LA    R0,SPAVERR4                                                      
         B     ACNERRX                                                          
ACNERR5  LA    R1,LACNMSG5         NO ACN ON VEHICLE                            
         LA    R0,SPAVERR5                                                      
         B     ACNERRX                                                          
ACNERR6  LA    R1,LACNMSG6         INVALID AGENCY/ACN                           
         LA    R0,SPAVERR6                                                      
         B     ACNERRX                                                          
*                                                                               
ACNERRX  MVC   TWAMSG(13),=C'* ACN ERROR *'                                     
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TWAMSG+14(0),1(R1)  * EXECUTED *                                 
*                                                                               
         STC   R0,SPAVERR                                                       
         OI    SPAVERR,SPAVNOTV    INDICATE MESSAGE PRESENT                     
*                                                                               
         OC    VSWITCH,VSWITCH    TEST OFFLINE                                  
         BZ    EXIT                                                             
         GOTO1 VSWITCH,DMCB,=C'SPOT',0                                          
         B     EXIT                                                             
*                                                                               
LACNMSG1 DC    AL1(L'ACNMSG1)                                                   
ACNMSG1  DC    C'INVALID INTERFACE NUMBER'                                      
LACNMSG2 DC    AL1(L'ACNMSG2)                                                   
ACNMSG2  DC    C'INVALID ACN NUMBER'                                            
LACNMSG3 DC    AL1(L'ACNMSG3)                                                   
ACNMSG3  DC    C'ACC SYSTEM NOT OPERATIONAL'                                    
LACNMSG4 DC    AL1(L'ACNMSG4)                                                   
ACNMSG4  DC    C'VEHICLE NOT VALID ON 3M LEDGER'                                
LACNMSG5 DC    AL1(L'ACNMSG5)                                                   
ACNMSG5  DC    C'NO ACN ON VEHICLE (SE)'                                        
LACNMSG6 DC    AL1(L'ACNMSG6)                                                   
ACNMSG6  DC    C'INVALID ACN/AGENCY PAIR'                                       
         EJECT                                                                  
CCAGYLST DS    0CL7                AGYALPHA(2)/OFFICE(2)/CCAGY(3)               
         DC    CL7'APNY510'                                                     
         DC    CL7'WVME465'                                                     
         DC    CL7'BJMN725'                                                     
         DC    CL7'LIPA560'                                                     
         DC    CL7'RRSS770'                                                     
         DC    CL7'PMWA515'                                                     
         DC    CL7'MCNY910'                                                     
         DC    CL7'MCAT915'                                                     
         DC    CL7'MCDT915'                                                     
         DC    CL7'MCNO931'                                                     
         DC    CL7'MCHO933'                                                     
         DC    CL7'MCDA935'                                                     
         DC    CL7'MCBO955'                                                     
         DC    CL7'MCDE970'                                                     
         DC    CL7'MCCN977'                                                     
         DC    CL7'MCLO979'                                                     
         DC    CL7'MCCH981'                                                     
         DC    CL7'MCLA981'                                                     
         DC    CL7'MCSE981'                                                     
         DC    CL7'MCSF981'                                                     
         DC    CL7'MCSD984'                                                     
         DC    CL7'SJJR123'        **** FOR TESTING ONLY                        
         DC    X'FF'               E-O-L FLAG                                   
         EJECT                                                                  
CCUSAID  EQU   797                                                              
COKEAT   EQU   4401                                                             
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
VSWITCH  DS    V                                                                
VDATAMGR DS    V                                                                
*                                                                               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
DMCB     DS    6F                                                               
SPACES   DS    CL64                                                             
ELCODE   DS    X                                                                
*                                                                               
IOAREA   DS    2000X                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TWAD+10                                                          
TWAORIG  DS    XL2                                                              
         ORG   TWAD+64                                                          
TWAMSGH  DS    XL8                                                              
TWAMSG   EQU   *                                                                
         SPACE 2                                                                
SPAVBLKD DSECT                                                                  
       ++INCLUDE SPACNVALD                                                      
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPACNVALX 05/01/02'                                      
         END                                                                    
