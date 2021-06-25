*          DATA SET RECNT74    AT LEVEL 207 AS OF 04/19/13                      
*PHASE T80274A +0                                                               
         TITLE 'T80274 - SPL COPY BUYACT'                                       
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT74 (T80274) --- SPL COPY BUY ACTION                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY                             *             
*                                                                 *             
* 19APR13 KUI FIX TRASHING AUTO-CLOSE DATE IN X'08' ELEMENT       *             
* 22SEP98 AST ORIGINAL CONCEPTION                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80274   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80274,R9,R8                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LA    R2,CONBACTH                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
*                                                                               
         LA    RE,TABKEY                                                        
         LA    RF,TABEQ                                                         
         XCEF                                                                   
         BAS   RE,VALCON           VALIDATE CONTRACTS INPUT                     
*                                                                               
         LA    R3,INVINP           INVALID INPUT                                
         LA    R2,SCOOKH           PNT TO 'OK' FLD                              
         CLI   8(R2),C'Y'          OK?                                          
         BNE   ERROR                                                            
*                                                                               
         BAS   RE,COPYSPL          COPY SPL DATA                                
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
******************                                                              
NXTFLD   DS    0H                                                               
         ZIC   R1,0(R2)            GET LENGTH OF CURRENT FLD                    
         AR    R2,R1               ADVANCE TO NXT FLD                           
         BR    RE                  RETURN TO PRG/RTN                            
******************                                                              
*                                                                               
********************************************************************            
*                                                                               
********************************************************************            
VALCON   NTR1                                                                   
         LA    R2,SCOCPO1H         POINT TO FIRST CONTRACT ENTRY                
         LA    R5,TABKEY           POINT TO TABLE OF KEYS                       
*                                                                               
VC10     DS    0H                                                               
         CLI   5(R2),0             NO INPUT?                                    
         BNE   VC15                INPUT, GO ON                                 
         BAS   RE,NXTFLD                                                        
         LA    R3,INVINP           INVALID INPUT                                
         CLI   5(R2),0             INPUT IN 'COMMENTS ONLY' ?                   
         BNE   ERROR               YES, SHOULD BE BLANK                         
         B     VCNXT               GOTO NEXT CONTRACT                           
*                                                                               
VC15     DS    0H                                                               
         LA    R3,INVINP           INVALID INPUT                                
         TM    4(R2),X'08'         VALID NUMERIC INPUT?                         
         BZ    ERROR                                                            
*                                                                               
         BAS   RE,FNDCON           FIND CONTRACT                                
*                                                                               
         BAS   RE,NXTFLD           POINT TO 'COMMENTS ONLY'                     
*                                                                               
         L     R1,AIO2                                                          
         LA    R3,INVCMNT          MUST BE CMNT ONLY                            
         CLC   6(5,R1),RCONKSTA    SAME STATIONS?                               
         BNE   VC22                NOT SAME STATION, MUST BE CMNT ONLY          
*                                                                               
* DON'T COPY TO TARGET K IF IT ALREADY HAS SPL DATA                             
*                                                                               
         LA    R3,NOCOPY           NO COPY IF TRGT HAS SPL DATA                 
         L     R6,AIO2                                                          
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BE    VC22                MUST BE CMNT ONLY                            
*                                                                               
VC20     DS    0H                  'COMMENTS ONLY' FIELD                        
         LA    R3,INVINP           INVALID INPUT                                
         CLI   5(R2),0             BLANK?                                       
         BE    VCNXT               BLANK, CHECK NEXT K                          
         CLI   8(R2),C'N'                                                       
         BE    VCNXT                                                            
VC22     CLI   8(R2),C'Y'          COMMENT ONLY?                                
         BNE   ERROR               NO, ERROR                                    
*                                                                               
VCNXT    DS    0H                  CHECK END CONDITION                          
* INSERT Y/N/' ' INTO TABLE                                                     
         MVC   27(1,R5),8(R2)      STORE CMNT FLAG IN TABLE                     
*                                                                               
         LA    R1,SCOCPCLH         ARE WE AT/PAST LAST 'CMT ONLY' FLD?          
         CR    R2,R1                                                            
         BNL   VCX                 YES, DONE                                    
         BAS   RE,NXTFLD           BUMP TO NEXT CONTRACT                        
         LA    R5,TABENEQ(R5)      NEXT TABLE ENTRY                             
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    VCX                 YES, DONE                                    
         B     VC10                                                             
VCX      B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
********************************************************************            
*                                                                               
*                                                                               
********************************************************************            
FNDCON   NTR1                                                                   
         XC    TEMPCON,TEMPCON                                                  
         XC    CONNIN,CONNIN                                                    
         LA    R3,CONERR           CONTRACT NOT FOUND ERR MSG                   
*                                                                               
         XC    MYKEY,MYKEY                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),RCONKREP                                               
*                                                                               
* RIGHT-ALIGN K#                                                                
         LA    R0,8                LENGTH OF TEMPCON                            
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         SR    R0,R1               POSITION OF K# IN TEMPCON                    
         LA    RE,TEMPCON          POINT TO APPROPRIATE BYTE                    
         AR    RE,R0                                                            
*                                                                               
         BCTR  R1,0                EX MOVE                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       PUT IN TEMPCON, THEN KEY                     
*                                                                               
         GOTO1 HEXIN,DMCB,TEMPCON,CONNIN,8                                      
         L     R1,=X'99999999'     GET 9'S COMPLEMENT                           
         L     R0,CONNIN                                                        
         SR    R1,R0                                                            
         ST    R1,CONNIN                                                        
         MVC   KEY+23(4),CONNIN                                                 
*                                                                               
         MVC   MYKEY,KEY                                                        
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),MYKEY                                                    
         BNE   ERROR                                                            
*                                                                               
* INSERT KEY INTO TABLE                                                         
         MVC   0(27,R5),KEY        STORE KEY, OLD CODE                          
         GOTO1 VGETREC,DMCB,AIO2   DON'T CLOBBER CONTRACT, NEW CODE             
*                                                                               
FCX      DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
********************************************************************            
* COPY SPL FROM DISPLAYED CONTRACT (RCONREC) TO TARGET K (AIO2)                 
********************************************************************            
COPYSPL  NTR1                                                                   
         LA    R5,TABKEY           POINT TO TABLE OF KEYS                       
*                                                                               
CS10     DS    0H                                                               
         OC    0(27,R5),0(R5)      NO INPUT?                                    
         BZ    CS20                NO INPUT, GO ON                              
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R5)       COPY KEY                                     
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'         READ FOR UPDATE                              
         GOTO1 VGETREC,DMCB,AIO2   DON'T CLOBBER DISPLAYED                      
*                                                                               
         CLI   27(R5),C'Y'         COMMENTS ONLY?                               
         BE    CS14                YES, DON'T COPY '06' ELEM                    
*                                                                               
***   FOR '06' SPL ELEM                                                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(6,AIO2)                                           
         LA    R6,RCONREC          POINT TO OLD CONTRACT                        
         MVI   ELCODE,6            SPL ELEM                                     
         BAS   RE,GETEL                                                         
         GOTO1 VADDELEM,DMCB,AIO2,0(R6)                                         
*                                                                               
***   FOR '08' SPL TRUE ACTIVITY DATE ELEM                                      
*                                                                               
CS14     DS    0H                                                               
         CLI   27(R5),C'Y'         COMMENTS ONLY?                               
         BNE   CS14A               NO, COPY '08' AS IS                          
         L     R6,AIO2             NEW CONTRACT                                 
         MVI   ELCODE,6            NEW K ALREADY HAS SPL?                       
         BAS   RE,GETEL                                                         
         BNE   CS14A               NO GO ON AS NORMAL                           
         L     R6,AIO2             NEW CONTRACT                                 
         MVI   ELCODE,8            JUST CHANGE TRUE DATE TO TODAY               
         USING RCONACEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   CS15                                                             
         MVC   RCONACTA,TODAY      COPY TODAY'S DATE                            
         B     CS15                COPY COMMENTS                                
         DROP  R6                                                               
*                                                                               
CS14A    GOTO1 VDELELEM,DMCB,(8,AIO2)                                           
         LA    R6,RCONREC          POINT TO OLD CONTRACT                        
         MVI   ELCODE,8            TRUE DATE ELEM                               
         BAS   RE,GETEL                                                         
         XC    WORK,WORK                                                        
         ZIC   R1,1(R6)            GET ELEM LENGTH                              
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)       COPY ELEMENT '08'                            
*                                                                               
         CLI   27(R5),C'Y'         COMMENTS ONLY?                               
         BNE   CS14B               NO, COPY '08' AS IS                          
         XC    WORK+2(3),WORK+2    CLEAR EVERYTHING BUT TRUE DATE               
         XC    WORK+8(4),WORK+8                                                 
*                                                                               
CS14B    MVC   WORK+5(3),TODAY     COPY TODAY'S DATE                            
         GOTO1 VADDELEM,DMCB,AIO2,WORK                                          
*                                                                               
*** DELETE OLD '07' ELEMS AND COPY NEW ONES                                     
*                                                                               
CS15     DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(7,AIO2)                                           
         LA    R6,RCONREC          POINT TO NEW CONTRACT                        
         MVI   ELCODE,7            SPL CMNT                                     
         BAS   RE,GETEL                                                         
         B     CS17                                                             
CS16     BAS   RE,NEXTEL                                                        
CS17     BNE   CS19                                                             
         GOTO1 VADDELEM,DMCB,AIO2,0(R6)                                         
         B     CS16                                                             
*                                                                               
CS19     DS    0H                                                               
         GOTO1 VPUTREC,DMCB,AIO2                                                
*                                                                               
CS20     LA    R5,TABENEQ(R5)      NEXT TABLE ENTRY                             
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    CSX                 YES, DONE                                    
         B     CS10                                                             
CSX      DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
********************************************************************            
*                                                                               
TABKEY   DS    0H                                                               
TKEY1    DS    CL27                CONTRACT KEY                                 
TCMNT    DS    CL1                 COMMENT ONLY?                                
TABENEQ  EQU   *-TKEY1                                                          
*                                                                               
         DS    29CL28              OTHER ENTRIES                                
TABEQ    EQU   *-TKEY1                                                          
         DC    X'FF'               END OF TABLE                                 
*                                                                               
MYKEY    DS    CL27                CONTRACT KEY                                 
TEMPCON  DS    CL8                 RIGHT ALIGNED EBCDIC K#                      
CONNIN   DS    F                   FOUR BYTE 9'S COMPL K #                      
*                                                                               
INVCMNT  EQU   821                 MUST BE CMNT ONLY FOR DIFF STA'S             
NOCOPY   EQU   822                 NO COPY IF TRGT HAS SPL DATA                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTDBD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'207RECNT74   04/19/13'                                      
         END                                                                    
