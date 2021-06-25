*          DATA SET APGHFIDMA  AT LEVEL 168 AS OF 05/01/02                      
*PHASE ACHFDMAA                                                                 
         TITLE 'APG HOOK FOR DOREMUS PROFIT AND LOSS REPORT'                    
ACHFDMA  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         L     RF,=A(OFCTAB1)      ODD REPORT OFFICE TABLE                      
         A     RF,HKRELO                                                        
         ST    RF,AOFCTAB1                                                      
         L     RF,=A(OFCTAB2)      EVEN REPORT OFFICE TABLE                     
         A     RF,HKRELO                                                        
         ST    RF,AOFCTAB2                                                      
         L     RF,=A(COPYREC)      IO AREA FOR COPYING SORT RECORDS             
         A     RF,HKRELO                                                        
         ST    RF,ACOPYREC                                                      
         L     RF,=A(DUPREC)       IO AREA FOR COPYING SORT RECORDS             
         A     RF,HKRELO                                                        
         ST    RF,ADUPREC                                                       
         CLI   HOOKNUM,1           HOOKS BEFORE SORT                            
         BE    PUTSORT                                                          
         CLI   HOOKNUM,2           HOOKS AFTER SORT                             
         BE    BLDOFTAB                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ELIMINATE RECORDS BASED ON OFFICE GROUPING                   *         
*---------------------------------------------------------------------*         
PUTSORT  EQU   *                                                                
         LA    R3,GRPNTR                                                        
         CLC   QUNIT(2),=C'1C'                                                  
         BNE   *+10                                                             
         MVC   CUROFF,ACTACC+3                                                  
*                                                                               
         CLC   QUNIT(2),=C'SE'                                                  
         BNE   *+10                                                             
         MVC   CUROFF,ACTACC+9                                                  
*                                                                               
         CLC   QUNIT(2),=C'12'                                                  
         BNE   *+10                                                             
         MVC   CUROFF,CURRFLT      F1=OFFICE                                    
         BAS   RE,LIMITOF                                                       
         BNE   XIT_NO                                                           
*                                                                               
         CLC   QSELECT(2),=CL2' '                                               
         BE    XIT                                                              
*                                                                               
         TM    RPTOPT,RPTSUP                                                    
         BO    PUTS05                                                           
         TM    RPTOPT,RPTREG                                                    
         BO    PUTS10                                                           
*                                                                               
         OI    RPTOPT,RPTREG                                                    
         LA    R5,SUPERGR             DOES SELECT CODE INDICATE A               
PUTS03   CLI   0(R5),X'FF'            SUPERGROUP SELECTION                      
         BE    PUTS10                                                           
         CLC   QSELECT(2),0(R5)                                                 
         BE    PUTS04                                                           
         LA    R5,SUPERLN(R5)                                                   
         B     PUTS03                                                           
*                                                                               
PUTS04   NI    RPTOPT,X'FF'-RPTREG    TURN OFF REGULAR REPORT                   
         OI    RPTOPT,RPTSUP          TURN ON SUPERGROUP REPORT                 
         MVC   *+8(2),2(R5)           BASE AND DISP TO 'LA' INSTR.              
         LA    R4,0                   R4 NOW HAS SUPERGROUP LIST                
         ST    R4,ASUPLIST                                                      
         BAS   RE,SUPERTAB                                                      
*                                                                               
PUTS05   L     R5,HOOKAREC                                                      
         SR    R4,R4                                                            
         IC    R4,0(R5)                                                         
PUTS06   SH    R4,=H'2'                                                         
         LTR   R4,R4                                                            
         BZ    PUTS07                                                           
         BM    PUTS20                                                           
         BP    PUTS06                                                           
PUTS07   MVC   2(L'CUROFF,R5),CUROFF                                            
         B     PUTS20                                                           
*                                                                               
PUTS10   CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         CLC   QSELECT(2),0(R3)                                                 
         BE    PUTS20                                                           
         LA    R3,12(R3)           BUMP UP TO NEXT ENTRY                        
         B     PUTS10                                                           
*                                                                               
PUTS20   DS    0H                                                               
         L     R5,HOOKAREC                                                      
         LA    R7,1                ONLY GO THROUGH LOOP ONCE UNLESS             
         TM    RPTOPT,RPTSUP       IT IS SUPERLIST REQUEST                      
         BZ    PUTS40                                                           
         L     R4,ASUPLIST                                                      
         LH    R7,0(R4)            NUMBER OF PASSES TO MAKE                     
PUTS25   LA    R4,2(R4)            POINT TO FIRST GROUP ENTRY                   
         MVC   *+8(2),0(R4)        POINT TO GROUP TABLE AND GO TO               
         LA    R3,0                POINT TO OFFICE LIST                         
*                                                                               
PUTS40   L     R2,4(R3)            LOAD ADDRESS POINTING TO OFFICE LIST         
         A     R2,HKRELO                                                        
PUTS42   LA    R2,36(R2)           BUMP PAST NAME                               
PUTS45   CLI   0(R2),0             END OF LIST?                                 
         BE    PUTS50                                                           
         CLC   CUROFF,0(R2)                                                     
         BE    PUTS60              FOUND SO OK                                  
         LA    R2,2(R2)            BUMP PAST COMMA TO NEXT CHAR                 
         B     PUTS45                                                           
*                                                                               
PUTS50   DS    0H                                                               
         BCT   R7,PUTS25                                                        
         B     XIT_NO                                                           
*                                                                               
PUTS60   DS    0H                                                               
         TM    RPTOPT,RPTSUP                                                    
         BZ    PUTS61                                                           
         BAS   RE,PUTSUPS                                                       
         B     XIT_NO                                                           
*                                                                               
PUTS61   ICM   R2,15,8(R3)         GET RULE LIST                                
         BZ    XIT                 NONE PRESENT                                 
         A     R2,HKRELO                                                        
PUTS62   CLI   0(R2),0             END OF RULES?                                
         BE    XIT                                                              
         CLC   CUROFF,0(R2)        MATCH OFFICE                                 
         BE    PUTS65                                                           
         LA    R2,RULELNQ(R2)      BUMP UP                                      
         B     PUTS62                                                           
*                                                                               
PUTS65   ZAP   DDUB,SRAMT1                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT1,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT2                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT2,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT3                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT3,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT4                                                      
         MP    DDUB,4(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT4,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT5                                                      
         MP    DDUB,4(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT5,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT6                                                      
         MP    DDUB,7(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT6,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT7                                                      
         MP    DDUB,7(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT7,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT8                                                      
         MP    DDUB,10(3,R2)                                                    
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT8,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT9                                                      
         MP    DDUB,10(3,R2)                                                    
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT9,DDUB                                                      
*                                                                               
XIT      SR    RE,RE                                                            
XIT_NO   LTR   RE,RE                                                            
         XMOD1 1                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET OFFICE TABLE                                                       
*---------------------------------------------------------------------*         
BLDOFTAB DS    0H                                                               
         TM    RPTOPT,RPTSUP                                                    
         BZ    *+8                                                              
         BAS   RE,SETSTAK                                                       
*                                                                               
         CLI   ONEXONLY,NO                                                      
         BNE   GETOFFNM                                                         
         MVI   ONEXONLY,YES                                                     
         L     R3,=A(OFFTAB)       ADDRESS OF OFFICE TABLE                      
         A     R3,HKRELO                                                        
         ST    R3,OFFPT                                                         
         MVI   NOFF,0              NUMBER OF OFFICES                            
         MVI   CUROFF,0                                                         
*                                                                               
         USING ACKEYD,R2                                                        
         L     R2,=A(IO1)                                                       
         A     R2,HKRELO                                                        
         ST    R2,MYAIO1                                                        
         MVC   ACKEYD(42),SPACES                                                
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=CL2'2D'   GET 2D OFFICES                          
BLDOF12  MVI   ACKEYACC+4,X'FF'                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',MYAIO1,MYAIO1,0              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,MYAIO1                                                        
         CLC   ACKEYACC+1(2),=CL2'2D'                                           
         BNE   GETOFFNM            FINISHED                                     
         MVI   0(R3),0                                                          
         MVC   1(36,R3),SPACES                                                  
         MVC   CUROFF,ACKEYACC+3                                                
         MVC   0(1,R3),CUROFF                                                   
         AH    R2,DATADISP                                                      
*                                                                               
BLDOF15  CLI   0(R2),0                                                          
         BE    BLDOF90                                                          
         CLI   0(R2),X'20'                                                      
         BE    BLDOF20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     BLDOF15                                                          
*                                                                               
BLDOF20  CLI   NOFF,MAXOFF         END OF TABLE?                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NOFF             BUMP UP COUNT                                
         AH    R1,=H'01'                                                        
         STC   R1,NOFF                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'03'                                                        
         EXMVC R1,1(R3),2(R2)                                                   
BLDOF90  LA    R3,37(R3)           BUMP UP IN TABLE                             
         L     R2,MYAIO1                                                        
         B     BLDOF12                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                                                                               
*---------------------------------------------------------------------*         
GETOFFNM L     R3,OFFPT                                                         
         L     R5,HOOKAREC                                                      
         SR    R1,R1                                                            
         ICM   R1,1,NOFF                                                        
         BZ    XIT                                                              
         CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         L     R3,=A(OFFTAB)                                                    
         A     R3,HKRELO                                                        
GETOFF05 CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         LA    R3,37(R3)           BUMP UP IN TABLE                             
         BCT   R1,GETOFF05                                                      
         DC    H'0'                OFFICE NOT FOUND                             
*                                                                               
GETOFF10 MVC   SRNAM1,1(R3)        RESTORE 1C OFFICE NAME                       
         ST    R3,OFFPT                                                         
         B     XIT                                                              
LIMITOF  L     RF,ADOFFLST                                                      
         CLI   0(RF),0             END OF LIST                                  
         BER   RE                  NO OFFICE LIST                               
*                                                                               
         LA    R1,32                                                            
LIMITOF3 CLC   CUROFF,0(RF)                                                     
         BER   RE                                                               
         LA    RF,1(RF)                                                         
         CLI   0(RF),0                                                          
         BE    LIMITOF9                                                         
         BCT   R1,LIMITOF3                                                      
*                                                                               
LIMITOF9 LTR   RE,RE               XIT NO                                       
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET STACK FOR NUMBER OF REPORTS NEEDED                                 
*---------------------------------------------------------------------*         
SETSTAK  NTR1                                                                   
         LA    R3,SUPERGR                                                       
SETS10   CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QSELECT(2),0(R3)                                                 
         BE    SETS50                                                           
         LA    R3,SUPERLN(R3)                                                   
         B     SETS10                                                           
*                                                                               
SETS50   MVC   *+8(2),2(R3)           BASE AND DISP TO 'LA' INSTR.              
         LA    R4,0                   R4 NOW HAS SUPERGROUP LIST                
         ST    R4,ASUPLIST                                                      
         SR    R0,R0                                                            
         LH    R0,0(R4)               NUMBER OF OFFICES IN SUPERGROUP           
         ST    R0,REPSTKOF            STORE FOR MY PROGRAM                      
         MH    R0,=H'2'                                                         
         ST    R0,NRSTACK             STORE FOR CONTROLLER                      
*                                                                               
         L     R0,REPSTKOF                                                      
         L     R4,ARSTACK             ADDRESS OF REPORT STACK                   
         LA    R5,RPTLN               LENGTH OF STACK                           
         MH    R5,=H'2'                                                         
         AR    R4,R5                  NEXT AREA FOR RSTACK                      
         B     SETS62                                                           
SETS60   LA    R5,RPTLN               LENGTH OF STACK                           
         MH    R5,=H'2'                                                         
SETS62   LR    R3,R5                  REPEAT LENGTH FOR MOVE                    
         L     R2,ARSTACK                                                       
         MVCL  R4,R2                  REPEAT STACK                              
         BCT   R0,SETS60                                                        
         OI    RQSW,RQSTK             TELL CONTROLLER TO LEAVE STACK            
*                                                                               
SETSXIT  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET TABLE OF OFFICES WITH GROUP CODES                                  
*---------------------------------------------------------------------*         
SUPERTAB NTR1                                                                   
         MVI   BYTE,1              REPORT COUNTER                               
*                                                                               
         USING OFTABD,R3                                                        
         L     R3,AOFCTAB1         OFFICE REPORT TABLE                          
         L     R4,ASUPLIST         ADDRESS OF THIS SUPERLIST SELECTION          
         LA    R4,2(R4)            POINT PAST NUMBER OF GROUPS                  
*                                                                               
SUPT100  DS    0H                                                               
         LA    R0,2                DUPLICATE THIS OFFICE LIST GROUP             
SUPT110  MVC   *+8(2),0(R4)        IN OFFICE TABLE                              
         LA    R7,0                R2 NOW HAS GROUP LIST                        
         MVC   GRPTEMP,0(R7)                                                    
         MVC   ARULE,8(R7)                                                      
         L     R2,4(R7)            R2 NOW HAS GROUP LIST                        
         A     R2,HKRELO                                                        
         MVC   GPNAM,0(R2)         OFFICE GROUP NAME                            
         LA    R2,36(R2)           POINT PAST NAME                              
SUPT115  CLI   0(R2),0             END OF THIS OFFICE LIST                      
         BE    SUPT150                                                          
         MVC   OFRPTNM,BYTE        THIS REPORT NUMBER                           
         MVC   OFNUM,0(R2)         THIS OFFICE                                  
         MVC   OFGRPNAM,GPNAM      OFFICE GROUP NAME                            
         MVC   OFGRULE,ARULE                                                    
         MVC   OFGRPCOD,GRPTEMP                                                 
         LA    R3,OFENTRY(R3)                                                   
         MVI   0(R3),X'FF'         MARK NEW END OF TABLE                        
         LA    R2,2(R2)                                                         
         B     SUPT115                                                          
*                                                                               
SUPT150  DS    0H                                                               
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         BCT   R0,SUPT110          REPEAT THIS TABLE FOR REPORT TWO             
*                                                                               
         XC    ARULE,ARULE                                                      
         LA    R4,2(R4)                                                         
         CLI   0(R4),X'FF'         END OF SUPER GROUP LIST                      
         BNE   SUPT100                                                          
*                                                                               
SUPTXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PUT OUT THIS RECORD FOR EACH OFFICE GROUP IT IS IN                     
*---------------------------------------------------------------------*         
PUTSUPS  NTR1                                                                   
         BAS   RE,ODDEVEN                                                       
         L     R5,HOOKAREC                                                      
         USING OFTABD,R3                                                        
         L     R3,AOFCTAB1         REPEAT REPORT 1 FROM THIS TABLE              
PSUP100  CLC   CUROFF,OFNUM                                                     
         BNE   PSUP500                                                          
*                                                                               
         SR    R4,R4                                                            
         IC    R4,0(R3)                                                         
PSUP120  SH    R4,=H'2'                                                         
         LTR   R4,R4                                                            
         BZ    PSUP150                                                          
         BM    PSUP160                                                          
         BP    PSUP120                                                          
PSUP150  CLI   ODEVSW,EVEN                                                      
         BNE   PSUP500                                                          
         B     PSUP200                                                          
PSUP160  CLI   ODEVSW,ODD                                                       
         BNE   PSUP500                                                          
         B     PSUP200                                                          
*                                                                               
PSUP200  MVC   BYTE,OFRPTNM                                                     
         ST    R3,REGTEMP                                                       
         BAS   RE,COPYPUT                                                       
PSUP500  LA    R3,OFENTRY(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   PSUP100                                                          
PSUPXIT  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        COPY SORTREC, ADJUST REPORT NUMBER AND PUT TO SORTER                   
*---------------------------------------------------------------------*         
COPYPUT  NTR1                                                                   
         L     R2,HOOKAREC                                                      
         L     R4,ACOPYREC                                                      
         LA    R3,SRLNQ                                                         
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
*                                                                               
         L     R3,REGTEMP                                                       
         OC    OFGRULE,OFGRULE                                                  
         BZ    COP090                                                           
         SR    R2,R2                                                            
         ICM   R2,15,OFGRULE                                                    
         A     R2,HKRELO                                                        
COP010   CLI   0(R2),0             END OF TABLE                                 
         BE    COP090                                                           
         CLC   CUROFF,0(R2)                                                     
         BE    COP020                                                           
         LA    R2,RULELNQ(R2)                                                   
         B     COP010                                                           
*                                                                               
COP020   DS    0H                                                               
         USING SRECD,R5                                                         
         L     R5,ACOPYREC                                                      
         ZAP   DDUB,SRAMT1                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT1,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT2                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT2,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT3                                                      
         MP    DDUB,1(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT3,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT4                                                      
         MP    DDUB,4(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT4,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT5                                                      
         MP    DDUB,4(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT5,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT6                                                      
         MP    DDUB,7(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT6,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT7                                                      
         MP    DDUB,7(3,R2)                                                     
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT7,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT8                                                      
         MP    DDUB,10(3,R2)                                                    
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT8,DDUB                                                      
*                                                                               
         ZAP   DDUB,SRAMT9                                                      
         MP    DDUB,10(3,R2)                                                    
         SRP   DDUB,61,5                                                        
         ZAP   SRAMT9,DDUB                                                      
*                                                                               
         USING SRECD,R5                                                         
COP090   L     R5,ACOPYREC                                                      
         LA    RF,4                                                             
         LA    R1,SRROW1                                                        
COP100   CLI   0(R1),0                                                          
         BE    *+10                                                             
         MVC   0(1,R1),BYTE          SET FOR REPORT 2                           
         LA    R1,16(R1)                                                        
         BCT   RF,COP100                                                        
*                                                                               
         OI    SRTSW,SRTAC                                                      
         L     R4,ACOPYREC                                                      
         GOTO1 MERGER,DMCB,=C'PUT',(R4),ADSORTER                                
         BAS   RE,SRTRACE                                                       
COPYXIT  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        TRACE PUTS TO SORT                                                     
*---------------------------------------------------------------------*         
SRTRACE  NTR1                                                                   
         TM    UPSI,TRCMRGE        TRACE PUTS TO MERGE                          
         BNO   SRTXIT                                                           
         CP    TRCMCNT,=PL2'300'   ALREADY PRINTED  THE MAX                     
         BH    SRTXIT                                                           
         USING BOXD,RF                                                          
         L     RF,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   RCSUBPRG,2          TURN OFF BOXES                               
         MVC   P+1(9),=C'MODE=SORT'                                             
         MVC   P+86(7),=C'OFFICE='                                              
         MVC   P+96(1),CUROFF                                                   
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         LA    R0,SRLNQ                                                         
         L     R4,ACOPYREC                                                      
         GOTO1 PRNTBL,DMCB,0,(R4),C'DUMP',(R0),=C'2D',(C'P',PRINT)              
         AP    TRCMCNT,=P'1'                                                    
         L     RF,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'198'                                                 
         MVI   RCSUBPRG,0                                                       
SRTXIT   B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        COPY HOOK SORTREC TO MY IO AREA                                        
*---------------------------------------------------------------------*         
DUPLICAT NTR1                                                                   
         L     R2,HOOKAREC                                                      
         L     R4,ADUPREC                                                       
         LA    R3,SRLNQ                                                         
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
         B     XIT                                                              
*---------------------------------------------------------------------*         
*        IS THIS REPORT ODD OR EVEN                                             
*---------------------------------------------------------------------*         
ODDEVEN  NTR1                                                                   
         MVI   ODEVSW,ODD                                                       
         L     R5,HOOKAREC                                                      
         CLI   0(R5),1                                                          
         BE    XIT                                                              
         MVI   ODEVSW,EVEN                                                      
         B     XIT                                                              
*---------------------------------------------------------------------*         
*                                                                               
*---------------------------------------------------------------------*         
ONEXONLY DC    AL1(NO)                                                          
NOFF     DS    CL1                                                              
MAXOFF   EQU   36                  MAX NUMBER OF OFFICES IN TABLE               
CUROFF   DS    CL1                 CURRENT OFFICE                               
HKRELO   DS    A                                                                
OFFPT    DS    A                                                                
MYAIO1   DS    A                                                                
AOFCTAB1 DS    A                   REPORT ONE OFFICE TABLE                      
AOFCTAB2 DS    A                   REPORT TWO OFFICE TABLE                      
ACOPYREC DS    A                   IOAREA FOR COPYING SORT RECORDS              
ADUPREC  DS    A                   IOAREA FOR COPYING SORT RECORDS              
REPSTKOF DS    A                   NUMBER OF STACK ITEMS                        
ASUPLIST DS    A                   ADDRESS OF REQUESTED SUPER LIST              
ARULE    DS    A                   ADDRESS OF RULE LIST                         
REGTEMP  DS    F                   TEMPORARY REGISTER AREA                      
GRPTEMP  DS    CL2                 THIS GROUP CODE                              
GPNAM    DS    CL36                THIS GROUP NAME                              
DDUB     DS    PL16                                                             
RPTOPT   DS    CL1                 REPORT OPTION INDICATOR                      
RPTSUP   EQU   X'80'               IS THIS SUPER LIST REQUEST                   
RPTREG   EQU   X'40'               IS THIS SUPER LIST REQUEST                   
ODEVSW   DS    CL1                 IS THIS REPORT 1 OR 2                        
ODD      EQU   X'01'               ODD REPORT                                   
EVEN     EQU   X'02'               EVEN REPORT                                  
TRCMCNT  DC    PL2'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SUPERLN  EQU   4                   LENGTH OF ONE TABLE ENTRY                    
SUPERGR  DS    0A                  SUPER GROUP CODE POINTERS                    
         DC    C'AX',S(SUPERAX)                                                 
         DC    C'BX',S(SUPERBX)                                                 
         DC    C'CX',S(SUPERCX)                                                 
         DC    C'DX',S(SUPERDX)                                                 
         DC    X'FF'                                                            
*                                                                               
SUPERLST DS    0A                  SUPER OFFICE LIST CODES                      
SUPERAX  DC    AL2(1),S(SGDC),X'FF'                                             
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERBX  DC    AL2(1),S(SGLC),X'FF'                                             
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERCX  DC    AL2(4),S(SGFA),S(SGCS),S(SGDO),S(SGDD),X'FF'                     
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERDX  DC    AL2(4),S(SGFA),S(SGCS),S(SGDO),S(SGDD),X'FF'                     
*                                                                               
*                                                                               
GRPNTR   DS    0A                  GROUP CODE POINTERS                          
SGCS     DC    C'CS',A(GRPCS),A(0)                                              
SGDC     DC    C'DC',A(GRPDC),A(0)                                              
SGDD     DC    C'DD',A(GRPDD),A(0)                                              
SGDO     DC    C'DO',A(GRPDO),A(RULEDO)                                         
SGFA     DC    C'FA',A(GRPFA),A(RULEFA)                                         
SGGE     DC    C'GE',A(GRPGE),A(0)                                              
SGIN     DC    C'IN',A(GRPIN),A(0)                                              
SGLC     DC    C'LC',A(GRPLC),A(RULELC)                                         
         DC    X'FF'                                                            
*                                                                               
*              GROUP NAMES / OFFICE LIST                                        
*                                                                               
GRPCS    DC    CL36'OVERHEAD OFFICES'                                           
         DC    C'A,I,K,M,Z',X'0000'                                             
*                                                                               
GRPDC    DC    CL36'DOMESTIC, NO FINANCIAL PRINTING'                            
         DC    C'A,I,J,K,M,N,R,T,U,W,Z,1,3,5,6,9',X'0000'                       
*                                                                               
GRPDD    DC    CL36'DOMESTIC OFFICES'                                           
         DC    C'A,I,J,K,M,N,O,R,T,U,W,Z,1,3,5,6,9',X'0000'                     
*                                                                               
GRPDO    DC    CL36'REGIONAL OFFICES'                                           
         DC    C'J,R,3,6',X'0000'                                               
*                                                                               
GRPFA    DC    CL36'FINANCIAL OFFICES'                                          
         DC    C'O,T,U,1,5',X'0000'                                             
*                                                                               
GRPGE    DC    CL36'DOREMUS GENERAL'                                            
         DC    C'N,W,9',X'0000'                                                 
*                                                                               
GRPIN    DC    CL36'INTERNATIONAL OFFICES'                                      
         DC    C'V,X,8',X'0000'                                                 
*                                                                               
GRPLC    DC    CL36'MERKLEY OFFICES'                                            
         DC    C'B,C,D,E,F,G,H,P,Q,S,Y,2',X'0000'                               
*                                                                               
*-------------------------------------------------------------------*           
*        CL1 = OFFICE                                                           
*        PL3 = COLUMNS 1-3  TO ONE DECIMAL XXX.X                                
*        PL3 = COLUMNS 4,5                                                      
*        PL3 = COLUMNS 6,7                                                      
*        PL3 = COLUMNS 8,9                                                      
*-------------------------------------------------------------------*           
RULEDO   DC    C'U',PL3'0',PL3'20',PL3'0',PL3'0'                                
RULELNQ  EQU   *-RULEDO                                                         
         DC    AL1(0)                                                           
*                                                                               
RULEFA   DC    C'U',PL3'1000',PL3'850',PL3'1000',PL3'1000'                      
         DC    C'Q',PL3'0',PL3'250',PL3'0',PL3'0'                               
         DC    AL1(0)                                                           
*                                                                               
RULELC   DC    C'U',PL3'0',PL3'130',PL3'0',PL3'0'                               
         DC    C'Q',PL3'1000',PL3'750',PL3'1000',PL3'1000'                      
         DC    AL1(0)                                                           
*                                                                               
         EJECT                                                                  
*----------------------------------*                                            
*        OFFICE CODE(1)            *                                            
*        OFFICE NAME(36)           *                                            
*----------------------------------*                                            
OFFTAB   DS    (MAXOFF)CL37                                                     
IO1      DS    CL2001                                                           
*                                                                               
OFCTAB1  DS    200CL(OFENTRY)     REPORT ONE OFFICE TABLE                       
         DS    CL1                END OF TABLE                                  
OFCTAB2  DS    200CL(OFENTRY)     REPORT TWO OFFICE TABLE                       
         DS    CL1                END OF TABLE                                  
         DS    0A                                                               
COPYREC  DS    CL(SRLNQ)                                                        
         DS    0A                                                               
DUPREC   DS    CL(SRLNQ)                                                        
*                                                                               
OFTABD   DSECT                    COVER REPORT OFFICE TABLES                    
OFRPTNM  DS    CL1                                                              
OFGRPCOD DS    CL2                                                              
OFNUM    DS    CL1                                                              
OFGRPNAM DS    CL36                                                             
OFGRULE  DS    CL4                                                              
OFENTRY  EQU   *-OFTABD                                                         
*                                                                               
         EJECT                                                                  
*--------------------------*                                                    
*        SORT RECORD       *                                                    
*--------------------------*                                                    
SRECD    DSECT                                                                  
SRREC    DS    0C                                                               
SRROW1   DS    CL2                 REPORT NUMBER/COPY                           
SRACC1   DS    CL1                 ROW 1 OFFICE CODE                            
         DS    CL13                                                             
SRROW2   DS    CL2                 REPORT NUMBER/COPY                           
SRACC2   DS    CL1                 ACCOUNT LEVEL 1 (SUPERLEDGER)                
         DS    CL13                                                             
SRROW3   DS    CL2                 REPORT NUMBER/COPY                           
SRACC3   DS    CL2                 ACCOUNT LEVEL 2 (SUPERLEDGER)                
         DS    CL12                                                             
SRROW4   DS    CL2                 REPORT NUMBER/COPY                           
SRACC4   DS    CL3                 ACCOUNT LEVEL 3 (SUPERLEDGER)                
         DS    CL11                                                             
         DS    CL2                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE NAME                                  
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRAMT1   DS    PL8                 BUCKETS                                      
SRAMT2   DS    PL8                 BUCKETS                                      
SRAMT3   DS    PL8                 BUCKETS                                      
SRAMT4   DS    PL8                 BUCKETS PRIOR                                
SRAMT5   DS    PL8                 BUCKETS PRIOR                                
SRAMT6   DS    PL8                 BUCKETS                                      
SRAMT7   DS    PL8                 BUCKETS                                      
SRAMT8   DS    PL8                 BUCKETS                                      
SRAMT9   DS    PL8                 BUCKETS                                      
SRLNQ    EQU   *-SRREC                                                          
         EJECT                                                                  
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'168APGHFIDMA 05/01/02'                                      
         END                                                                    
