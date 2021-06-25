*          DATA SET DETMPRD    AT LEVEL 079 AS OF 03/23/15                      
*PHASE DETMPRDA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
DETMPRD  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,DETMPRD,=V(REGSAVE),R6                                       
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RE,=A(PROGBUFF)                                                  
         L     RF,=F'65000'                                                     
         XCEF                                                                   
         L     RE,=A(PROGBUF2)                                                  
         L     RF,=F'100000'                                                    
         XCEF                                                                   
         L     RE,=A(SUMTABS)                                                   
         LA    RF,SUMTABE-SUMTABS                                               
         XCEF                                                                   
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         OPEN  (IN,(INPUT))                                                     
         MVI   GETSW,1                                                          
         OPEN  (OUT,(OUTPUT))                                                   
         LA    RE,OUTDATA                                                       
         XCEF  (RE),1000                                                        
         LA    RE,SVOUTREC                                                      
         XCEF  (RE),1004                                                        
         GET   IN,INREC                                                         
*                                                                               
GET      MVI   CLEARS,C' '                                                      
         MVC   CLEARS+1(CLEARE-CLEARS-1),CLEARS                                 
         GET   IN,INREC                                                         
         B     GETX                                                             
GETX     DS    0C                                                               
         MVC   P(255),INREC                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
OUT10    DS    0C                                                               
*                                                                               
         LA    R4,INREC                                                         
         USING INRECD,R4                                                        
         LA    R5,OUTDATA                                                       
         USING OUTRECD,R5                                                       
         MVC   OUTMKTN,INMKTN                                                   
         MVC   OUTCALL,INCALL                                                   
         MVC   OUTTITLE,INTITLE                                                 
*                                                                               
         CLC   INCALL,=X'404040404040'                                          
         BE    GET                                                              
*  START DAY                                                                    
         LA    RE,DAYTAB                                                        
OUT20    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INSDAY,0(RE)                                                     
         BE    OUT30                                                            
         LA    RE,L'DAYTAB(RE)                                                  
         B     OUT20                                                            
OUT30    DS    0H                                                               
         MVC   OUTSDAY,2(RE)                                                    
*                                                                               
*  END DAY                                                                      
         LA    RE,DAYTAB                                                        
         CLC   =X'4040',INEDAY    ANY END DAY?                                  
         BE    OUT60                                                            
         CLC   =C'FR',INEDAY      IF END DAY = FRIDAY THEN MUST BE M-F          
         BNE   OUT40                                                            
         MVI   OUTSDAY,X'95'                                                    
         B     OUT60                                                            
OUT40    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INEDAY,0(RE)                                                     
         BE    OUT50                                                            
         LA    RE,L'DAYTAB(RE)                                                  
         B     OUT40                                                            
OUT50    DS    0H                                                               
         MVC   OUTEDAY,2(RE)                                                    
         CLC   =C'FR',INEDAY      IF END DAY = FRIDAY THEN MUST BE M-F          
         BNE   OUT60                                                            
         MVI   OUTSDAY,X'95'                                                    
OUT60    DS    0H                                                               
         CLC   =C'MDNT',INSTIME                                                 
         BNE   OUT70                                                            
         MVC   OUTSTIME,=X'2400'                                                
         B     OUT90                                                            
OUT70    CLC   =C'NOON',INSTIME                                                 
         BNE   OUT80                                                            
         MVC   OUTSTIME,=X'1200'                                                
         B     OUT90                                                            
* START TIME TO MILITARY                                                        
OUT80    ZICM  RE,INSTIME,4                                                     
         ZICM  RF,=C'0000',4                                                    
         OR    RE,RF                                                            
         ST    RE,FULL                                                          
         PACK  DUB(8),FULL(4)                                                   
         CVB   RE,DUB                                                           
         CLC   =C'MDNT',INETIME   IF ENDTIME IS MIDNIGHT THEN STARTTIME         
         BE    OUT83              IS PM                                         
         CLC   =C'PM',INAMPM                                                    
         BNE   OUT85                                                            
OUT83    LA    RF,1200                                                          
         AR    RE,RF                                                            
OUT85    STCM  RE,3,OUTSTIME                                                    
*                                                                               
OUT90    DS    0H                                                               
         CLC   =C'MDNT',INETIME                                                 
         BNE   OUT100                                                           
         MVC   OUTETIME,=X'2400'                                                
         B     OUT130                                                           
OUT100   CLC   =C'NOON',INETIME                                                 
         BNE   OUT110                                                           
         MVC   OUTETIME,=X'1200'                                                
         B     OUT130                                                           
* END TIME TO MILITARY                                                          
OUT110   ZICM  RE,INETIME,4                                                     
         ZICM  RF,=C'0000',4                                                    
         OR    RE,RF                                                            
         ST    RE,FULL                                                          
         PACK  DUB(8),FULL(4)                                                   
         CVB   RE,DUB                                                           
         CLC   =C'PM',INAMPM                                                    
         BNE   OUT120                                                           
         LA    RF,1200                                                          
         AR    RE,RF                                                            
OUT120   STCM  RE,3,OUTETIME                                                    
*  END TIME MILITARY                                                            
OUT130   DS    0H                                                               
         LA    RE,INWEEKS                                                       
         LA    RF,OUTWEEKS                                                      
         LA    R0,4                                                             
OUT140   CLI   0(RE),X'40'                                                      
         BE    *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,OUT140                                                        
*                                                                               
         CLI   INRECTY,C'2'   RECORD TYPE 2 ENDS AT BYTE 176                    
         BNE   *+12                                                             
         LA    R9,RECTYP2                                                       
         B     OUT150                                                           
         CLI   INRECTY,C'3'                                                     
         BNE   *+12                                                             
         LA    R9,RECTYP3                                                       
         B     OUT150                                                           
         CLI   INRECTY,C'4'                                                     
         BNE   *+12                                                             
         LA    R9,RECTYP4                                                       
         B     OUT150                                                           
         CLI   INRECTY,C'7'                                                     
         BNE   ENDJOB                                                           
         LA    R9,RECTYP7                                                       
OUT150   LA    R2,INDEMOS                                                       
         LA    R8,OUTDEMOS                                                      
*                                                                               
OUT160   CLC   =X'00000000',0(R9)  END OF THE DISPLACEMENT TABLE                
         BE    OUT180                                                           
         L     RE,0(R9)                                                         
         LA    RF,WORK                                                          
         XC    WORK,WORK                                                        
         LR    R0,R2                SAVE REGISTER 2                             
OUT163   CLI   0(R2),X'40'                                                      
         BE    *+10                                                             
         MVC   0(1,RF),0(R2)                                                    
         LA    RF,1(RF)                                                         
         LA    R2,1(R2)                                                         
         BCT   RE,OUT163                                                        
         LR    R2,R0                                                            
*                                                                               
         L     RE,0(R9)                                                         
         BCTR  RE,0                                                             
         LA    RF,WORK                                                          
         AR    RE,RF                                                            
         CLI   0(RE),C'0'                                                       
         BL    OUT165         NON NUMERIC BECOMES ZEROES                        
         CLI   0(RE),C'9'                                                       
         BH    OUT165                                                           
*                                                                               
         L     RE,0(R9)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),WORK(0)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R8)                                                      
*        DC    H'0'                                                             
OUT165   LA    R8,4(R8)        NEXT DEMO SLOT                                   
*        ZIC   RE,WORK                                                          
         L     RE,0(R9)                                                         
         AR    R2,RE                                                            
         ZIC   RE,4(R9)                                                         
         AR    R2,RE                                                            
         LA    R9,5(R9)                                                         
         B     OUT160                                                           
*                                                                               
*                                                                               
OUT180   LA    R1,INREC       LENGTH OF INPUT REC - DEMOS                       
         SR    R2,R1                                                            
         STCM  R2,3,SVOUTLEN                                                    
*                                                                               
         LA    RE,OUTDEMOS                                                      
         LR    RF,R8                                                            
         SR    RF,RE          LENGTH OF DEMOS                                   
         LA    RE,OUTHDLEN                                                      
         AR    RE,RF          LENGTH OF RECORD                                  
         LA    RE,4(RE)                                                         
         STCM  RE,3,OUTHEAD                                                     
*  FEED RECORDS TO SORTER                                                       
         LA    RE,SVSORTK                                                       
         USING SORTKEY,RE                                                       
         OC    SORTMKTN,SORTMKTN    COMPARE FOR THE CHANGE OF MARKET            
         BZ    *+14                 IF MKT CHANGE STOP PUTTING IN SORT          
         CLC   OUTMKTN,SORTMKTN                                                 
         BNE   ENDJOB                                                           
         MVC   SORTMKTN,OUTMKTN                                                 
         MVC   SORTSTAT,OUTCALL                                                 
         MVC   SORTDAY,OUTSDAY                                                  
         MVC   SORTTIME,OUTSTIME                                                
         MVC   SORTWEEK,OUTWEEKS                                                
         MVC   SORTPROG,OUTTITLE                                                
         DROP  RE                                                               
*                                                                               
         ZICM  RF,SVOUTLEN,2   MOVE ENTIRE RECORD INCLUDING 4 BYTE              
         LA    RF,4(RF)        ADD ANOTHER 4BYTES FOR VARIABLE LEN              
         STCM  RF,3,SVODATA    OF INPUT RECORD                                  
         LR    R1,RF                                                            
         LA    R0,SVODATA+4   VARIABLE LENGTH                                   
         LA    RE,INREC                                                         
         MVCL  R0,RE                                                            
         ZICM  R1,SVOUTLEN,2  INPUT RECORD LENGTH PLUS SORT LENGTH              
         LA    R1,39(R1)      SORTKEY LENGTH 35+4 BYTE LENGTH                   
         LA    R1,4(R1)       ADD ANOTHER 4BYTES FOR VARIABLE LEN               
         STCM  R1,3,SVOUTLEN  OF THE INPUT RECORD                               
         LA    RF,SVOUTREC                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SVOUTREC                                 
*                                                                               
                                                                                
         LA    RE,OUTREC                                                        
         XCEF  (RE),1004                                                        
         B     GET                                                              
ENDJOB   DS    0C                                                               
         B     CLSE                                                             
CLSE     CLI   GETSW,5                                                          
*                                                                               
CLSE2    CLOSE (IN)                                                             
*                                                                               
GETRECS  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         LTR   R2,R2                                                            
         BZ    OUTPROGX                                                         
         LA    R2,4(R2)             STUPID LENGTH                               
         USING SORTKEY,R2                                                       
*                                                                               
         PUT   OUT,SORTOREC                                                     
         B     GETRECS                                                          
*                                                                               
         EJECT                                                                  
OUTPROGX DS    0C                                                               
         GOTO1 =V(SORTER),DMCB,=C'END',0,0                                      
         CLOSE (OUT)                                                            
         B     EXIT                                                             
************************************************                                
*  PARSER                                                                       
* ON ENTRY R2-POINTS TO BEGINNING OF WHERE TO START PARSING                     
* ON EXIT  WORK- HAS THE LENGTH OF FIELD                                        
*          WORK+1- HAS THE PARSED FIELD                                         
*                                                                               
*                                                                               
PARSER   NTR1                                                                   
         LA    RE,WORK+1                                                        
         XC    WORK,WORK                                                        
         LA    R7,0                                                             
PAR10    CLI   0(R2),C' '                                                       
         BE    PAR20                                                            
         MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         B     PAR10                                                            
PAR20    STC   R7,WORK                                                          
PARXX    XIT1                                                                   
EXIT     XBASE                                                                  
*                                                                               
         EJECT                                                                  
STXTAB   DS    0F                                                               
         DC    A(DETMPRD)                                                       
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(4,5,A,9,30,A),FORMAT=BI'                       
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1030'                                  
LNBUFF   EQU   L'PNAME+L'HN+L'GS+L'ONUMS                                        
LNBUF2   EQU   CLEARE-CLEARS                                                    
         DC    CL8'**WORK**'                                                    
SAVER8   DC    A(0)                                                             
DMCB     DS    6F                                                               
ARDEF    DC    A(0)                                                             
GETSW    DS    C                                                                
MATCH    DS    C                                                                
ALPHA    DS    C                                                                
NUMSW    DS    C                                                                
RFIELD   DS    C                                                                
HX       DS    C                                                                
HA       DS    C                                                                
HN       DS    C                                                                
GS       DS    C                                                                
MULTISW  DS    C                                                                
DPT3     DS    CL3                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
PNUM     DS    H                                                                
ADPTCUME DS    F                                                                
WORK     DS    CL20                                                             
WORK2    DS    CL80                                                             
PNAME    DS    CL50                                                             
         DC    CL8'*CLEARS*'                                                    
CLEARS   DS    0C                                                               
TVQNAME  DS    CL60                                                             
TVQNET   DS    CL4                                                              
TVQPTYP  DS    CL4                                                              
TVQDPT   DS    CL6                                                              
NTINET   DS    CL6                                                              
NTINTI   DS    XL5                                                              
NTIFILT  DS    XL2                                                              
NTILP    DS    CL24                                                             
CLEARE   DS    0C                                                               
TBSTART  DS    0C                                                               
TBPNAME  DS    CL50                                                             
TBNET    DS    CL1                                                              
TBDPT    DS    CL1                                                              
TBIQ     DS    XL50                                                             
TBTVQ    DS    XL50                                                             
TBEND    DS    0C                                                               
P2       DS    CL132                                                            
         DC    CL8'**IREC**'                                                    
INREC    DS    CL606    800                                                     
         DC    CL8'**ONUM**'                                                    
ONUMS    DS    XL100                                                            
         DC    CL8'**DNUM**'                                                    
DNUMS    DS    XL100                                                            
DPIQ     EQU   DNUMS                                                            
DPTVQ    EQU   DNUMS+50                                                         
         LTORG                                                                  
*                                                                               
************************************************                                
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
IN       DCB   DDNAME=FILEIN,                                          X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=FB,                                               X        
               LRECL=00606,                                            X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=OTAPE,DSORG=PS,RECFM=VB,LRECL=1004,BLKSIZE=5000, X        
               MACRF=PM                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
QTRTAB   DS    0CL2                                                             
         DC    CL1'1',AL1(2)                                                    
         DC    CL1'2',AL1(5)                                                    
         DC    CL1'3',AL1(7)                                                    
         DC    CL1'4',AL1(11)                                                   
         DC    AL1(0)                                                           
*                                                                               
DAYTAB   DS    0CL3                                                             
         DC    CL2'MO',XL1'10'                                                  
         DC    CL2'TU',XL1'20'                                                  
         DC    CL2'WE',XL1'30'                                                  
         DC    CL2'TH',XL1'40'                                                  
         DC    CL2'FR',XL1'50'                                                  
         DC    CL2'SA',XL1'60'                                                  
         DC    CL2'SU',XL1'70'                                                  
         DC    AL1(0)                                                           
*  RECORD TYPE 2 DEMO LAYOUT TABLE                                              
RECTYP2  DS   0CL4                                                              
         DC   AL4(3),AL1(4)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(3)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(4)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(3)                                                     
         DC   AL4(5),AL1(0)                                                     
         DC   AL4(0),AL1(0)                                                     
*                                                                               
*  RECORD TYPE 3 DEMO LAYOUT TABLE                                              
RECTYP3  DS   0CL4                                                              
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(3)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(5),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(0),AL1(0)                                                     
*                                                                               
*  RECORD TYPE 4 DEMO LAYOUT TABLE                                              
RECTYP4  DS   0CL4                                                              
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(3)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(3),AL1(0)                                                     
         DC   AL4(5),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(4),AL1(0)                                                     
         DC   AL4(0),AL1(0)                                                     
*                                                                               
*  RECORD TYPE 7 DEMO LAYOUT TABLE                                              
RECTYP7  DS   0CL4                                                              
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(10),AL1(0)                                                    
         DC   AL4(0),AL1(0)                                                     
*                                                                               
FRSTFLG  DS    CL1                                                              
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
SVOUTREC DS    0XL1043                                                          
SVOUTLEN DS    XL4                                                              
SVSORTK  DS    XL35                                                             
SVODATA  DS    XL1004                                                           
OUTKEY   DS    CL(L'DRKMAJOR+L'DRKMINOR)                                        
OUTREC   DS    0XL1004       MAX LENGTH 1004 WITH LENGTH                        
OUTHEAD  DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
         DC    CL8'**TOTS**'                                                    
SUMTABS  DS    0C                                                               
PRIME    DS    XL400                                                            
KID      DS    XL400                                                            
DAY      DS    XL400                                                            
NEWS     DS    XL400                                                            
KIDS     DS    XL400                                                            
SPORTS   DS    XL400                                                            
LATE     DS    XL400                                                            
MISC     DS    XL400                                                            
SUMTABE  DS    0C                                                               
         EJECT                                                                  
* PROGRAM EXTENSIONS                                                            
DP1      DC    AL2(DP2-DP1)        ABC COLLEGE BASKETBALL                       
         DC    CL4'ABC'                                                         
         DC    AL2(2541)           BASE                                         
         DC    AL2(287,1629,287)                                                
         DC    X'FFFF'                                                          
DP2      DC    AL2(DP3-DP2)        ABC NFL PRE-SEASON GAME                      
         DC    CL4'ABC'                                                         
         DC    AL2(34665)          BASE                                         
         DC    AL2(34823)                                                       
         DC    X'FFFF'                                                          
DP3      DC    AL2(DP4-DP3)        ABC PGA TOUR                                 
         DC    CL4'ABC'                                                         
         DC    AL2(1401)           BASE                                         
         DC    AL2(22625,22627,2528,36261,36263,6689,37671,37672)               
         DC    AL2(55917,55855,1611,1612,114,1797,1792,32352,32335)             
         DC    X'FFFF'                                                          
DP4      DC    AL2(DP5-DP4)        CBS COLLEGE BASKETBALL                       
         DC    CL4'CBS'                                                         
         DC    AL2(2672)           BASE                                         
         DC    AL2(30632,56695,2672,30632,56695,2799,315,361,1598)              
         DC    AL2(1599,1601,2848,25366,1350,2768,2769,25147,25148)             
         DC    AL2(25136,25137,2754,2755,24972,24973,24974,11814)               
         DC    AL2(24976,24977,24978,24946,24947,24940,24941,24948)             
         DC    AL2(24949,24942,24943,25146,30632,483,3016,3014,1408)            
         DC    X'FFFF'                                                          
DP5      DC    AL2(DP6-DP5)        FOX NFL PRE-SEASON GAME                      
         DC    CL4'FOX'                                                         
         DC    AL2(53186)          BASE                                         
         DC    AL2(53505)                                                       
         DC    X'FFFF'                                                          
DP6      DC    AL2(DP7-DP6)        FOX NHL HOCKEY                               
         DC    CL4'FOX'                                                         
         DC    AL2(333)            BASE                                         
         DC    X'FFFF'                                                          
DP7      DC    AL2(DP8-DP7)        FOX SATURDAY BASEBALL                        
         DC    CL4'FOX'                                                         
         DC    AL2(713)            BASE                                         
         DC    X'FFFF'                                                          
DP8      DC    AL2(DP9-DP8)        NBC COLLEGE FOOTBALL                         
         DC    CL4'NBC'                                                         
         DC    AL2(35223)          BASE                                         
         DC    AL2(261)                                                         
         DC    X'FFFF'                                                          
DP9      DC    AL2(DP10-DP9)       NBC COLLEGE BASKETBALL                       
         DC    CL4'NBC'                                                         
         DC    AL2(3717)           BASE                                         
         DC    X'FFFF'                                                          
DP10     DC    AL2(DP11-DP10)      NBC NBA GAME                                 
         DC    CL4'NBC'                                                         
         DC    AL2(1221)           BASE                                         
         DC    AL2(1223,63839,1499,2687,23982,24178,25482,38922)                
         DC    AL2(23982,24178,23982,24178,23982,24178)                         
         DC    X'FFFF'                                                          
DP11     DC    AL2(DP12-DP11)      NBC WNBA GAME                                
         DC    CL4'NBC'                                                         
         DC    AL2(1992)           BASE                                         
         DC    AL2(2175)                                                        
         DC    X'FFFF'                                                          
DP12     DC    AL2(DP13-DP12)      NBC NFL PRE-SEASON                           
         DC    CL4'NBC'                                                         
         DC    AL2(27088)          BASE                                         
         DC    AL2(43672)                                                       
         DC    X'FFFF'                                                          
DP13     DC    AL2(DP14-DP13)      NBC PGA TOUR                                 
         DC    CL4'NBC'                                                         
         DC    AL2(28436)          BASE                                         
         DC    AL2(28463,65211,65225,2673,2674,24296,24297,48445)               
         DC    AL2(48392,7479,7489,48798,48815,545,535)                         
         DC    X'FFFF'                                                          
DP14     DC    AL2(DP15-DP14)      OLYMPIC WINTERFEST                           
         DC    CL4'CBS'                                                         
         DC    AL2(02761)          BASE                                         
         DC    AL2(2512,2563)                                                   
         DC    X'FFFF'                                                          
DP15     DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
         DS    C'PROGBUFF'                                                      
PROGBUFF DS    65000C                                                           
         DS    C'PROGBUF2'                                                      
PROGBUF2 DS    100000C                                                          
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
*  DSECT OF INPUT REC                                                           
INRECD   DSECT                                                                  
INSURV   DS    CL7                                                              
INMKTN   DS    XL5                                                              
INMKTD   DS    CL18                                                             
INCALL   DS    CL6                                                              
INTITN   DS    CL5                                                              
INTITLE  DS    CL16                                                             
         DS    CL1                                                              
INTIME   DS    0CL22                                                            
INSDAY   DS    CL2                                                              
INEDAY   DS    CL2                                                              
         DS    CL1                                                              
INSTIME  DS    CL4                                                              
         DS    CL1                                                              
INETIME  DS    CL4                                                              
         DS    CL1                                                              
INAMPM   DS    CL2                                                              
         DS    CL1                                                              
INWEEKS  DS    CL4                                                              
INRECTY  DS    CL1                                                              
INQTRHR  DS    CL4                                                              
INKEYL   EQU   *-INRECD                                                         
INDEMOS  DS    0C                                                               
*                                                                               
*  DSECT OF OUTPUT REC                                                          
OUTRECD   DSECT                                                                 
OUTMKTN   DS    XL5                                                             
OUTCALL   DS    CL6                                                             
OUTSDAY   DS    CL1                                                             
OUTEDAY   DS    CL1                                                             
OUTSTIME  DS    XL2                                                             
OUTETIME  DS    XL2                                                             
OUTWEEKS  DS    CL4                                                             
OUTTITLE  DS    CL16                                                            
OUTHDLEN  EQU   *-OUTRECD                                                       
OUTDEMOS  DS    0C                                                              
*                                                                               
*SORT KEY DSECT                                                                 
SORTKEY   DSECT                                                                 
SORTMKTN  DS    XL5                                                             
SORTSTAT  DS    CL5                                                             
SORTDAY   DS    CL1                                                             
SORTTIME  DS    XL4                                                             
SORTWEEK  DS    CL4                                                             
SORTPROG  DS    CL16                                                            
SORTKEYL  EQU   *-SORTKEY                                                       
SORTOREC  DS    0C                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079DETMPRD   03/23/15'                                      
         END                                                                    
