*          DATA SET SPREQ02    AT LEVEL 045 AS OF 01/06/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T20802A                                                                  
*INCLUDE DPTRD                                                                  
*INCLUDE NSIWEEK                                                                
         TITLE 'SPREQ02 - REQUEST - DISPLAY/UPDATE REQUEST FILE'                
* --------------------------------------------------------------------*         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-40755  11/11/19 FIX BUG FROM SPEC-37908                   *         
* AKAT CUSTENH3341 10/28/16 ALLOW MKT 0000 D8 REQ FOR CANADA MEDIA N  *         
* AKAT SPSUG-85    07/08/16 ALLOW AGENCY OU TO REQUEST GT REPORT      *         
* AKAT SPSUG-86    07/08/16 ALLOW AGENCY OO TO REQUEST GT REPORT      *         
* AKAT CSD-477     07/08/16 ALLOW AGENCY UB TO REQUEST GT REPORT      *         
* AKAT ITMF-2462   01/21/16 RELAX PERIOD LIMIT FOR RN REPORT AGY ON   *         
* --------------------------------------------------------------------*         
T20802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDX-WORKD,T20802,CLEAR=YES,RR=R9                              
         USING WORKD,RC                                                         
*                                                                               
         ST    R9,RELO                                                          
         L     R9,0(R1)                      R9=A(W/S)                          
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LA    R8,T20802+4095                                                   
         LA    R8,1(R8)                                                         
         LA    R7,1(R8)                                                         
         LA    R7,4095(R7)                                                      
         USING T20802,RB,R8,R7   ** RB,R8,R7 ARE BASE REGISTERS                 
*                                                                               
         L     R1,=A(CHKRNUM)                                                   
         A     R1,RELO                                                          
         ST    R1,ACHKRNUM                                                      
         L     R1,=A(VVALMAX)                                                   
         A     R1,RELO                                                          
         ST    R1,AVVALMAX                                                      
         MVI   DMIN,X'20'                                                       
         MVI   SPACES,X'40'                                                     
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         L     RF,ACOMFACS                                                      
         MVC   VLOCKET,CLOCKET-COMFACSD(RF)                                     
         EJECT                                                                  
         LR    R2,R3                                                            
         USING T208FFD,R2                                                       
IOCTL    MVC   FERN,=AL2(FF)                                                    
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         CLI   STATUS,3                                                         
         BNE   IOCTL3                                                           
         GOTO1 ACHKRNUM,DMCB,(R9)                                               
         CLI   REQOPTN,C'T'                                                     
         BNE   ENQREQ                                                           
         B     TOTREQ                                                           
IOCTL3   CLI   REQACTN,C'A'                                                     
         BE    CHKIT                                                            
         CLI   REQACTN,C'N'                                                     
         BE    CHKIT                                                            
         GOTO1 ACHKRNUM,DMCB,(R9)                                               
         CLI   REQACTN,C'D'                                                     
         BE    CANREQ                                                           
         DC    H'0'                                                             
*                                                                               
REQIOERR MVC   FERN,=AL2(0)                                                     
         SPACE 2                                                                
CLEARADR XC    LADR,LADR                                                        
         B     EXIT                                                             
         SPACE 2                                                                
SAVEADR  MVC   LADR,ADR                                                         
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        ANY GLOBAL REQUEST POST VALIDATION CODE COMES HERE                     
*                                                                               
CHKIT    EQU   *                                                                
         CLI   LREQMAP,126                   CARD REQUEST                       
         BE    CHKREQX                       YES THEN NO VALIDATION             
         CLC   RMARK(3),=C'GLL'              SPECIAL MARKET CODE                
         BNE   *+12                                                             
         MVI   RMARK,C'A'                                                       
         MVI   RDIST,C'G'                                                       
         SPACE 2                                                                
*        CHECK IF REQUEST FIELDS REQUIRE FURTHER VALIDATION                     
*                                                                               
         CLI   MLTREQSW,C'Y'       IF IT IS A MULTIPLE REQ ADD                  
         BNE   CHKREQ                                                           
         MVC   REQRECSV,REQREC     SAVE REQUEST REQ BEFORE POSTVAL              
*                                                                               
CHKREQ   L     R5,=A(VALROUTS)                                                  
         A     R5,RELO                                                          
CHKREQ0  CLI   0(R5),0                                                          
         BE    CHKREQX                       NOT IN TBL NO VAL REQUIRED         
         CLC   REQNUM(2),0(R5)                                                  
         BE    *+12                                                             
         LA    R5,L'VALROUTS(R5)                                                
         B     CHKREQ0                                                          
         MVC   DUB+1(3),2(R5)                                                   
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         BASR  RA,RF                                                            
         CLC   FERN,=AL2(FF)                                                    
         BE    CHKREQX                       ALL FIELDS OK                      
         SPACE 2                                                                
CHKREQ00 LA    R0,24                         SEARCH REQ MAP TABLE               
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP                    NOT IN TBL POSN TO 1ST FLD         
CHKREQ3  MVC   HALF,1(R1)                                                       
         LR    R6,R3                                                            
         AH    R6,HALF                                                          
CHKREQ4  ST    R6,FADR                       POSN CURSOR TO ROUTNUM FLD         
         B     EXIT                                                             
         SPACE 2                                                                
CHKREQX  DS    0H                                                               
         GOTO1 ACHKRNUM,DMCB,(R9)                                               
         CLI   REQACTN,C'N'                                                     
         BE    NEWREQ                                                           
         B     AMDREQ                                                           
         EJECT                                                                  
*        ROUTINES FOR FURTHER FIELD VALIDATION                                  
*                                                                               
VALR02   BRAS  RE,VVALR02                                                       
         BR    RA                                                               
                                                                                
VALR03   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VR03K                                                            
         CLI   RO6,C'Y'            MUST BE TEST RUN FOR SOON                    
         BE    VR03K                                                            
         MVI   ROUTNUM,X'74'       CURSOR TO TEST RUN                           
         B     VALINV                                                           
*                                                                               
VR03K    BRAS  RE,VVALR03          FOR PH SOME CLTS ALLOWED                     
         BNE   VALINV                                                           
         BR    RA                                                               
*                                                                               
VALR05   DS    0H                                                               
         CLC   =C'NO',REST          IF EST = NO                                 
         BNE   VAL05X                                                           
         MVC   RENDD,=X'FAF9F1F2F3F1'  SET END DATE                             
         CLI   RSTRD,X'40'             MUST HAVE START                          
         BNE   VAL05X                  OK                                       
         MVI   ROUTNUM,X'D6'           NO-ERROR                                 
         B     VALINV                                                           
VAL05X   BR    RA                                                               
*                                                                               
VALR06   DS    0H                                                               
         MVC   REST1,=X'404040'    CLEAR THIS IN CASE SET                       
         MVC   HALF,=X'0501'                 5  WEEKS                           
         BAS   RE,VALMAX                                                        
         CLC   RENDD,ESTDATES+6    END DATE = EST END?                          
         BE    VALR06B                        YES-OK                            
         GOTO1 GETDAY,DMCB,(0,RENDD),TEMP     NO-MUST BE SUNDAY                 
         CLI   DMCB,7              SUNDAY                                       
         BE    VALR06B                                                          
         MVC   FERN,=AL2(830)                                                   
         B     VAL06X                                                           
VALR06B  CLC   RBOOK,SPACES             IF RBOOK = SPACES                       
         BNE   VAL06X                                                           
         CLI   RBOOK1,X'40'        RERATE TYPE MUST = X'40'                     
         BE    VAL06X                                                           
         CLI   RBOOK1,C'P'         RERATE TYPE MUST = P                         
         BE    VAL06X                                                           
         MVI   ROUTNUM,X'55'                                                    
         B     VALINV                                                           
VAL06X   BR    RA                                                               
*                                                                               
VALR07   DS    0H                                                               
         BRAS  RE,VVALR07                                                       
         BNE   VALINV                                                           
         BR    RA                                                               
*                                                                               
VALR08   DS    0H                                                               
         BRAS  RE,VVALR08                                                       
         BNE   VALINV                                                           
         BR    RA                                                               
*                                                                               
VALR12   DS    0H                  SPOT/NET UNBILLING ONLY RUNS SOON            
         CLI   BVRDESTH+5,0         AND REQUIRES DESTINATION                    
         BNE   VALR12D                                                          
         LA    R1,BVRDESTH                                                      
         ST    R1,FADR                                                          
         B     VALINV                                                           
VALR12D  CLC   =C'SOON',BVROUT                                                  
         BE    VALR12X                                                          
         LA    R1,BVROUTH                                                       
         ST    R1,FADR                                                          
         B     VALINV                                                           
VALR12X  BR    RA                                                               
*                                                                               
VALR14   DS    0H                  GT EXTRACT                                   
         BRAS  RE,VVALR14                                                       
         BE    VALR14X                                                          
         MVI   ROUTNUM,X'02'                                                    
         B     VALINV                                                           
VALR14X  BR    RA                                                               
*                                                                               
VALR15   DS    0H                  SUPERDESK STAUS                              
         CLC   RPRO,SPACES         IF BLANK PUT 'ALL'                           
         BNE   *+10                                                             
         MVC   RPRO,=C'ALL'                                                     
*                                                                               
         CLC   REST,SPACES                                                      
         BNE   *+10                                                             
         MVC   REST,=C'ALL'                                                     
*                                                                               
VALR15X  BR    RA                                                               
*                                                                               
VALR17   DS    0H                  DEMO LOOKUP ELEM FIX                         
         CLC   RSTRD(2),=X'FBF6'   DATE MUST BE 2016 OR AFTER                   
         BNL   VALR17X                                                          
         MVI   ROUTNUM,X'D6'       CURSOR TO DATE                               
         MVC   FERN,=AL2(FLDINV)                                                
VALR17X  BR    RA                                                               
*                                                                               
          SPACE 2                                                               
VALR18   DS    0H                            EST=ALL                            
         MVC   HALF,=X'0701'                 07 WEEKS                           
         BAS   RE,VALMAX                                                        
         BAS   RE,VALIDS                                                        
VR18X    BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR19   CLC   RCLI,=C'LBT'     MUST BE CLIENT LBT - LABATT INTERFACE           
         BE    VR19X            (HDTO)                                          
         CLC   RCLI,=C'LAB'     MUST BE CLIENT LAB - LABATT INTERFACE           
         BE    VR19X            (M2TOA)                                         
         CLC   RCLI,=C'LBB'     OR CLIENT LBB - LABATT INTERFACE                
         BE    VR19X            (M2TOA)                                         
         MVI   ROUTNUM,X'02'       CURSOR TO CLIENT                             
         B     VALINV                                                           
VR19X    BR    RA                                                               
*                                                                               
VALR20   BRAS  RE,VVALR20                                                       
         BR    RA                                                               
*                                                                               
VALR21   TM    CLISAVE,X'04'       IF CLT AND/OR PRD=XXX,NO EST=ALL             
**       BO    VR21A                                                            
**       TM    PROSAVE,X'04'                                                    
**       BNO   VR21C                                                            
VR21A    TM    ESTSAVE,X'02'                                                    
**       BNO   VR21C                                                            
**       MVI   ROUTNUM,X'05'       SET CURSOR TO EST                            
**       MVI   FERN,ESTALLX                                                     
**       B     VR21X                                                            
VR21C    DS    0H                                                               
         CLI   RREPNO,C' '                                                      
         BE    *+8                                                              
         MVI   RREPT,C'S'                    SET SPECIAL REP CODE               
         MVC   HALF,=X'0701'                 07 WEEKS                           
         BAS   RE,VALMAX                                                        
         MVI   ROUTNUM,35                    AMOUNT TYPE                        
         CLI   RAMTT,C' '                                                       
         BE    VR21X                                                            
         CLI   RAMT,C' '                                                        
         BNE   VR21X                                                            
         MVC   FERN,=AL2(AMTMIS)                                                
VR21X    BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR23   EQU   *                                                                
         GOTO1 =A(VVALR23),DMCB,(R9),(RC),RR=RELO                               
*                                                                               
         CLC   FERN,=AL2(NUMINV)                                                
         BNE   VALR23X                                                          
         LA    R1,BVRNUMH                                                       
         ST    R1,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALR23X  BR    RA                                                               
*                                                                               
*                                                                               
VALR24   DS 0H                                                                  
         BRAS  RE,VVALR24                                                       
         BNE   VALINV                                                           
         BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR27   DS 0H                                                                  
         BRAS  RE,VVALR27                                                       
         BNE   VALINV                                                           
         BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR41   DS 0H                                                                  
         BRAS  RE,VVALR41                                                       
         BR    RA                                                               
*                                                                               
VALR42   DS    0H                                                               
         BRAS  RE,VVALR42                                                       
         BR    RA                                                               
*                                                                               
         SPACE 2                                                                
VALR13M  MVC   HALF,=X'0D00'                 13 MONTHS                          
         BAS   RE,VALMAX                                                        
VR13MX   BR    RA                                                               
*                                                                               
VALR44   DS    0H                                                               
         MVI   ROUTNUM,X'61'       DATA OPTION                                  
         CLI   RMED,C'R'           IF RADIO                                     
         BNE   VR44X                                                            
         CLI   RO1,C'L'            MUST BE 'L'                                  
         BNE   VALINV                                                           
VR44X    BR    RA                                                               
*                                                                               
VALR47   CLI   RO3,C'Y'            SCHEME CHK                                   
         BNE   VR47X               NO                                           
         MVI   ROUTNUM,X'7B'                                                    
         CLI   RDIST,C' '                                                       
         BE    VR47ERR                                                          
         CLI   RO2,C'Y'            MUST BE DOING MKTGRPS                        
         BE    VR47X                                                            
*                                                                               
VR47ERR  B     VALINV                                                           
VR47X    BR    RA                                                               
         SPACE 2                                                                
VALR48   CLI   RO3,X'40'           OPT3 ONLY IF OPT1=S OR ALL                   
         BNH   VR48D               NO                                           
         CLI   RO1,X'40'                                                        
         BNH   VR48D                                                            
         CLI   RO1,C'S'                                                         
         BE    VR48D                                                            
         MVI   ROUTNUM,X'D5'                                                    
         B     VALINV                                                           
VR48D    CLI   RO6+1,C'Y'           LOCK REQUIRES DOWN AND STATION              
         BNE   VR48X                                                            
         CLI   RO6,C'D'                                                         
         BE    VR48X                                                            
         MVI   ROUTNUM,X'CB'        CURSOR TO OPTIONS FIELD                     
         B     VALINV                                                           
VR48X    BR    RA                                                               
         SPACE 2                                                                
VALR49   MVC   HALF,=X'3900'                 58 MONTHS                          
         BAS   RE,VALMAX                                                        
VR49X    BR    RA                                                               
         SPACE 2                                                                
                                                                                
VALR50   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VR50B                                                            
         CLI   RO6,C'Y'            MUST BE TEST RUN FOR SOON                    
         BE    VR50B                                                            
         MVI   ROUTNUM,X'74'       CURSOR TO TEST RUN                           
         B     VALINV                                                           
*                                                                               
VR50B    CLC   =C'ALL',REST                                                     
         BNER  RA                                                               
         MVC   REST(6),=C'001255'                                               
         BR    RA                                                               
*                                                                               
VALR52   DS    0H                  IS THERE NO 52 REQUEST ANYMORE?              
*                                  NO WAY TO GET TO VALR52                      
*                                  BUT VALR52A USED BY OTHER REQUESTS           
*                                                                               
******   MVC   HALF,=X'1A01'                 26 WEEKS                           
******   B     VR52                                                             
******                                                                          
******   MVC   HALF,=X'0D00'       13 MTHS FOR A2 - REQ 192                     
******   BAS   RE,VALMAX                                                        
******   BAS   RE,VALEST                                                        
VALR52A  MVI   ROUTNUM,37                    ANALYSIS TYPE                      
         TM    PROSAVE,X'08'                                                    
         BZ    VR52A                                                            
         CLI   RO1,C'R'                                                         
         BE    *+12                                                             
         CLI   RO1,C'B'                                                         
         BNE   VR52A                                                            
         MVC   FERN,=AL2(PROPOLX)           R/B INVALID FOR PRO=POL             
         B     VR52X                                                            
VR52A    CLI   RO1,C'N'                                                         
         BNE   VR52B                                                            
         TM    PROSAVE,X'08'                                                    
         BO    VR52B                                                            
         MVC   FERN,=AL2(PROPOL)              L INVALID UNLESS PRO=POL          
         B     VR52X                                                            
VR52B    MVI   ROUTNUM,12                    PAY PERIOD START                   
         CLC   RNUM+49(6),RNUM+55                                               
         BNH   VR52D                                                            
         MVC   FERN,=AL2(SEDSGE)                   ERROR START GT END           
         B     VR52X                                                            
*                                                                               
VR52D    CLC   RNUM+55(6),SPACES                                                
         BE    VR52X                                                            
         CLC   RNUM+49(6),SPACES             END BUT NO START                   
         BNE   VR52X                                                            
         MVC   FERN,=AL2(FLDMIS)                                                
VR52X    BR    RA                                                               
         SPACE 2                                                                
VALR53   MVC   HALF,=X'1A01'                 26 WEEKS                           
         BAS   RE,VALMAX                                                        
VR53X    BR    RA                                                               
         SPACE 2                                                                
VALR59   MVC   HALF,=X'0D00'                 13 MONTHS                          
         BAS   RE,VALMAX                                                        
VR59X    BR    RA                                                               
         SPACE 2                                                                
VALR61   MVC   HALF,=X'1201'                 16 WEEKS                           
         BAS   RE,VALMAX                                                        
VR61X    BR    RA                                                               
         SPACE 2                                                                
VALR74   BRAS  RE,VVALR74                                                       
         BR    RA                                                               
         SPACE 2                                                                
VALR90   MVC   RENDD,RSTRD                   SET CURRENT MONTH                  
         BR    RA                                                               
         SPACE 2                                                                
VALR91   BRAS  RE,VVALR91                                                       
         BNE   VALINV                                                           
         BR    RA                                                               
         SPACE 2                                                                
VALR98   MVC   HALF,=X'1001'           16 WEEKS                                 
         BAS   RE,VALMAX                                                        
VALR98X  BR    RA                                                               
         SPACE 2                                                                
VALR101  LA    R6,1                MONDAY                                       
         LA    R5,RSTRD                                                         
         BAS   RE,VALDAY                                                        
         LA    R6,7                SUNDAY                                       
         LA    R5,RENDD                                                         
         BAS   RE,VALDAY                                                        
         MVC   HALF,=X'0D00'                                                    
         BAS   RE,VALMAX                                                        
VALR101X BR    RA                                                               
         SPACE 2                                                                
VALR102  DS    0H                                                               
         BRAS  RE,VVALR102                                                      
         BR    RA                                                               
         SPACE 2                                                                
         SPACE 2                                                                
VALR103  DS    0H                  SC REPORT                                    
         BRAS  RE,VVALR103                                                      
         BR    RA                                                               
         SPACE 2                                                                
VALR106  DS    0H                  SP REPORT                                    
         CLI   RO1,C'N'            IF MARKING FILES                             
         BE    VR106X                                                           
         BRAS  RE,CKUPDT           CHECK IF UPDATE IS ALLOWED                   
         BNE   *+14                                                             
         MVI   ROUTNUM,X'17'                                                    
         MVC   FERN,=AL2(1250)                                                  
VR106X   BR    RA                                                               
         SPACE 2                                                                
VALR107  DS    0H                                                               
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALACC                                                        
         BAS   RE,VALIDS                                                        
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
VR107G   BR    RA                                                               
         SPACE 2                                                                
VALR1120 CLI   RO7,C' '                                                         
         BE    *+8                                                              
         MVI   RO6,C'F'                                                         
         CLI   RNUM+55,C'U'        IF UPGRADE                                   
         BNE   VALR112                                                          
         CLI   RNUM+56,C'C'        DATA COMPARE MUST BE 'C'                     
         BE    VALR112A                                                         
         MVI   ROUTNUM,X'19'                                                    
         B     VALINV                                                           
                                                                                
VALR112  BAS   RE,VALNDCB                                                       
VALR112A BAS   RE,VALDPOR               DPT OVERIDE                             
         BAS   RE,VALEST                                                        
VALR112B MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALRRATE                                                      
VALR112X BR    RA                                                               
         SPACE 2                                                                
VALR114  BAS   RE,VALNDCB                                                       
         BAS   RE,VALDPOR               DPT OVERIDE                             
         BAS   RE,VALEST                                                        
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALRRATE                                                      
         CLI   RO7,C' '                                                         
         BE    *+8                                                              
         MVI   RO6,C'F'                                                         
VALR114X BR    RA                                                               
         SPACE 2                                                                
VALR118  BAS   RE,VALEST                                                        
         CLC   RAGY,=C'DF'         DATA OPT A-E ONLY FOR DF                     
         BE    VALR118X                                                         
         CLI   RO3,C'A'                                                         
         BL    VALR118X                                                         
         CLI   RO3,C'E'                                                         
         BH    VALR118X                                                         
         MVI   ROUTNUM,X'61'       CURSOR TO DATA OPT                           
         B     VALINV                                                           
*                                                                               
VALR118X B     VALR13M             REST SAME AS VALR13M                         
         SPACE 2                                                                
VALR119  BAS   RE,VALACC                                                        
         B     VALR118             REST SAME AS VALR118                         
         SPACE 2                                                                
VALR132  BRAS  RE,VVALR132                                                      
         BR    RA                                                               
         SPACE 2                                                                
VALR137  DS    0H                                                               
         BAS   RE,VALEST                                                        
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALACC                                                        
         BR    RA                                                               
         SPACE 2                                                                
VALR140  DS    0H                                                               
         BAS   RE,VALACC                                                        
         MVC   HALF,=X'0E01'       14 WEEKS                                     
         BAS   RE,VALMAX                                                        
VALR140X BR    RA                                                               
         SPACE 2                                                                
VALR150  DS    0H                                                               
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALEST                                                        
         BR    RA                                                               
         SPACE 2                                                                
VALR153  DS    0H                                                               
         MVC   HALF,=X'1800'       24 MTHS                                      
         BAS   RE,VALMAX                                                        
VALR153B BAS   RE,VALEST                                                        
         BAS   RE,VALDPOR                                                       
         BR    RA                                                               
         SPACE 2                                                                
VALR156  DS    0H                                                               
         TM    SEDSAVE,X'04'       SEE IF YYMM INPUT                            
         BZ    VR156X                                                           
         MVC   RSTRD+4(2),SPACES      YES - RESET DAYS TO SPACES                
         MVC   RENDD+4(2),SPACES                                                
VR156X   BR    RA                                                               
         SPACE 2                                                                
*                                                                               
*ALR159  DS    0H                                                               
*        CLI   RO1,C'B'                                                         
*        BNE   VR159X                                                           
*        CLC   RES,=C'ES'          CAN'T USE ES WITH BILLING DATES              
*        BNE   VR159X                                                           
*        MVC   FERN,=AL2(FLDINV)                                                
*        MVI   ROUTNUM,09                                                       
*R159X   BR    RA                                                               
*        SPACE 2                                                                
VALR160  DS    0H                                                               
         MVC   HALF,=X'0C00'       13 MTHS                                      
         B     VR168C                                                           
VALR168  DS    0H                                                               
         MVC   HALF,=X'0701'       7 WEEKS                                      
         CLC   RNUM,=C'D6'         D6 REPORT HAS MAX 56 DAY SPREAD              
         BNE   VR168C                                                           
         CLI   RBOOK1,C' '         IF RERATE TYPE ENTERED                       
         BNH   VR168A                                                           
         CLI   RBOOK,C' '          BOOK-HUT MUST BE FILLED IN                   
         BH    VR168A                                                           
         MVC   FERN,=AL2(FLDMIS)                                                
         MVI   ROUTNUM,X'0E'                                                    
         B     VR168X                                                           
VR168A   LA    R5,RSTRD                                                         
         BRAS  RE,VALRFP                                                        
         BE    VR168C                                                           
         GOTO1 ADDAY,DMCB,RSTRD,TEMP,F'55'                                      
         CLC   RENDD,TEMP                                                       
         BNH   *+10                                                             
         MVC   FERN,=AL2(103)                                                   
         B     VR168X                                                           
VR168C   BAS   RE,VALMAX                                                        
         BAS   RE,VALACC                                                        
         CLC   RNUM,=C'D2'                                                      
         BE    *+8                                                              
         BAS   RE,VALIDS                                                        
*                                                                               
         CLC   RNUM,=C'D4'                                                      
         BNE   *+10                                                             
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
*                                                                               
         BAS   RE,VALRRATE                                                      
         CLC   RNUM,=C'D2'                                                      
         BNE   VR168F                                                           
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
*                                                                               
         CLI   RCARD2+43,C' '      CHECK IF HAVE UNWPER W/OUT UNWIND            
         BNH   VR168X                                                           
         CLI   RCARD2+39,C'U'                                                   
         BE    VR168X                                                           
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(39),=C'CANNOT USE OPTION UNWPER WITHOUT UNWIND'           
         MVC   FERN,=AL2(FE)            SET FOR MY MESSAGE                      
*                                                                               
VR168F   CLC   RNUM,=C'ML'                                                      
         B     VR168X              YKVA BACKOUT FOR NOW                         
         BNE   VR168X                                                           
         CLC   RAGY,=C'GZ'         FOR GM CHECK GLOCK OPTION                    
         BNE   VR168X                                                           
         CLI   RO1,C' '            ANYTHING IN QOPT1?                           
         BH    VR168X              BETTER BE!                                   
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(30),=C'GLOCK OPTION MUST BE FILLED IN'                    
         MVC   FERN,=AL2(FE)            SET FOR MY MESSAGE                      
*                                                                               
VR168X   BR    RA                                                               
*                                                                               
VALR170  DS    0H                                                               
         LA    R5,RSTRD            J6 REPORT HAS MAX 14 WEEKS SPREAD            
         GOTO1 ADDAY,DMCB,RSTRD,TEMP,F'97'                                      
         CLC   RENDD,TEMP                                                       
         BNH   *+10                                                             
         MVC   FERN,=AL2(103)                                                   
VR170X   BR    RA                                                               
*                                                                               
VALR163  DS    0H                                                               
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALIDS                                                        
         CLC   =C'N5',RNUM         ,, IF N5                                     
         BNE   *+10                                                             
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
*                                                                               
* code below added at zen's request but test showed this situation was          
* already handled at station validation which requires specific mkt             
* for a specific station - don't know how requests got out there                
* for a specific station for all mkts                                           
**       CLC   =C'D5',RNUM         ,, IF D5                                     
**       BNER  RA                                                               
**       TM    STASAVE,X'04'       ,, AND SPECIFIC STATION                      
**       BNOR  RA                                                               
**       MVI   ROUTNUM,X'5E'                                                    
**       TM    MKTSAVE,X'08'       ,, MUST BE SPECIFIC MARKET                   
**       BNO   VALINV                                                           
         BR    RA                                                               
         SPACE 2                                                                
VALR166  BAS   RE,VALEST                                                        
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALNDCB                                                       
         BAS   RE,VALDPOR                                                       
         BR    RA                                                               
         SPACE 2                                                                
VALR169  EQU   *                                                                
         CLC   RNUM,=C'D8'          IF D8                                       
         BNE   VLR169A                                                          
*                                                                               
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
*                                                                               
         CLI   RO2,C'N'            IF REPLACE DEMO = N, DON'T CHK SOX           
         BE    VALR1690                                                         
         CLI   RCARD2+23,C'Y'      LKUP INCOMPATIBLE WITH REPL DEMO             
         BNE   *+12                                                             
         MVI   ROUTNUM,X'CB'        CURSOR TO OPTIONS FIELD                     
         B     VALINV                                                           
*                                                                               
         BRAS  RE,CKUPDT                                                        
         BNE   VALR1690                                                         
         MVI   ROUTNUM,X'62'                                                    
         MVC   FERN,=AL2(1250)                                                  
         BR    RA                                                               
VALR1690 CLI   RMED,C'N'           FOR MEDIA N & C, MKT=0, INVALID              
***      BE    *+12                MARKET 0000 NOW ALLOWED FOR D8               
         BE    VALR1695            REQUEST FOR MEDIA N                          
         CLI   RMED,C'C'                                                        
         BNE   VALR1695                                                         
         CLC   RMARK,=C'0000'                                                   
         BNE   VALR1695                                                         
         MVI   ROUTNUM,X'07'                                                    
         B     VALINV                                                           
VALR1695 MVI   ROUTNUM,X'0E'       POINT TO BOOK-HUT INPUT FIELD                
         CLI   RNUM+55,C'U'        IF COL 55 = U                                
         BE    VLR169AA                                                         
         CLI   RCARD2+71,X'40'     ELSE NOTHING IN RCARD2 COL71                 
         BH    VALINV                                                           
         B     VLR169A                                                          
VLR169AA DS    0H                                                               
         CLI   RNUM+50,C'0'        MUST BE YYMM                                 
         BL    VALINV                                                           
         CLI   RCARD2+71,C'0'      MUST BE YYMM                                 
         BL    VALINV                                                           
*                                                                               
VLR169A  CLI   RO2,C'S'                                                         
         BNE   *+12                                                             
         MVI   RO2,C'Y'                                                         
         MVI   RCARD2+20,C'S'                                                   
         BAS   RE,VALACC                                                        
         MVI   ROUTNUM,99                                                       
         CLI   RO3,C'Y'                                                         
         BNE   VALR169A                                                         
         CLI   RO2,C'Y'                                                         
         BE    VALR169A                                                         
         B     VALINV                                                           
*                                                                               
VALR169A CLC   BVROUT(4),=C'SOON'  IF SOON                                      
         BNE   VALR169B                                                         
         MVI   ROUTNUM,X'62'                                                    
         CLI   RO2,C'N'            REPLACE DEMO MUST = N                        
         BNE   VALINV                                                           
         MVI   ROUTNUM,X'63'                                                    
         CLI   RO3,C'N'            REPLACE OVERRIDES MUST = N                   
         BNE   VALINV                                                           
VALR169B B     VALR112                  REST SAME AS 112                        
*                                                                               
         SPACE 2                                                                
VALR171  DS    0H                                                               
*                                                                               
         CLC   REST(3),=C'ALL'    IF EST=ALL                                    
         BNE   VALR171E                                                         
         CLC   RSTA(3),=C'ALL'     AND NET=ALL                                  
         BNE   VALR171E                                                         
VR171D   CLI   RO4,C'U'             RO4 MUST = U                                
         BE    VALR171E                                                         
         MVI   ROUTNUM,X'61'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
VALR171E DS    0H                                                               
         CLC   RSTRD+4(2),SPACES                                                
         BE    VALR171X            MONTH INPUT - NO MAX                         
         MVC   HALF,=X'0D00'       13 MONTHS                                    
         BAS   RE,VALMAX                                                        
         BR    RA                                                               
*                                                                               
VALR172  CLC   RSTRD+4(2),SPACES                                                
         BE    VALR171X            MONTH INPUT - NO MAX                         
         BRAS  RE,VALCROSS                                                      
         BNER  RA                                                               
         CLC   RAGY,=C'ON'         AGENCY ON?                                   
         BE    VALR171X            YES - NO MAX                                 
         MVC   HALF,=X'0E01'       14 WEEKS                                     
         BAS   RE,VALMAX                                                        
VALR171X BR    RA                                                               
         EJECT                                                                  
*                                                                               
VALR192  DS    0H                                                               
         CLC   RNUM(2),=C'A2'           IF A2 REPORT                            
         BNE   *+14                                                             
         MVC   HALF,=C'A2'                                                      
         B     V192BF                                                           
         CLC   RNUM(2),=C'AB'      OR AB REPORT                                 
         BNE   CHKAX                                                            
         MVC   HALF,=C'AB'                                                      
V192BF   CLC   RNAME(8),=C'GROSS=BF'    AND IF REQUESTOR NAME =                 
         BE    VLGR10                                                           
         CLC   RNAME(4),=C'G=BF'                                                
         BNE   VLGR20                                                           
VLGR10   CLC   RCARD2+22(8),SPACES                                              
         BNE   INVNUM                                                           
         MVC   RCARD2+22(2),=C'BF'      SET BF IN CARD2                         
         MVC   RNAME,=C'ACC         '                                           
*                                                                               
CHKPOL   CLC   RPRO,=C'POL'        POL IS INVALID                               
         BNE   CHKEST                                                           
         MVI   ROUTNUM,93                                                       
         B     V192E                                                            
CHKEST   TM    ESTSAVE,X'26'            MUST BE EST=NNN OR ALL                  
         BNZ   V192A                                                            
         MVI   ROUTNUM,5                                                        
         B     V192E                                                            
*                                                                               
VLGR20   CLC   RNAME(6),=C'GROSS='       G(ROSS)=999.9999 MAX                   
         BNE   *+12                                                             
         LA    R4,RNAME+6                                                       
         B     VLGR30                                                           
         CLC   RNAME(2),=C'G='                                                  
         BE    *+18                                                             
         CLC   RCARD2+22(2),=C'BF'     CHK IF SET FROM OPTION FIELD             
         BE    CHKPOL                                                           
         B     CHKAX                                                            
         LA    R4,RNAME+2                                                       
VLGR30   SR    R1,R1                                                            
         LA    R5,9                                                             
         LR    RE,R4                                                            
VLGR32   CLI   0(RE),X'40'                                                      
         BE    VLGR34                                                           
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R5,VLGR32                                                        
         B     INVNUM                                                           
VLGR34   LR    R5,R1                                                            
         GOTO1 CASHVAL,PLIST,(4,0(R4)),(R5)                                     
         CLI   PLIST,0                                                          
         BNE   INVNUM                                                           
         L     RE,PLIST+4                                                       
         C     RE,=F'9999999'                                                   
         BH    INVNUM                                                           
         CHI   R5,4             IF MORE THAN 3 DIGITS, NEEDS DECIMAL            
         BL    VLGROK                                                           
         LR    RF,R4                                                            
         LA    RE,4                                                             
         CLI   0(RF),C'.'                                                       
         BE    VLGROK                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         B     INVNUM                                                           
VLGROK   CLC   RCARD2+22(8),SPACES                                              
         BNE   INVNUM                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCARD2+22(0),0(R4)                                               
         MVC   RNAME,=C'ACC         '                                           
         B     V192A                                                            
INVNUM   MVC   FERN,=AL2(FLDINV)         SET ERROR AND EXIT                     
         B     EXIT                                                             
*                                                                               
CHKAX    CLC   RNUM(2),=C'AX'      IS IT AX                                     
         BNE   SKIPROF                                                          
         MVC   HALF,=C'AX'                                                      
*                                  READ PROFILE FOR A2 AND SET DEFAULTS         
V192A    DS    0H                                                               
         BAS   RE,RDPROF                                                        
         OC    TEMP,TEMP                                                        
         BZ    SKIPROF                                                          
         CLI   RO1,X'40'                                                        
         BH    *+10                                                             
         MVC   RO1,TEMP+5          ANALYSIS TYPE                                
         CLI   RO2,X'40'                                                        
         BH    *+10                                                             
         MVC   RO2,TEMP+7          GOAL/AUTH TOTS                               
         CLI   RO3,X'40'                                                        
         BH    VA2A                                                             
         MVC   RO3,TEMP+6          PAID TOTALS                                  
         CLI   RO3,X'40'                                                        
         BH    VA2A                                                             
         MVI   RO3,C'N'                                                         
VA2A     CLI   RO4,X'40'                                                        
         BH    *+10                                                             
         MVC   RO4,TEMP+8          SUMMARIES ONLY                               
         CLI   RO5,X'40'                                                        
         BH    *+10                                                             
         MVC   RO5,TEMP+9          SPECIAL REP BREAKOUT                         
         CLI   RO6,X'40'                                                        
         BH    SKIPROF                                                          
         MVC   RO6,TEMP+11         PRINT COMMENTS                               
*                                                                               
SKIPROF  BAS   RE,VALACC                                                        
         BAS   RE,VALIDS                                                        
         CLC   RCLI,=C'ALL'        IF CLI = ALL                                 
         BNE   VALR192X                                                         
         CLC   RPRO,=C'POL'        .PRO MUST = POL                              
         BNE   POLERR                                                           
         TM    MKTSAVE,X'80'       .MKT GROUP MUST NOT = XNNNN                  
         BNO   VALR192B                                                         
         MVI   ROUTNUM,X'5E'                                                    
         B     V192E                                                            
POLERR   MVI   ROUTNUM,X'5D'                                                    
         B     V192E                                                            
VALR192B DS    0H                                                               
         CLC   REST(2),=C'NO'       .AND EST MUST = NO                          
         BE    VALR192X                                                         
         MVI   ROUTNUM,05                                                       
V192E    B     VALINV                                                           
*                                                                               
VALR192X MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         CLC   RNUM,=C'A2'         ALLOW EST=ALL FOR A2                         
         BE    VALR52A             REST SAME AS VALR52A                         
         CLC   RNUM,=C'AX'         ALLOW EST=ALL FOR AX                         
         BE    VALR52A             REST SAME AS VALR52A                         
         CLC   RNUM,=C'AB'         ALLOW EST=ALL FOR AB (LIKE A2)               
         BE    VALR52A             REST SAME AS VALR52A                         
         BAS   RE,VALEST                                                        
         B     VALR52A             REST SAME AS VALR52A                         
         SPACE 2                                                                
VALR194  CLC   REST(6),=C'001255'                                               
         BE    VR194X                                                           
         CLC   REST+3(3),=C'   '                                                
         BE    VR194X                                                           
         MVI   ROUTNUM,X'05'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
VR194X   BR    RA                                                               
         SPACE 2                                                                
VALR195  DS    0H                                                               
         LA    R5,RSTRD                                                         
         BRAS  RE,VALRFP                                                        
         BE    VR197C                                                           
         MVC   RSTRD+2(2),=C'01'   SET MTH TO 01                                
**       PACK  DUB,RENDD(2)                                                     
**       SP    DUB,=P'1'                                                        
**       OI    DUB+7,X'0F'                                                      
**       UNPK  RSTRD(2),DUB        EDN YEAR MINUS 1                             
         GOTO1 DATCON,PLIST,RENDD,(3,FULL)                                      
         ZIC   R1,FULL                                                          
         BCTR  R1,0                                                             
         STC   R1,FULL                                                          
         GOTO1 DATCON,PLIST,(3,FULL),DUB                                        
         MVC   RSTRD(2),DUB                                                     
*                                                                               
         B     VR197C              REST LIKE 197                                
         SPACE 2                                                                
VALR197  DS    0H                                                               
         CLC   RNUM,=C'A5'                                                      
         BNE   VR197B                                                           
         CLI   RPRO,X'40'                                                       
         BH    *+14                                                             
         MVC   RPRO(3),=C'ALL'                                                  
         OI    PROSAVE,X'02'                                                    
         CLC   REST(3),=3X'40'                                                  
         BNE   VR197B                                                           
         MVC   REST(2),=C'NO'                                                   
VR197B   MVC   HALF,=X'1800'       24 MTHS                                      
         BAS   RE,VALMAX                                                        
VR197C   CLI   RNUM+60,C'Y'        SEE IF DOING SUMMARIES ONLY                  
*                                  COL 61                                       
         BNE   VR197X                                                           
         TM    MKTSAVE,X'08'       SEE IF SINGLE MKT INPUT                      
         BZ    VR197E                                                           
         MVI   ROUTNUM,X'6E'       CURSOR TO SUMMARIES ONLY                     
         B     VALINV              YES ERROR                                    
*                                                                               
VR197E   CLI   RO6,C'3'            STATIONS ONLY INVALID                        
         BNE   VR197X              FOR SUMMARIES ONLY                           
         MVI   ROUTNUM,X'57'       CURSOR TO TOTAL OPTION                       
         MVC   FERN,=AL2(FLDINV)                                                
VR197X   BR    RA                                                               
         EJECT                                                                  
*                                                                               
VALR196  DS    0H                                                               
         LA    R5,RSTRD                                                         
         BRAS  RE,VALRFP                                                        
         BE    SKPDTCK                                                          
         CLI   RBOOK,X'40'         IF BLANK                                     
         BNH   SKPDTCK                                                          
         CLI   RBOOK,C'R'          OR REVERSAL BILL                             
         BE    SKPDTCK                                                          
         MVI   ROUTNUM,9            CHECK DATE WITHIN ESTIMATE                  
         CLC   RENDD,SPACES         YYMM AND NO END ENTERED?                    
         BNE   SKPDTCK              NO                                          
         CLC   RSTRD+2(2),=C'13'   13TH MONTH?                                  
         BE    SKPDTCK                                                          
         CLC   RSTRD+4(2),SPACES    ,,IF YYMM                                   
         BNE   SKPDTCK                                                          
         MVC   RSTRD+4(2),=C'14'    ,,SET MIDDLE OF MONTH                       
         LA    R1,RSTRD                                                         
         ST    R1,PLIST                                                         
         MVI   PLIST,1                                                          
         BAS   RE,DATBROAD                                                      
         MVC   SPTREC(12),SPTWORK     SPTREC -> BROADCST START/END              
         MVC   RSTRD+4(2),SPACES      RESET SPACES                              
         CLI   ESTDATES,0                                                       
         BE    SKPDTCK                                                          
         LA    R1,ESTDATES         START VS REQ DATE                            
         ST    R1,PLIST                                                         
         MVI   PLIST,1                                                          
         BAS   RE,DATBROAD                                                      
         CLC   SPTREC+6(6),SPTWORK  REQ END - EST START                         
         BL    ESTDATER                                                         
         LA    R1,ESTDATES+6                                                    
         ST    R1,PLIST                                                         
         MVI   PLIST,1                                                          
         BAS   RE,DATBROAD                                                      
         CLC   SPTREC(6),SPTWORK+6  REQ START - EST END                         
         BNH   SKPDTCK                                                          
ESTDATER MVC   FERN,=AL2(79)   DATES NOT IN EST                                 
         BR    RA                                                               
*                                                                               
DATBROAD NTR1                                                                   
         L     R2,GETDAY                                                        
         L     R4,ADDAY                                                         
         GOTO1 GETBROAD,PLIST,,SPTWORK,(R2),(R4)                                
         CLI   PLIST,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
SKPDTCK  EQU   *                                                                
*        CLC   RNUM,=C'BZ'          IF BZ                                       
*        BE    CHKBR1               CHECK B1R PROFILE                           
*        CLC   RNUM,=C'DZ'          IF DZ                                       
*        BNE   CHKV196                                                          
*HKBR1   XC    TEMP,TEMP                                                        
*        MVC   TEMP+30(4),=C'SB1R'                                              
*        NI    TEMP+30,X'BF'       LOWER CASE = PROG IS 3 CHAR LONG             
*        MVC   TEMP+34(6),RAGY       AGY/MED/CLT                                
*        L     R5,DATAMGR                                                       
*        BAS   R4,GTPROF                                                        
*        CLI   TEMP+5,C'Y'                                                      
*        BNE   NOTOKB1R                                                         
*        XC    TEMP,TEMP           ALSO CHECK B4 ETC PROFILE                    
*        MVI   HALF,C'B'                                                        
*        MVC   HALF+1(1),RO2                                                    
*        BAS   RE,RDPROF                                                        
*        CLI   TEMP+9,C'N'         PRIO MONTH MUST = N                          
*        BE    VRBU00                                                           
*        DS    0H                                                               
*        MVC   FERN,=AL2(FE)          SET MY MESSAGE                            
*        XC    BVRHDR,BVRHDR                                                    
*        MVC   BVRHDR(30),=C'*** ERROR - PRIOR MONTH OPTION'                    
*        BR    RA                                                               
*                                                                               
*OTOKB1R MVC   FERN,=AL2(FE)          SET MY MESSAGE                            
*        XC    BVRHDR,BVRHDR                                                    
*        MVC   BVRHDR(34),=C'*** ERROR - INVALID REPORT REQUEST'                
*        BR    RA                                                               
                                                                                
CHKV196  DS    0H                                                               
*                                                                               
*                                                                               
         DS    0H                  PROFILE B4,B5 ETC OR BN                      
         MVI   HALF,C'B'           ...B4/5/6/7                                  
         MVC   HALF+1(1),RO2                                                    
         BAS   RE,RDPROF                                                        
         OC    TEMP(20),TEMP                                                    
         BZ    VRBU00F                                                          
*&&DO                                                                           
         CLI   RO2,C'4'            FOR B4 PROFILE                               
         BNE   VRBU00D                                                          
         CLC   =C'H9',RAGY          FOR FOLLOWING P&G CLIENTS                   
         BNE   VRBU00B2                                                         
         CLC   =C'PGB',RCLI         PRINT UDEF NEXT TO PRD NAME                 
         BE    VRBU00C                                                          
         CLC   =C'HPG',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'HP1',RCLI                                                     
         BE    VRBU00C                                                          
*                                                                               
VRBU00B2 CLC   =C'DU',RAGY                                                      
         BNE   VRBU00D                                                          
         CLC   =C'PG1',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'PG2',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'PG3',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'PG4',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'PG5',RCLI                                                     
         BE    VRBU00C                                                          
         CLC   =C'PGG',RCLI                                                     
         BNE   VRBU00D                                                          
VRBU00C  CLI   TEMP+1,C'T'         PRODUCTS HAVE TO BE TOGETHER                 
         BE    VRBU00D                                                          
         MVC   FERN,=AL2(PRDTOG)                                                
         BR    RA                                                               
*&&                                                                             
VRBU00D  MVC   CLCOPT2,TEMP+9          PASS PRIOR MONTH OTION                   
*                                                                               
VRBU00F  DS    0H                                                               
*                                                                               
         CLC   RNUM,=C'DU'         FOR LIVE BU                                  
         BE    VRBU00K                                                          
         CLC   RNUM,=C'D1'         FOR LIVE B1                                  
         BE    VRBU00K                                                          
*******  CLC   BVROUT(4),=C'SOON'  BUT NOT FOR LB                               
*******  BE    VRBU00K                                                          
         MVC   HALF,=C'BT'         CHECK IF BT NOT INV REG?                     
         BAS   RE,RDPROF                                                        
         CLI   TEMP+2,C'Y'                                                      
         BNE   *+8                                                              
         MVI   R2USER+28,C'B'                                                   
*                                                                               
VRBU00K  CLC   RNUM,=C'B1'          IF B1                                       
         BE    VRBU10                                                           
         CLC   RNUM,=C'D1'          OR D1                                       
         BE    VRBU10                                                           
*                                                                               
*                                                                               
         CLC   =C'MC',RAGY         FOR MC ONLY OFFICE J CAN DO                  
         BNE   VRBU00L             SUBMEDIA O REQUESTS                          
         CLI   RO5,C'O'                                                         
         BNE   VRBU00L                                                          
         CLI   CLIOFFC,C'J'                                                     
         BE    VRBU00L                                                          
         MVI   ROUTNUM,X'71'       CURSOR TO NETWORK FIELD                      
         MVC   FERN,=AL2(FLDINV)   INVALID                                      
         BR    RA                                                               
*                                                                               
*                                                                               
VRBU00L  CLI   RDIV,C'N'           PRD GROUP SCHEME CAN'T BE 'N'                
         BNE   *+16                                                             
         MVI   ROUTNUM,X'5D'       CURSOR TO PRD GROUP FIELD                    
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         BR    RA                                                               
*                                                                               
         CLI   RO5,C'*'            FOR 'ALL' SUBMEDIA AND CURR=FLIGHT           
         BNE   *+12                                                             
         MVI   ROUTNUM,X'71'       CURSOR TO NETWORK FIELD                      
         B     *+16                                                             
*                                                                               
         CLI   R2USER+29,C'C'                                                   
         BL    VRBU00M                                                          
         MVI   ROUTNUM,X'CB'       CURSOR TO OPTIONS FIELD                      
*                                                                               
         CLC   =C'SOON',BVROUT     CANNOT REQUEST SOON                          
         BE    *+12                                                             
         CLI   RBOOK,C' '          ANYTHING IN AMOUNT FIELD ?                   
         BNH   VRBU00M                                                          
*                                                                               
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         BR    RA                                                               
*                                                                               
VRBU00M  XC    TEMP(20),TEMP                                                    
         MVC   TEMP+30(4),=C'SB4N'     B4N - B7N PROFILE READ                   
         MVC   TEMP+32(1),RO2          4 TROUGH 7 BILL                          
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         CLI   RO5,C' '            ANY SUBMEDIA ?                               
         BNH   *+10                                                             
         MVC   TEMP+36(1),RO5                                                   
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         XC    TEMP+30(20),TEMP+30                                              
         OC    TEMP(16),TEMP                                                    
         BZ    VRBU01              CHECK IF EXISTS                              
         CLI   TEMP+15,C'Y'        IGNORE THIS PROFILE ?                        
         BNE   *+14                NO, USE IT                                   
VRBU01   MVC   HALF,=C'BN'         ...ELSE USE BN PROF                          
         BAS   RE,RDPROF                                                        
*                                                                               
         CLC   RAGY,=C'TH'         TEMP CODE FOR SAATCHI                        
         BNE   VRBU01A                                                          
         CLC   RCLI,=C'LDL'        FOR CLT LDL                                  
         BNE   VRBU01A                                                          
         CLC   RPRO,=C'CO '        PRODUCT CO                                   
         BNE   VRBU01A                                                          
         CLC   REST,=C'036'        EST 036 OR                                   
         BE    *+14                                                             
         CLC   REST,=C'044'        EST 044                                      
         BNE   VRBU01A                                                          
*                                                                               
         MVC   FERN,=AL2(FE)       SET MY MESSAGE                               
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(37),=C'BILLING RECORDS ARE FULL, CONTACT DDS'             
         BR    RA                                                               
*        CLC   RNUM,=C'BU'         FOR LIVE MANUAL BILL                         
*        BNE   VRBU01A             GET NOTIFICATION                             
*        CLC   RAGY,=C'MC'         TEMP CODE FOR MC                             
*        BNE   VRBU01A                                                          
*        LA    RF,EMSG                                                          
*        MVC   45(3,RF),RCLI                                                    
*        MVC   53(3,RF),RPRO                                                    
*        MVC   62(3,RF),REST                                                    
*        GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'EMSG,EMSG)                             
VRBU01A  CLI   RBOOK,C'G'          IS THIS MANUAL BILL?                         
         BNE   VRBU01B                                                          
         CLI   TEMP+11,C'Y'        IS STATION TYPE REQUIRED ?                   
         BE    VRBU01B                                                          
         CLI   RO5,C' '            ANY SUBMEDIA?                                
         BNH   VRBU01B                                                          
         MVI   ROUTNUM,X'71'       CURSOR TO NETWORK FIELD                      
         MVC   FERN,=AL2(NOSUBM)         INVALID                                
         BR    RA                                                               
VRBU01B  CLI   RO1,C'3'                                                         
         BE    VRBU01C                                                          
         CLI   RO1,X'40'                                                        
         BH    VRBU01F                                                          
         CLI   TEMP+10,0           IF BINARY ZERO PUT IN DEFAULT                
         BNE   *+12                                                             
         MVI   RO1,C'3'                                                         
         B     VRBU01C                                                          
*???     CLI   TEMP+10,X'40'       IF RDPROF RETURNS BLANK                      
*???     BNH   VRBU01F             LEAVE BLANK                                  
         MVC   RO1,TEMP+10                                                      
         CLI   RO1,C'3'                                                         
         BNE   VRBU01F                                                          
VRBU01C  CLC   TEMP+8(1),TEMP+9                                                 
         BE    VRBU01F                                                          
         MVI   ROUTNUM,X'CC'                                                    
         MVC   FERN,=AL2(95)                                                    
         BR    RA                                                               
VRBU01F  DS    0H                                                               
*        CLI   TEMP+3,C'N'         BILLING ASSIGNED ?                           
*        BNE   VRBU01P             NO NXT CHECK                                 
*        TM    ODDMNTS,X'38'       IS ECOST SET AT CLT LEVEL?                   
*        BNZ   *+16                YES, GOOD                                    
*        MVI   ROUTNUM,X'02'       NO, ERROR                                    
*        MVC   FERN,=AL2(ECMISS)                                                
*        BR    RA                                                               
*                                                                               
         TM    ODDMNTS,ECINT       INTEGRATION IN ECOST ?                       
         BO    *+12                                                             
         TM    ODDMNTS,ECINTSP     INT + SPECIALS ?                             
         BZ    VRBU01P             NO, DONE                                     
         CLI   RO1,C'I'            INTEGRATION, INVALID                         
         BE    VRBU01K                                                          
         CLI   RO1,C'1'            T+I, INVALID                                 
         BE    VRBU01K                                                          
         CLI   RO1,C'3'            T+I+SPECIALS, INVALID                        
         BE    VRBU01K                                                          
         TM    ODDMNTS,ECINTSP     SPECIALS IN ECOST ?                          
         BZ    VRBU01P                                                          
         CLI   RO1,C'T'            ONLY TIME, VALID                             
         BE    VRBU01P                                                          
         CLI   RO1,C'I'            ONLY INTEG, VALID                            
         BE    VRBU01P                                                          
         CLI   RO1,C'1'            T+I, VALID                                   
         BE    VRBU01P             THE REST INVALID                             
VRBU01K  MVI   ROUTNUM,X'CC'       NO, ERROR                                    
         MVC   FERN,=AL2(ECMISM)                                                
         BR    RA                                                               
VRBU01P  CLI   TEMP+11,C'Y'                                                     
         BE    VRBU01R                                                          
         CLC   =C'MC',RAGY         FOR MCCANN'S L'OREAL CLIENTS                 
         BNE   VRBU02              MAKE SURE S-MED IS THERE                     
         CLC   =C'LRH',RCLI                                                     
         BE    VRBU01R                                                          
         CLC   =C'LCP',RCLI                                                     
         BE    VRBU01R                                                          
         CLC   =C'LCH',RCLI                                                     
         BE    VRBU01R                                                          
         CLC   =C'COS',RCLI                                                     
         BE    VRBU01R                                                          
         CLC   =C'CO2',RCLI                                                     
         BE    VRBU01R                                                          
         CLC   =C'CSH',RCLI                                                     
         BNE   VRBU02                                                           
*                                                                               
*   NOTE; WHEN ADDING CLIENTS HERE BE SURE TO UPDATE NETWORK BILLING            
*         SPECIAL FEATURE FOR L'OREAL                                           
*                                                                               
VRBU01R  CLI   RO5,X'40'           AND NO STATION TYPE                          
         BH    VRBU02                                                           
         CLI   RSTA,X'40'          BUT STATION=BLANKS(ALL)                      
         BH    VRBU02                                                           
         MVI   ROUTNUM,X'71'       NEEDS STATION TYPE (ALL,C/S/N<)              
*        MVI   FERN,FLDINV                                                      
         MVC   FERN,=AL2(FE)          SET MY MESSAGE                            
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(34),=C'*** ERROR - BN PROF REQUIRES INPUT'                
         BR    RA                                                               
*                                                                               
VRBU02   DS    0H                  IF DOING 2 BILLFORMS, CHECK T/I SEP          
         CLI   TEMP+8,C'S'         TEMP STILL HAS BN PROFILE                    
         BE    VRBU10              IF SET SEP ON BN, DON'T CHK B4A              
         MVC   HALF(1),TEMP+8      STORE TIME/INT OPTION                        
         MVC   TEMP+30(4),=C'SB4A'        B4A PROFILE READ                      
         MVC   TEMP+32(1),RO2      4 TROUGH 7 BILL                              
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         CLI   TEMP+10,C'Y'        USING 2 BILLING FORMULAS ?                   
         BNE   VRBU10              NO, DONE                                     
         CLI   RO1,C'T'            IS IT FOR TIME OR INTEGRATION ?              
         BE    VRBU10              YES, GOOD                                    
         CLI   RO1,C'I'                                                         
         BE    VRBU10                                                           
         CLI   HALF,C'*'           IF N OR T -- ERROR                           
         BH    *+22                                                             
         MVC   HALF,=C'B2'         CHECK INTEG SEPARATE OPTION ON B2            
         BAS   RE,RDPROF                                                        
         CLI   TEMP+9,C'S'         IF SEPARATE - GOOD, IF NOT - ERROR           
         BE    VRBU10                                                           
*                                                                               
         MVI   ROUTNUM,X'CC'                                                    
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(52),=C'IF USING 2 BILL FORMULA, TIME/INTEG MUST B+        
               E SEPARATE'                                                      
         MVC   FERN,=AL2(FE)       MY OWN ERROR MESSAGE                         
         BR    RA                                                               
*                                                                               
VRBU10   DS    0H                                                               
*                                                                               
         CLC   RNUM,=C'BU'          .IF BU                                      
         BE    *+14                                                             
         CLC   RNUM,=C'B1'          .IF B1                                      
         BNE   VR196AA                                                          
*                                                                               
         CLC   =C'SOON',BVROUT       AND SOON (LB)                              
         BNE   VR196AA                                                          
         MVI   ROUTNUM,2                                                        
         CLC   =C'ALL',RCLI        ,,CLI=ALL IS VERBOTEN                        
         BE    VALINV                                                           
*                                                                               
VR196AA  CLC   RNUM,=C'D1'          .IF D1                                      
         BE    *+14                                                             
         CLC   RNUM,=C'B1'          .IF B1                                      
         BNE   VR196A                                                           
********************************** IF B1/D1 AND RNAME=PW                        
********************************** AND EST=ALL/ERROR                            
         MVI   ROUTNUM,5                                                        
         CLC   =C'WI',RAGY                                                      
         BNE   NOTPW                                                            
         CLC   =C'PW',RNAME                                                     
         BNE   NOTPW                                                            
         CLC   =C'ALL',REST                                                     
         BE    VALINV                                                           
NOTPW    EQU   *                                                                
******************************************************                          
         BAS   RE,B1XPROF         B1X PROFILE CHECK                             
         BNE   B1XERR                                                           
*                                                                               
         CLC   RNUM,=C'B1'          ...IF B1                                    
         BNE   VR196A                                                           
         BAS   RE,B1XPROF         B1X PROFILE CHECK                             
         BNE   B1XERR                                                           
         CLI   RO6,C'2'             ...AND REQUESTING A2 OR AB                  
         BE    *+12                                                             
         CLI   RO6,C'B'                                                         
         BNE   VR196A                                                           
         CLC   REST(2),=C'NO'       ...EST CAN NOT BE NO                        
         BNE   VR196A                                                           
         MVI   ROUTNUM,5                                                        
         MVC   FERN,=AL2(8)                                                     
         BR    RA                                                               
VR196A   BAS   RE,VALACC                                                        
         CLC   =C'D1',RNUM         FOR D1                                       
         BE    V196RFP                                                          
         CLC   =C'DU',RNUM         FOR DU                                       
         BE    V196RFP                                                          
         CLC   =C'B1',RNUM         FOR B1                                       
         BE    V196RFP                                                          
         CLC   =C'BU',RNUM         FOR BU                                       
         BNE   *+16                                                             
*V196RFP  LA    R5,RREPNO           SEE IF RFP SYMBOLIC NAME                    
V196RFP  LA    R5,R2USER           SEE IF RFP SYMBOLIC NAME                     
         BRAS  RE,VALRFP                                                        
         BE    VR196B                                                           
*                                                                               
         BAS   RE,CKINVD1          CHK INVOICE DATE COL 30                      
         CLI   ESTDATES,0                                                       
         BE    VR196B                                                           
         LA    R5,RSTRD                                                         
         BRAS  RE,VALRFP                                                        
         BE    VR196B                                                           
         CLC   RSTRD+2(2),=C'13'   13TH MONTH?                                  
         BE    VR196B              YES, SKIP CHECK                              
         CLC   RSTRD(4),ESTDATES                                                
         BNL   VR196B                                                           
         MVI   ROUTNUM,9                                                        
         MVC   FERN,=AL2(MOSERR)                                                
         BR    RA                                                               
B1XERR   MVI   ROUTNUM,X'73'                                                    
         MVC   FERN,=AL2(646)                                                   
         BR    RA                                                               
*                                                                               
VR196B   DS    0H                                                               
         CLI   RBOOK,C'G'          SEE IF MANUAL AMT INPUT                      
         BNE   VR196F                                                           
         MVI   ROUTNUM,X'5E'       CURSOR TO MKT                                
         TM    MKTSAVE,X'FC'       MKT MUST BE ALL OR BLANK                     
         BNZ   VR196ME                                                          
         MVI   ROUTNUM,X'72'       CURSOR TO AMOUNT                             
         TM    PROSAVE,X'12'       PRD CAN'T BE ALL OR NO                       
         BNZ   VR196E                                                           
         TM    ESTSAVE,X'12'       EST CAN'T BE ALL OR NO                       
         BZ    VR196F                                                           
VR196E   MVC   FERN,=AL2(AMTPRO)                                                
         B     VR196XX                                                          
VR196ME  MVC   FERN,=AL2(MKTALL)                                                
         B     VR196XX                                                          
*                                                                               
VR196F   MVC   TEMP+30(4),=C'SB2B'        B2B PROFILE READ                      
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
B2B5     L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         XC    TEMP+100(2),TEMP+100                                             
         CLI   TEMP+5,C'%'         OVERRIDING B2 PROFILE?                       
         BNH   B2B8                NO, LEAVE ALONE                              
         CLI   TEMP+6,C'%'                                                      
         BNH   B2B8                                                             
         MVC   TEMP+100(2),TEMP+5                                               
         LARL  RF,SPMGRTAB                                                      
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   TEMP+5(2),0(RF)                                                  
         BE    B2B6                                                             
         AHI   RF,L'SPMGRTAB                                                    
         BCT   RE,*-14                                                          
         MVI   ROUTNUM,X'5E'       CURSOR TO MGRP                               
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         BR    RA                                                               
B2B6     CLC   RDIST,2(RF)         ELSE MUST MATCH                              
         BE    B2B8                                                             
         MVI   ROUTNUM,X'5E'       CURSOR TO MGRP                               
         B     VALPROFE                                                         
*                                                                               
*                                                                               
B2B8     CLI   TEMP,C'Y'           SEPARATE COMMISION BILLING                   
         BNE   VR191F                                                           
         CLI   RO6,C'C'                                                         
         BE    B2B10                                                            
         CLI   RO6,C'N'                                                         
         BE    B2B10                                                            
         CLI   RO6,C'R'                                                         
         BE    B2B10                                                            
         MVI   ROUTNUM,X'CB'       CURSOR TO OPTIONS FIELD                      
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         BR    RA                                                               
B2B10    CLI   TEMP+2,0            EST FILTER POSITION 0,1-3                    
         BE    VR191F                                                           
* - MORE HERE LATER FOR B2B                                                     
VR191F   MVC   HALF,=C'B2'                                                      
         BAS   RE,RDPROF                                                        
         OC    TEMP(16),TEMP                                                    
         BNZ   VR196F2                                                          
         MVI   ROUTNUM,2           CURSOR TO CLIENT                             
         MVC   FERN,=AL2(NOPROF)         NO PROFILE                             
         BR    RA                                                               
*                                                                               
VR196F2  CLI   TEMP+1,C'*'                                                      
         BE    VR196G              ANYTHING OK FOR PGRP                         
         CLC   RDIV,TEMP+1         ELSE SCHEME MUST MATCH                       
         BE    VR196G                                                           
         MVI   ROUTNUM,X'5D'         CURSOR TO PGRP                             
         B     VALPROFE                                                         
*                                                                               
VR196G   DS    0H                                                               
         CLI   RBOOK,C'G'          SEE IF MANUAL AMT INPUT                      
         BE    VR196H              YES - BYPASS MKTGRP TEST                     
         CLI   TEMP+100,0          DID WE DO 2 CHAR OVERRIDE?                   
         BNZ   VR196H              YES, DON'T CHK 1 CHAR                        
         CLI   TEMP+2,C'*'                                                      
         BE    VR196H              ANYTHING ALLOWED FOR MGRP                    
         CLC   RDIST,TEMP+2        ELSE MUST MATCH                              
         BE    VR196H                                                           
         MVI   ROUTNUM,X'5E'       CURSOR TO MGRP                               
         B     VALPROFE                                                         
*                                                                               
VR196H   CLI   TEMP+8,C'0'         ALL TYPES ARE OK                             
         BL    VR196I                                                           
         CLC   RO2,TEMP+8          CHECK BILLING TYPE                           
         BNH   VR196I                                                           
         MVI   ROUTNUM,X'73'         CURSOR TO BILLING TYPE                     
         B     VALPROFE                                                         
*                                                                               
VR196I   DS    0H                                                               
*        CLC   TEMP+6(1),RO2       IF PROF+6 = BILLING TYPE (OPT2)              
*        BE    VR196J              ACCEPT NEGATIVE MANUAL AMTS                  
*        CLI   R5055+2,C'-'        ELSE DON'T                                   
*        BNE   VR196J                                                           
*        MVI   ROUTNUM,X'72'       CURSOR TO AMT                                
*        MVI   FERN,FLDINV                                                      
*        BR    RA                                                               
*                                                                               
VR196J   OC    TEMP+14(2),TEMP+14     CHK MOS DATE                              
         BZ    VR196JE                                                          
         CLC   RENDD,SPACES         SEE IF END DATE SPECIFIED                   
         BNE   VR196K              YES - BYPASS DATE CHECK                      
         LA    R5,RSTRD                                                         
         BRAS  RE,VALRFP                                                        
         BE    VR196K                                                           
         MVC   TEMP+30(4),RSTRD                                                 
         MVC   TEMP+34(2),=C'01'       SET DAY TO 01                            
         GOTO1 DATCON,PLIST,(0,TEMP+30),(3,TEMP+20)                             
         CLC   TEMP+20(2),TEMP+14                                               
         BNL   VR196K                                                           
*                                                                               
VR196JE  MVI   ROUTNUM,X'09'       CURSOR TO DATE                               
         B     VALPROFE                                                         
*                                                                               
VR196K   DS    0H                  CHECK GM CGR CONDITIONS                      
         CLC   RAGY,=C'GZ'                                                      
         BE    *+14                                                             
         CLC   RAGY,=C'*B'         AND DDSB FOR NOW                             
         BNE   VR196L                                                           
         CLC   RNUM,=C'B1'         FOR SPOT                                     
         BE    *+14                                                             
         CLC   RNUM,=C'D1'                                                      
         BNE   VR196L                                                           
*                                                                               
         TM    CLISAVE,X'04'       SINGLE CLIENT ?                              
         BZ    VR196K4                                                          
         TM    DEMS,X'10'          LMG CLT ?                                    
         BZ    VR196K2                                                          
         CLI   R2USER+32,C' '      DID THEY USE GMBILL                          
         BH    VR196K7                                                          
         MVI   ROUTNUM,X'CB'       NO,CURSOR TO OPTIONS FIELD                   
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         B     VR196XX                                                          
*                                                                               
VR196K2  CLI   R2USER+32,C' '      NON LMG CLIENT USED GMBILL OPTION?           
         BNH   VR196L              ALL CLEAR, DONE                              
         MVI   ROUTNUM,X'CB'       CURSOR TO OPTIONS FIELD                      
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         B     VR196XX                                                          
*                                                                               
VR196K4  CLI   RCARD2+4,C' '       CLIENT GROUP REQUEST?                        
         BNH   VR196K7                                                          
         BRAS  RE,VALGMCGR         YES, CHECK IT                                
         BE    *+18                                                             
         MVI   ROUTNUM,X'02'                                                    
         MVC   FERN,=AL2(GMCGRERR)                                              
         B     VR196XX                                                          
*                                                                               
VR196K7  DS    0H                                                               
         CLI   R2USER+32,C'L'      LMG REQUEST ?                                
         BNE   *+16                                                             
         CLI   RO2,C'7'            SHOULD BE TYPE 7                             
         BE    VR196L                                                           
         B     VR196KE                                                          
*                                                                               
         CLI   R2USER+32,C'R'      REG REQUEST ?                                
         BNE   VR196L                                                           
         CLI   RO2,C'4'            SHOULD BE TYPE 4                             
         BE    VR196L                                                           
*                                                                               
VR196KE  MVI   ROUTNUM,X'73'       INVALID BILL TYPE                            
         MVC   FERN,=AL2(FLDINV)                                                
         B     VR196XX                                                          
*                                                                               
VR196L   DS    0H                                                               
         CLC   RAGY,=C'M2'         FOR WB PROD ALL NOT ALLOWED                  
         BE    VR196L2                                                          
         CLC   RAGY,=C'W+'                                                      
         BE    VR196L2                                                          
         CLC   RAGY,=C'FR'                                                      
         BE    VR196L2                                                          
         CLC   RAGY,=C'*B'                                                      
         BNE   VR196M                                                           
         CLC   =C'WB',RCLI                                                      
         BE    VR196L2X                                                         
         B     VR196M                                                           
VR196L2  CLC   =C'WB8',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'WBH',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'WA9',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'WB9',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'HW9',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'WHH',RCLI                                                     
         BE    VR196L2X                                                         
         CLC   =C'HW8',RCLI                                                     
         BNE   VR196M                                                           
VR196L2X CLC   =C'ALL',RPRO                                                     
         BNE   VR196L3                                                          
         MVI   ROUTNUM,X'5D'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         B     VR196XX                                                          
*                                                                               
VR196L3  CLC   RNUM,=C'B1'                                                      
         BE    VR196M                                                           
         CLC   RNUM,=C'D1'                                                      
         BE    VR196M                                                           
         CLI   RBOOK,C'G'          FOR NET BILLING CHECK THAT                   
         BNE   VR196M              MANUAL BILL HAS FLIGHT                       
         TM    DEMS+1,X'20'+X'10'  ONLY FOR THTR OR THNT PRODS                  
         BZ    VR196M                                                           
         CLI   R2USER+33,C' '                                                   
         BH    VR196M                                                           
         MVI   ROUTNUM,X'CB'       CURSOR TO OPTIONS FIELD                      
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         B     VR196XX                                                          
*                                                                               
VR196M   DS    0H                                                               
         CLC   RNUM,=C'B1'                                                      
         BNE   *+18                                                             
         CLC   =C'SOON',BVROUT     ,,IF B1 AND SOON (LB)                        
         BE    VR196N              DISALLOW CASH/TRADE OPTION                   
         B     *+14                                                             
*                                                                               
         CLC   RNUM,=C'D1'                                                      
         BNE   VR196N                                                           
*                                                                               
         TM    CLISAVE,X'80'       IF GMI CLIENT                                
         BZ    VR196N                                                           
         CLI   RCARD2+31,C'T'      IF OPTION 'TRADE' WAS USED ON SOON           
         BE    VR196P              THEN F-RATE OPTION NOT ALLOWED               
         MVI   RCARD2+31,C'C'      CASH/TRA SEPARATE                            
         B     VR196P                                                           
*                                                                               
*                                  F-RATE OPTION                                
VR196N   CLC   RNUM,=C'B1'                                                      
         BNE   *+18                                                             
         CLC   =C'SOON',BVROUT     ,,IF B1 AND SOON (LB)                        
         BE    VR196P              DISALLOW F-RATE OPTION                       
         B     *+14                                                             
*                                                                               
         CLC   RNUM,=C'D1'                                                      
         BNE   VR196P                                                           
*                                  F-RATE OPTION                                
         CLC   RAGY,=C'MC'         FOR MCCAN GM DON'T DO IT FOR                 
         BNE   VR196NA             CLT GRP SCHEEMS Y & Z                        
         TM    CLISAVE,X'40'       IS IT CGROUP REQUEST                         
         BZ    VR196NA                                                          
         CLI   RCARD2+4,C'Y'       IF IT'S Y OR Z CHECK EFFECTIVE DATE          
         BE    *+12                                                             
         CLI   RCARD2+4,C'Z'                                                    
         BNE   VR196NA                                                          
*                                  F-RATE OPTION                                
         CLC   RSTRD(4),=X'FAF4F0F1' IF BEFORE EFFECT DATE, IGNORE PROF         
         BL    VR196NJ                                                          
*                                  F-RATE OPTION                                
VR196NA  MVC   TEMP+30(4),=C'SB4A'        B4A PROFILE READ                      
         MVC   TEMP+32(1),RO2      4 TROUGH 7 BILL                              
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
VR196NB  L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         CLI   TEMP+11,C'F'        ARE WE DOING F-RATES?                        
         BNE   VR196NJ                                                          
         CLI   RCARD2+30,C' '      ANYTHING IN THAT FIELD ALREADY?              
         BNH   *+14                                                             
         CLI   RCARD2+30,C'F'      HAS TO BE  'F'                               
         BE    VR196P                                                           
         DC    H'0'                                                             
*                                                                               
         MVI   RCARD2+30,X'86'     PUT IN LOWER CASE 'F'                        
         B     VR196P                                                           
*                                                                               
VR196NJ  CLI   RCARD2+30,C' '      ANYTHING IN THAT FIELD ALREADY?              
         BNH   VR196P                                                           
         MVI   ROUTNUM,X'CB'       CURSOR TO OPTIONS FIELD                      
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         B     VR196XX                                                          
*                                                                               
VR196P   CLC   RNUM,=C'B1'         MTRADE OPTION                                
         BE    *+14                                                             
         CLC   RNUM,=C'D1'                                                      
         BNE   VR196Q                                                           
*                                                                               
         MVC   TEMP+30(4),=C'SB1S'        B1S PROFILE READ                      
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         CLI   TEMP+6,C'Y'         ARE WE DOING MTRADE?                         
         BE    VR196PJ                                                          
         CLI   R2USER+20,C' '      NO,                                          
         BNH   VR196Q              AND NOTHING IN THE OPTION, DONE              
         CLI   R2USER+20,C'T'      DID WE USE MTRADE OPTION?                    
         BNE   *+18                NO, CLEAR THE FIELD                          
         MVI   ROUTNUM,X'CB'       YES,CURSOR TO OPTIONS FIELD                  
         MVC   FERN,=AL2(FLDINV)         INVALID                                
         B     VR196XX                                                          
*                                                                               
         MVI   R2USER+20,C' '      CLEAR THE FIELD OTHERWISE                    
*                                                                               
VR196PJ  DS   0H                   THE PROFILE IS ON                            
         CLI   R2USER+20,C' '      ANYTHING IN THE FIELD?                       
         BNH   VR196Q              AND NOTHING IN THE OPTION, DONE              
         CLI   RCARD2+31,C' '      USING GMI TRADE?                             
         BNH   *+14                                                             
         MVC   FERN,=AL2(TRDMIX)   ERROR, MIX OF TRADE OPTIONS                  
         B     VR196XX                                                          
*                                                                               
         CLI   RCARD2+30,C' '      F-RATE?                                      
         BNH   *+14                                                             
         MVC   FERN,=AL2(TRDFRT)   ERROR, MIX OF TRADE AND F-RATE               
         B     VR196XX                                                          
*                                                                               
         CLI   DDS,1               FOR DDS TERMINAL DON'T CHECK                 
         BE    VR196Q                                                           
         CLC   RNUM,=C'B1'         CHECK IF LIVE SOON                           
         BNE   VR196Q              NO, DONE                                     
         CLC   =C'SOON',BVROUT     ,,IF B1 AND SOON (LB)                        
         BNE   VR196Q              NO, DONE                                     
         MVI   ROUTNUM,X'02'                                                    
         MVC   FERN,=AL2(TRDERR)   NOT ALLOWED                                  
         B     VR196XX                                                          
*                                                                               
VR196Q   CLC   RNUM,=C'D1'         IREG OPTION                                  
         BE    *+14                                                             
         CLC   RNUM,=C'DU'         SPOT/NET DRAFT BILLING                       
         BNE   VR196XX                                                          
         BRAS  RE,B8PROF                                                        
         BNE   VR196XX                                                          
         MVI   R2USER+28,C'R'                                                   
*                                                                               
*                                                                               
VR196XX  BR    RA                                                               
*                                                                               
         EJECT                                                                  
B1XPROF  NTR1                                                                   
         MVC   TEMP+30(4),=C'SB1X'        B1X PROFILE READ                      
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
B1X5     L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
         CLI   TEMP+12,C'Y'        ...IF Y                                      
         BNE   B1XOK                                                            
         MVI   HALF,C'B'                                                        
         MVC   HALF+1(1),RO2       ...READ REQUESTED TYPE                       
         BAS   RE,RDPROF                                                        
         LA    R5,TEMP+9                                                        
         LA    R4,B1XTABLE                                                      
         LA    R1,6                                                             
B1X10    CLC   0(1,R5),0(R4)                                                    
         BE    B1XOK                                                            
         LA    R4,1(R4)                                                         
         BCT   R1,B1X10                                                         
         LA    R1,1                                                             
         B     *+8                                                              
B1XOK    SR    R1,R1                                                            
         LTR   R1,R1                                                            
BIXX     XIT1                                                                   
         SPACE                                                                  
B1XTABLE DC    C'NSU123'                                                        
         EJECT                                                                  
VALR199  CLC   =C'SOON',BVROUT     IF SOON                                      
         BNE   V199B                                                            
         CLI   RO1,C'Y'            RO1=Y                                        
         BE    V199B                                                            
         MVI   ROUTNUM,X'74'                                                    
         B     VALINV                                                           
V199B    CLC   RO2(2),=C'YY'                                                    
         BNE   VR199X                                                           
         MVI   ROUTNUM,X'76'                                                    
         MVC   FERN,=AL2(2)                                                     
*                                                                               
VR199X   BR    RA                                                               
         SPACE 2                                                                
VALR202  DS    0H                                                               
         BAS   RE,VALACC                                                        
VR202X   BR    RA                                                               
*                                                                               
VALR210  DS    0H                                                               
         BRAS  RE,VVALR210                                                      
         BR    RA                                                               
*                                                                               
VALR205  DS    0H                                                               
         BRAS  RE,VVALR205                                                      
         BR    RA                                                               
*                                                                               
         EJECT                                                                  
* K4 K5 KL ALLOW UPDATIVE SOON                                                  
VALR211  DS    0H                                                               
         CLI   RO4,C'Y'            CHECK IF TEST RUN                            
         BE    VR211               IF YES, DON'T CHECK FOR SOX                  
         BRAS  RE,CKUPDT                                                        
         BNE   VR211                                                            
         MVI   ROUTNUM,X'74'                                                    
         MVC   FERN,=AL2(1250)                                                  
         BR    RA                                                               
VR211    CLC   =C'KL',RNUM                                                      
         BNE   SKIPTHIS                                                         
         CLC   =C'0000',RMARK                                                   
         BNE   SKIPTHIS                                                         
         MVI   ROUTNUM,X'5E'                                                    
         MVC   FERN,=AL2(789)                                                   
         BR    RA                                                               
SKIPTHIS L     RF,APARM            ..IF SPOT                                    
         L     RF,16(RF)                                                        
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,2           2=SPOT,3=NET                                 
         BE    VR211A                                                           
* IF NETWORK / K5,K4 COULD BE PASSWORD PROTECTED                                
*****************************************************************               
         CLC   RNUM,=C'KL'         IF KL,SKIP THIS                              
         BE    IDX                                                              
         TM    FATFLAG,X'08'       PASSWORD PROTECT ACTIVE?                     
         BZ    IDX                                                              
         OC    FAPASSWD,FAPASSWD   PASSWORD?                                    
         BZ    IDX                                                              
         MVC   RCARD2+32(2),FATAGYSC    PASS SEQURITY AGY NOW                   
         LA    R6,KEY                                                           
         XC    KEY,KEY             13 LONG                                      
         XC    KEYS,KEYS           17 LONG   SA0KEY=25 LENGTH                   
         USING SA0KEY,R6                                                        
         MVI   SA0KTYP,C'0'                                                     
         MVC   SA0KAGY,FATAGYSC                                                 
         OC    SA0KAGY,SA0KAGY                                                  
         BNZ   *+10                                                             
         MVC   SA0KAGY,AGY                                                      
         MVC   SA0KNUM,FAPASSWD                                                 
         GOTO1 DATAMGR,PLIST,(0,=C'DMREAD '),=C'CTFILE ',KEY,SPTREC             
         LA    R6,SPTREC                                                        
         LA    R6,SA0DATA                                                       
ID10     CLI   0(R6),X'C3'                                                      
         BE    ID12                                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         BH    ID13                                                             
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ID13                                                             
         AR    R6,R1                                                            
         B     ID10                                                             
         USING SAPALD,R6                                                        
ID12     MVC   RCARD2+24(8),SAPALPID                                            
         B     IDX                                                              
ID13     MVC   RCARD2+20(8),=C'WHATHPND'                                        
IDX      B     VR211AA                                                          
*****************************************************************               
VR211A   CLC   RMARK,=4X'40'                                                    
         BNE   VR211AA             ..MARKET FIELD IS REQUIRED                   
         MVI   ROUTNUM,X'5E'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR211X                                                           
         DROP  RF                                                               
VR211AA  CLC   =C'SOON,',BVROUT    IF SOON                                      
         BNE   VR211D                                                           
         CLC   RNUM,=C'K4'          IF K4 OR K5 OR KL                           
         BE    VR211AB                                                          
         CLC   RNUM,=C'K5'                                                      
         BE    VR211AB                                                          
         CLC   RNUM,=C'KL'                                                      
         BNE   VR211D                                                           
VR211AB  DS    0H                                                               
*VR211AB  CLI   RO4,C'Y'            IF NOT TEST RUN                             
*         BE    VR211D                                                          
*         CLI   FAOVSYS,2           IF NETWORK                                  
*         BE    VR211BB                (SPOT=ERROR)                             
         DROP  R1                                                               
* - UPDATIVE SOON - CHECK X PROFILE                                             
*         XC    TEMP,TEMP                                                       
*         MVI   TEMP+30,C'S'                                                    
*         MVC   TEMP+31(2),RNUM                                                 
*         MVI   TEMP+33,C'X'        X PROFILE                                   
*         NI    TEMP+30,X'BF'       LOWER CASE                                  
*         MVC   TEMP+34(6),RAGY                                                 
*         CLI   CLIOFFC,C' '                                                    
*         BNH   VR211B                                                          
*         MVI   TEMP+40,C'*'                                                    
*         MVC   TEMP+41(1),CLIOFFC                                              
*VR211B   L     R5,DATAMGR                                                      
*         BAS   R4,GTPROF                                                       
*         CLI   TEMP,C'Y'           UPDATIVE SOON ALLOWED                       
*         BNE   VR211BB                                                         
* - ONLY ONE CLIENT FOR UPDATIVE SOON                                           
         TM    CLISAVE,X'04'                                                    
         BNO   VR211BC                                                          
* - SET CLIENT CODE TO LOCKLST                                                  
         MVC   LOCKLST(3),RCLI                                                  
VR211D   DS    0H                                                               
*                                                                               
         TM    RFPSTAT,RFPINUSE             ARE WE IN $RFP                      
         BO    VR211X                       THEN DON'T CALL PERVERT             
         GOTO1 PERVERT,DMCB,RSTRD,RENDD     LET PERVERT                         
         CLC   8(2,R1),=H'182'     26 WW    DO THE JOB                          
         BNH   VR211X                                                           
         MVI   ROUTNUM,09                                                       
         MVC   FERN,=AL2(SEDBIG)                                                
******>  MVC   HALF,=X'1A01'   26 WEEKS     DOING THIS YOURSELF IS              
******>  BAS   RE,VALMAX                    REDICULOUS                          
VR211X   BR    RA                                                               
*                                                                               
VR211BB  MVI   ROUTNUM,X'74'                                                    
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(37),=C'*** ERROR UPDATIVE SOON NOT VALID ***'             
         MVC   FERN,=AL2(FE)       MY OWN ERROR MESSAGE                         
         BR    RA                                                               
VR211BC  MVI   ROUTNUM,X'02'                                                    
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(41),=C'*** ONLY ONE CLIENT FOR UPDATIVE SOON***'          
         MVC   FERN,=AL2(FE)                                                    
         BR    RA                                                               
         EJECT                                                                  
         SPACE 2                                                                
VALR212  DS    0H                                                               
         BRAS  RE,VVALR212                                                      
         BR    RA                                                               
         SPACE 2                                                                
VALR214  DS    0H                                                               
****     MVC   RPRO,=C'POL'                                                     
         MVC   REST,=C'NO '                                                     
         LA    R6,1                MONDAY                                       
         LA    R5,RSTRD                                                         
         BAS   RE,VALDAY                                                        
         LA    R6,7                SUNDAY                                       
         LA    R5,RENDD                                                         
         BAS   RE,VALDAY                                                        
         MVC   HALF,=X'0C00'       MUST BE 12 MONTHS                            
         BAS   RE,VALMAX                                                        
         CLC   HALF(1),0(R5)       HAS NUMBER OF MNTHS FROM VALMAX              
         BE    VR214B                                                           
         MVC   FERN,=AL2(SEDBIG)                                                
         B     VR214X                                                           
VR214B   CLC   RES+2(4),=4X'40'                                                 
         BE    *+8                                                              
         MVI   RREPNO,C'Y'                                                      
VR214X   BR    RA                                                               
*                                                                               
VALR215  DS    0H                                                               
         MVC   HALF,=X'0C00'       12 MONTHS                                    
         BAS   RE,VALMAX                                                        
         CLI   RO2,C'7'            IF COL 63 HAS 7                              
         BNE   VR215D                                                           
         CLI   RO4,C' '               AND COL 65 IS BLANK                       
         BNE   VR215D                                                           
         CLI   RO5,C' '            COL 66 MUST NOT BE BLANK                     
         BNE   VR215D                                                           
         MVI   ROUTNUM,X'A8'                                                    
         B     VALINV                                                           
VR215D   CLC   RBOOK(12),=12X'40'  IF ALL BLANK EXIT                            
         BE    VR215B                                                           
         CLI   RBOOK,X'40'       IS FIRST BYTE BLANK                            
         BNE   VR215C                                                           
         CLC   RBOOK+1(2),=C'BF'   IF BF                                        
         BNE   VR215B                                                           
         CLC   RPRO,=C'POL'        AND POL THEN INVALID                         
         BNE   V215EST                                                          
         MVI   ROUTNUM,93                                                       
         B     VALINV                                                           
V215EST  TM    ESTSAVE,X'26'       EST MUST = NNN OR ALL                        
         BNZ   VR215L                                                           
         MVI   ROUTNUM,5                                                        
         B     *+8                                                              
BFERR    MVI   ROUTNUM,3                                                        
         B     VALINV                                                           
*                                                                               
VR215C   CLI   RBOOK+6,X'40'                                                    
         BNE   VR215AA                                                          
         MVI   ROUTNUM,X'AB'                                                    
         B     VALINV                                                           
VR215AA  CLI   RBOOK,X'40'                                                      
         BNE   VR215AB                                                          
         MVI   ROUTNUM,X'AA'                                                    
         B     VALINV                                                           
VR215AB  CLI   RREPNO,X'40'                                                     
         BNE   VR215L                                                           
VR215A   MVI   ROUTNUM,X'1A'                                                    
         B     VALINV                                                           
VR215B   CLI   RREPNO,X'40'                                                     
         BNE   VR215A                                                           
VR215L   TM    SEDSAVE,X'10'       IF DATES=ES                                  
         BNO   VR215M                                                           
         TM    PROSAVE,X'02'       AND PROD=ALL                                 
         BNO   VR215M                                                           
         MVI   ROUTNUM,X'09'            THIS IS ERROR                           
         B     VALINV                                                           
VR215M   DS    0H                  FOR FUTURE                                   
*                                                                               
VR215X   BR    RA                                                               
*                                                                               
*                                                                               
VALR217  DS    0H                                                               
         CLI   RO7,X'40'           IS THERE A FILM TYPE                         
         BE    V217A                                                            
         CLI   RO7,X'FF'                                                        
         BNE   *+8                                                              
         MVI   RO7,X'40'                                                        
         MVI   RO6,C'F'                                                         
         CLI   RO2,C'Y'            IF RO2=Y                                     
         BNE   V217C                                                            
         MVI   RO2,C'F'            MAKE IT F                                    
         B     V217C                                                            
V217A    CLI   RO2,C'Y'            CLASS/CML DETAIL NEEDS FILM TYPE             
         BNE   V217C                                                            
         MVI   ROUTNUM,X'AF'                                                    
         B     V217INV                                                          
V217C    CLI   RO3,C'Y'            IF RO3=Y                                     
         BNE   V217X                                                            
         MVI   RO3,C'S'            MAKE IT S                                    
         B     V217X                                                            
V217INV  MVC   FERN,=AL2(FLDINV)                                                
V217X    BR    RA                                                               
         EJECT                                                                  
*                                                                               
VALR220  DS    0H                                                               
         MVC   RCLI,=C'ALL'                                                     
         BR    RA                                                               
*                                                                               
VALR221  DS    0H                                                               
         MVC   HALF,=X'0D00'       13 MTHS                                      
         BAS   RE,VALMAX                                                        
         BAS   RE,VALNDCB                                                       
         BAS   RE,VALDPOR                                                       
         BR    RA                                                               
*                                                                               
*                                                                               
VALR227  DS    0H                                                               
         MVC   RMARK,=4X'F0'                                                    
         MVC   RPRO,=C'POL'                                                     
         CLC   =C'NO',REST                                                      
         BNE   V227X                                                            
         CLI   RSTRD,X'40'                                                      
         BNE   V227X                                                            
         MVI   ROUTNUM,9                                                        
         MVC   FERN,=AL2(FLDINV)                                                
V227X    BR    RA                                                               
*                                                                               
*                                                                               
VALR231  EQU   *                                                                
         GOTO1 =A(VVALR231),DMCB,(R9),RR=RELO                                   
         BR    RA                                                               
*                                                                               
VALR233  EQU   *                                                                
         LA    R6,1                MONDAY                                       
         LA    R5,RSTRD                                                         
         BAS   RE,VALDAY                                                        
         LA    R6,7                SUNDAY                                       
         LA    R5,RENDD                                                         
         BAS   RE,VALDAY                                                        
         MVC   HALF,=X'0E01'       14 WEEKS                                     
         BAS   RE,VALMAX                                                        
         MVC   RHUT(3),=C'NOI'                                                  
         BR    RA                                                               
*                                                                               
VALR235  EQU   *                                                                
***      CLC   RNUM,=C'DX'         FOR DX SPECIAL VALIDATION                    
***      BNE   *+10                                                             
***      BRAS  RE,DXCHKS                                                        
***      BNER  RA                  RETURNED ERROR, EXIT                         
         CLC   RAGY,=C'BC'         AGENCY MD (POWER CODE=BC)                    
         BE    V235A                                                            
         CLC   RAGY,=C'QJ'         AGENCY QJ                                    
         BE    V235A                                                            
         CLC   RAGY,=C'BS'         AGENCY BS                                    
         BE    V235A                                                            
         CLC   RAGY,=C'JW'         AGENCY JW                                    
**       BNER  RA                                                               
         BNE   V235E                                                            
V235A    CLC   =3X'40',RSTA                                                     
         BE    V235D                                                            
         CLC   =C'ALL',RSTA        CAN NOT HAV STA=ALL                          
         BNER  RA                                                               
V235D    MVI   ROUTNUM,7                                                        
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
* D7 COMES IN HERE FOR 'IF STATION= ALL ' TEST                                  
V235E    CLI   RSTA,X'40'          ,,IF STATION ALL                             
         BE    V235F                                                            
         CLC   =C'ALL',RSTA                                                     
         BNER  RA                                                               
V235F    CLI   RMARK,X'40'          ,,MKT MUST BE INPUT                         
         BNER  RA                                                               
         MVI   ROUTNUM,X'5E'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
*                                                                               
VALR236  DS    0H                                                               
         CLC   RCLI,=C'ALL'        IF CLIENT NOT = ALL                          
         BER   RA                                                               
         CLI   RO1,C'3'            RO1 MUST = 3                                 
         BER   RA                                                               
         MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR234  EQU   *                                                                
         GOTO1 =A(VVALR234),DMCB,(R9),(RC),RR=RELO                              
         BR    RA                                                               
*                                                                               
*                                                                               
VALR228  EQU   *                                                                
         GOTO1 =A(VVALR228),DMCB,(R9),RR=RELO                                   
         BR    RA                                                               
*                                                                               
VALR232  EQU   *                                                                
         CLI   RMARK,X'40'          ,,MKT MUST BE INPUT                         
         BNER  RA                                                               
         MVI   ROUTNUM,X'06'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
*                                                                               
VALR240  EQU   *                                                                
*****    CLC   =C'ALL',RSTA              ALLOW NETW ALL FOR WT,U# ONLY          
*****    BNER  RA                                                               
*****    CLC   RAGY,=C'WT'                                                      
*****    BER   RA                                                               
*****    CLC   RAGY,=C'U#'                                                      
*****    BER   RA                                                               
*****    MVI   ROUTNUM,X'71'                                                    
*****    MVC   FERN,=AL2(FLDINV)                                                
*****    BR    RA                                                               
*                                                                               
VALR241  EQU   *                                                                
         MVC   RPRO,=C'POL'                                                     
         MVC   RES,=C'ES'                                                       
         BR    RA                                                               
*                                                                               
VALR242  EQU   *                                                                
         CLI   RO2,C'M'                 'M' OPTION ?                            
         BE    V242C                                                            
         CLI   RSTRD,X'40'               NO - IF DATES INPUT MUST               
         BNHR  RA                                                               
         CLC   RSTRD+4(2),=C'00'              BE YYMMDD                         
         BE    V242ERR                                                          
         CLC   RENDD+4(2),=C'00'                                                
         BE    V242ERR                                                          
         BR    RA                                                               
*                                         YES -M OPTION                         
V242C    CLI   RSTRD,X'40'             ,,  MUST HAVE DATES                      
         BNH   V242ERR                                                          
         CLC   RSTRD+4(2),=C'00'       ,,  YYMM00 ONLY IN START                 
         BH    V242ERR                                                          
         CLC   RENDD+4(2),=C'00'       ,,  YYMM00 ONLY IN END                   
         BH    V242ERR                                                          
         MVC   RSTRD+4(2),=C'  '         CHANGE TO BLANKS                       
         MVC   RENDD+4(2),=C'  '                                                
         BR    RA                                                               
V242ERR  MVI   ROUTNUM,9                                                        
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR244  EQU   *                                                                
         MVI   ROUTNUM,2           MN REPORT MUST BE FOR CLIENT 'CC'            
         CLC   =C'CC',RCLI                                                      
         BNE   VALINV                                                           
         CLC   =C'ALL',REST        IF EST=ALL                                   
         BNE   VR244AA                                                          
         CLC   RENDD,SPACES                                                     
         BNE   VR244AA                                                          
         MVI   ROUTNUM,9                                                        
         B     VALINV              MUST BE END DATE                             
VR244AA  CLC   RAGY,=C'AP'         IF APNY                                      
         BNER  RA                                                               
         CLC   =C'ALL',RSTA        STATION MUST = ALL                           
         BE    VR244A                                                           
         MVI   ROUTNUM,7                                                        
         B     VALINV                                                           
VR244A   CLC   RMARK,=C'ALL'       MARKET MUST = ALL                            
         BE    VR244B                                                           
         MVI   ROUTNUM,X'5E'                                                    
         B     VALINV                                                           
VR244B   MVI   RDIST,C'F'          SET MKTGROUP TO 'F'                          
         BR    RA                                                               
*                                                                               
VALR245  DS    0H                                                               
         CLC   =C'ALL',REST                                                     
         BNER  RA                                                               
         MVC   REST(6),=C'001255'                                               
         BR    RA                                                               
*                                                                               
VALR246  DS    0H                                                               
         MVC   RPRO(3),=C'POL'                                                  
         MVC   RES,=C'ES'                                                       
         BR    RA                                                               
*                                                                               
VALR252  DS 0H                                                                  
         CLI   RNUM+61,C'Y'        IF NOT TEST RUN                              
         BE    VR252                                                            
         BRAS  RE,CKUPDT           CHECK IF UPDATE IS ALLOWED                   
         BNE   VR252                                                            
         MVI   ROUTNUM,X'74'                                                    
         MVC   FERN,=AL2(1250)                                                  
         BR    RA                                                               
VR252    CLC   RNUM,=C'KB'                                                      
         BER   RA                                                               
         CLC   BVROUT(4),=C'SOON'  IF SOON                                      
         BNER  RA                                                               
         CLI   RNUM+61,C'Y'        MUST BE TEST RUN                             
         BER   RA                                                               
         MVI   ROUTNUM,X'74'                                                    
         B     VALINV                                                           
*                                                                               
VALINV   MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
VALPROFE MVC   FERN,=AL2(PROFERR)                                               
         BR    RA                                                               
*                                                                               
*                                                                               
*                                                                               
VALMAX   EQU   *                             START,END DATES DURATION           
         LR    R0,RE                                                            
         GOTO1 AVVALMAX,DMCB,(R9)                                               
         CLC   FULL(3),=C'ERR'                                                  
         BER   RA                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
VALNDCB  EQU   *                   NEW DATA COMPARE VS. BOOK-HUT                
         CLI   RNUM+56,C' '                                                     
         BE    VNDCBX              NOT INPUT                                    
         LA    R6,DCOMTAB                                                       
VNDCB2   CLI   0(R6),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R6),RNUM+56                                                  
         BE    VNDCB4                                                           
         LA    R6,3(R6)                                                         
         B     VNDCB2                                                           
*                                                                               
VNDCB4   CLI   1(R6),C'R'          IS BOOK REQUIRED                             
         BNE   VNDCB6                                                           
         CLI   RBOOK,C' '                                                       
         BE    VNDCB20             MISSING BOOK                                 
VNDCB6   CLI   RBOOK,C' '                                                       
         BE    VNDCBX                                                           
         MVC   RNUM+55(1),2(R6)       SET RERATE IN REQUEST                     
         B     VNDCBX              WHEN BOOK IS INPUT                           
VNDCB20  MVI   ROUTNUM,14                                                       
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
*                                                                               
VNDCBX   BR    RE                                                               
*                                                                               
DCOMTAB  DS    0H                                                               
         DC    C'AOP'              DATA COMPARE,BOOK,RERATE                     
         DC    C'BRI'                        O=OPTIONAL BOOK                    
         DC    C'CRP'                       R=REQUIRED BOOK                     
         DC    C'DRI'                                                           
         DC    C'EOP'                                                           
         DC    C'FRI'                                                           
         DC    C'GRA'                                                           
         DC    C'HRP'                                                           
         DC    C'IRI'                                                           
         DC    C'JRP'                                                           
         DC    C'KRI'                                                           
         DC    X'00'                                                            
         EJECT                                                                  
VALACC   EQU   *                                                                
         MVI   ROUTNUM,X'5E'       CURSOR TO MKT/MKTGRP                         
         B     VALACC1                                                          
*                                                                               
VALACCM  MVI   ROUTNUM,X'06'           CURSOR TO MARKET                         
*                                                                               
VALACC1  EQU   *                                                                
         CLI   6(R3),C'+'          CHECK FOR LIMIT ACCESS                       
         BNE   VALACCX                                                          
         CLC   RMARK(3),=C'ALL'                                                 
         BE    VALACC2                                                          
         CLC   RMARK,=C'    '                                                   
         BNE   VALACCX                                                          
VALACC2  EQU   *                                                                
         CLC   RSTA(3),=C'   '                                                  
         BE    VALACCE                                                          
         CLC   RSTA(3),=C'ALL'                                                  
         BNE   VALACCX                                                          
VALACCE  MVC   FERN,=AL2(ACCERR)                                                
         BR    RA                                                               
VALACCX  BR    RE                                                               
         EJECT                                                                  
VALEST   EQU   *                             EST=ALL ONLY FOR DDS               
         LR    R0,RE                                                            
         GOTO1 =A(VESTRTN),DMCB,(R9),RR=RELO                                    
         CLC   FULL(3),=C'ERR'                                                  
         BER   RA                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*VALPO    EQU   *                             GET PRINT OPTIONS                 
*         SR    R7,R7                                                           
*         LA    R6,RNUM+49                    POINT TO TEMP VALUES              
*         CLI   0(R6),C'Y'                                                      
*         BNE   *+8                                                             
*         LA    R7,1(R7)                      +1 IF COSTS=Y                     
*         CLI   1(R6),C'Y'                                                      
*         BNE   *+8                                                             
*         LA    R7,2(R7)                      +2 IF DEMOS=Y                     
*         CLI   2(R6),C'Y'                                                      
*         BNE   *+8                                                             
*         LA    R7,4(R7)                      +4 IF ASTERISKS=Y                 
*                                                                               
*         STC   R7,RO2                                                          
*         STC   R7,RO5                                                          
*         MVC   0(3,R6),=C'   '               CLEAR TEMP VALUES                 
*         TR    RO2,VALPO1T                                                     
*         TR    RO5,VALPO2T                                                     
*                                                                               
*VALPOX   BR    RE                                                              
*                                                                               
*VALPO1T  DC    C'04251325'              PRINT OPTION 1 VALUES(RO2)             
*VALPO2T  DC    C'03120312'              PRINT OPTION 2 VALUES(RO5)             
         EJECT                                                                  
VALDPT   ST    RE,FULL                                                          
         TM    RO2,X'F0'           SEE IF NUMERIC                               
         BNO   VALDPTX      NO - DON'T TRY TO READ                              
         MVI   ROUTNUM,30                                                       
         MVC   PLIST+3(1),RO2                                                   
         B     VALDPT1                                                          
*                                                                               
VALDPOR  ST    RE,FULL             STORE RETURN IN FULL                         
         TM    RNUM+60,X'F0'       DPT OVERRIDE IN CLO 61 - TEST NUM            
         BNO   VALDPTX                                                          
         MVI   ROUTNUM,89                                                       
         MVC   PLIST+3(1),RNUM+60                                               
*                                                                               
VALDPT1  MVC   PLIST(3),RAGY                                                    
         L     R5,DATAMGR                                                       
         GOTO1 =V(DPTRD),PLIST,,SPTREC,(0(R3),(R5)),RR=RELO                     
         CLI   PLIST+8,X'FF'                                                    
         BNE   *+14                                                             
         MVC   FERN,=AL2(0)    DISK ERROR                                       
         B     VALDPTX                                                          
*                                                                               
         CLI   PLIST+8,X'08'                                                    
         BNE   VALDPTX                                                          
         MVC   FERN,=AL2(DPTNFD)           DPT MENU NOT FOUND                   
         BR    RA                                                               
*                                                                               
VALDPTX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE EXPECTS R5 POINTS TO DATE                                      
*                                                                               
VALDAY   NTR1                                                                   
         BRAS  RE,VALRFP           ..ARE WE DEALING WITH SYMBOLIC NAME          
         BE    VALDX               ..YES/SKIP DAY VALIDATION                    
         GOTO1 GETDAY,DMCB,(0,(R5)),TEMP                                        
         CLC   TEMP(3),=C'   '                                                  
         BE    VALDERR                                                          
         SR    R4,R4                                                            
         IC    R4,DMCB                                                          
         CR    R4,R6                                                            
         BNE   VALDERR         WRONG DAY                                        
VALDX    XIT1                                                                   
*                                                                               
VALDERR  MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,9                                                        
         BR    RA                                                               
         EJECT                                                                  
VALRRATE DS    0H                  IF RERATE ENTERED THEN SO MUST               
RATVBK   CLI   RNUM+55,C' '        NO RERATE                                    
         BNE   RATVBK1                                                          
         CLI   RBOOK,C' '                                                       
         BE    RATVBKX                                                          
         MVI   ROUTNUM,85                                                       
         B     RATERR                                                           
*                                                                               
RATVBK1  CLI   RBOOK,C' '                                                       
         BNE   RATVBKX                                                          
         MVI   ROUTNUM,14                                                       
RATERR   MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
*                                                                               
RATVBKX  BR    RE                                                               
*                                                                               
         EJECT                                                                  
RDPROF   ST    RE,FULL                                                          
*                                  GET PROFILE AND RETURN IT IN TEMP            
         XC    TEMP(20),TEMP                                                    
         MVC   TEMP+30(2),=C'SO'                                                
         MVC   TEMP+32(2),HALF        HALF HAS REPORT NUMBER                    
         MVC   TEMP+34(6),RAGY       AGY/MED/CLT                                
* - IN NETWORK MEDIA = N, BUT IF THEY HAVE STATION TYPE PROFILES C,O            
* - ETC. THESE DO NOT GET PICKED UP, ATTEMPT TO RECTIFY THIS HERE               
         CLC   RNUM,=C'DU'         IF DU                                        
         BE    *+14                                                             
         CLC   RNUM,=C'BU'         IF BU                                        
         BNE   RDP1                                                             
         CLI   RMED,C'N'           AND NETWORK                                  
         BNE   RDP1                                                             
         CLI   RO5,X'40'           AND IF FILTERING STATION TYPE                
         BNH   RDP1                                                             
         MVC   TEMP+36(1),RO5      MOVE IN STATION TYPE                         
*                                                                               
RDP1     DS    0H                                                               
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
*                                                                               
RDP2     DS    0H                                                               
         L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
*        GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
*                                                                               
RDPROFX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
CKINVD   ST    RE,FULL                                                          
****     LA    R5,RENDD            COL 44 FOR REQS 04 AND 06                    
****     B     CKINV4                                                           
*                                                                               
CKINVD1  ST    RE,FULL                                                          
***      LA    R5,RREPNO           COL 30 FOR REQ B1                            
         LA    R5,R2USER           COL 30 FOR REQ B1                            
*                                                                               
CKINV4   CLC   0(6,R5),SPACES                                                   
*        BE    CKINVX              INV DATE NOT SPECIFIED                       
*        B     CKBLOK                                                           
         BNE   CKBLOK                      IF INV DATE BLANK                    
         GOTO1 DATCON,DMCB,(5,0),(0,0(R5)) GET TODAYS                           
         B     CKBLOK                                                           
*                                                                               
*CKINV5   LA    R5,RREPNO           COL 30 FOR REQ B1                           
CKINV5   LA    R5,R2USER           COL 30 FOR REQ B1                            
         PACK  DUB,TODAY+4(2)                                                   
         CVB   R4,DUB                                                           
         LCR   R4,R4               MAKE NEGATIVE                                
*                                  GOTO ADDDAY TO GET LAST MTH                  
         GOTO1 ADDAY,DMCB,TODAY,TEMP+6,(R4)                                     
         CLC   TEMP+6(4),0(R5)     YR AND MTH                                   
         BH    MOSERROR                                                         
*        BH    DATERR                                                           
*                                                                               
         LA    R4,90                GOTO ADDDAY TO GET 90 DAYS FORWARD          
         GOTO1 ADDAY,DMCB,TODAY,TEMP+6,(R4)                                     
         CLC   0(4,R5),TEMP+6                                                   
         BNH   CKINVX                                                           
         CLC   RAGY,=C'WI'                                                      
         BE    CKINVX                                                           
         B     MOSERROR                                                         
DATERR   MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'0B'                                                    
         BR    RA                                                               
         SPACE                                                                  
*                                                                               
CKBLOK   DS    0H                                                               
*              ROUTINE TO CHECK FOR MONTH OF SERVICE BILL DATE LOCKOUT          
         XC    TEMP,TEMP                                                        
         MVC   TEMP+30(2),=C'S0'                                                
         MVC   TEMP+32(2),=C'LK'                                                
         MVC   TEMP+34(6),RAGY        AGY/MED/CLT                               
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
CKBLOK5  L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
*        GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
         OC    TEMP(16),TEMP                                                    
         BZ    CKALK               RECORD NOT FOUND/TRY ALK                     
         MVI   TEMP+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,TEMP),(0,TEMP+6)                                  
* PROFILE DATE IS PWOS SO DATCON RETUNRS 00 FOR Y2K                             
* GO AGAIN TO DATCON TO GET NEW FORMAT                                          
         GOTO1 DATCON,DMCB,(0,TEMP+6),(0,TEMP+12)                               
***      CLC   TEMP+12(4),RREPNO                                                
         CLC   TEMP+12(4),R2USER                                                
         BH    MOSERROR                                                         
         BL    CKINV5                                                           
MOSERROR MVC   FERN,=AL2(240)            MOS BILLING DATE LOCK                  
         MVI   ROUTNUM,X'0B'                                                    
         BR    RA                                                               
CKALK    XC    TEMP,TEMP                                                        
         MVC   TEMP+30(2),=C'A0'                                                
         MVC   TEMP+32(2),=C'LK'                                                
         MVC   TEMP+42(2),RAGY                                                  
         L     R5,DATAMGR                                                       
         BAS   R4,GTPROF                                                        
*        GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
         OC    TEMP(16),TEMP                                                    
         BZ    CKINV5              RECORD NOT FOUND OK                          
         MVI   TEMP+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,TEMP),(0,TEMP+6)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+6),(0,TEMP+12) (see lk note above)           
***      CLC   TEMP+6(4),RREPNO                                                 
         CLC   TEMP+12(4),R2USER                                                
*        BH    DATERR                                                           
         BH    MOSERROR                                                         
         BL    CKINV5                                                           
         MVC   FERN,=AL2(240)            MOS BILLING DATE LOCK                  
         MVI   ROUTNUM,X'0B'                                                    
         BR    RA                                                               
*                                                                               
GTPROF   DS    0H                                                               
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
         BR    R4                                                               
         SPACE 2                                                                
*                                                                               
CKINVX   L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
VALIDS   DS    0H                                                               
         CLI   ROAGY,C'Y'                                                       
         BNER  RE                  ID SEQ NOT SPECIFIED                         
         LR    R0,RE                                                            
         MVI   ROUTNUM,0                                                        
         GOTO1 =A(VVALIDS),DMCB,(R9),RR=RELO                                    
         CLI   ROUTNUM,0           WAS THERE AN ERROR                           
         BNER  RA                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*        READ LAST REQUEST RECORD AND UPDATE IT WITH AMENDED DATA               
*                                                                               
AMDREQ   MVC   ADR,LADR                                                         
         GOTO1 DATAMGR,DMCB,(DMIN,DMRDIR),REQUEST,ADR,TEMP,,L'REQREC            
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         MVC   REQREC(80),TEMP         KEEP HEADER FROM READ                    
         BRAS  RE,SETREQ               SET REMAINING REQ RECORD DETAILS         
         OI    RHDR+15,X'02'           SET 2-CARD LINKED                        
         GOTO1 DATAMGR,DMCB,(DMIN,DMWRT),REQUEST,ADR,REQREC,,L'REQREC           
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
         EJECT                                                                  
*        SEARCH REQUEST CHAIN AND COUNT/DISPLAY                                 
*                                                                               
ENQREQ   LA    R5,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R5,R6                                                            
         AR    R2,R5                                                            
         USING T208FED,R2                    R2=A(1ST FLD IN ENQ)-64            
         LA    R4,ENQFRSTH                                                      
         ST    R4,DISPADR                    R4=A(NEXT TWA LINE NUM)            
*                                                                               
         TWAXC (R4),PROT=Y                   CLEAR TWA                          
         LA    RE,28                                                            
         LR    RF,R4                                                            
LOOP1    NI    1(RF),X'DF'         TURN OFF PROTECTED BIT                       
         OI    6(RF),X'80'                                                      
         ZIC   R1,0(RF)                                                         
         AR    RF,R1                                                            
         BCT   RE,LOOP1                                                         
*                                                                               
         USING DISPLD,R4                                                        
         XC    SKIPCTR(08),SKIPCTR           SET COUNTERS                       
         SPACE 2                                                                
         XC    ADR,ADR                                                          
         LA    R0,DMRDIR                                                        
         CLI   REQOPTN,C'N'                                                     
         BNE   ENQR1                                                            
         CLC   DISPFLDS(2),DISPMAX           WAS THERE PREVIOUS                 
         BL    ENQRE2                        NO                                 
         MVC   REQINCR,=H'2'                 SET TO SKIP 1                      
         LH    R6,DISPFLDS+2                 SEQ OF 1ST = LAST +1               
         AH    R6,DISPFLDS                                                      
         STH   R6,DISPFLDS+2                                                    
         LH    R6,DISPFLDS                   SET ADR TO A(LAST)                 
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   ADR,0(R6)                                                        
         B     ENQR3                                                            
ENQR1    MVC   DISPFLDS+2(2),REQINCR         SEQ OF 1ST = INPUT VALUE           
         CLI   REQNUM,255                                                       
         BNE   *+12                                                             
         LA    R0,DMRSEQ                                                        
         B     ENQR3                                                            
         MVC   ADR,=X'000000FF'                                                 
         MVC   ADR(2),LREQOHDR+26                                               
         B     ENQR3                                                            
         SPACE 2                                                                
ENQR2    LA    R0,DMRSEQ                                                        
         CLI   REQNUM,255                                                       
         BE    ENQR3                                                            
         MVC   ADR,RHDR+16                                                      
         LA    R0,DMRDIR                                                        
ENQR3    XC    REQREC,REQREC                                                    
         GOTO1 DATAMGR,DMCB,(DMIN,(R0)),REQUEST,ADR,REQREC,SPTREC               
         CLI   DMCB+8,0                                                         
         BE    ENQR4                                                            
         TM    DMCB+8,X'80'                                                     
         BO    ENQREOF                                                          
         B     REQIOERR                                                         
         SPACE 2                                                                
ENQR4    CLI   REQNUM,255          FILTER OUT CANCELLED FROM ALL OPTION         
         BNE   ENQR4A                                                           
         CLC   RNUM,=X'FFFF'                                                    
         BE    ENQR2                                                            
         CLC   RNUM,=C'99'                                                      
         BE    ENQR2                                                            
         TM    REQFLAG,X'01'       FILTER OUT UNLINKED FROM ALL OPTION          
         BZ    ENQR2                                                            
         B     ENQR4B                                                           
ENQR4A   CLC   RNUM,LREQOHDR+26    FILTER OUT ANY SUNDRY REQUESTS               
         BE    *+14                                                             
         CLC   RNUM,=C'99'                                                      
         BNE   ENQR2                                                            
*                                                                               
ENQR4B   CLI   LREQOHDR+28,C'*'                                                 
         BE    ENQR4C                                                           
         CLC   RAGY,LREQOHDR+28    FILTER ON AGY                                
         BNE   ENQR2                                                            
*                                                                               
ENQR4C   OC    LREQOHDR(4),LREQOHDR                                             
         BZ    ENQR5                                                            
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE CODE                        
         BNE   ENQR2                                                            
         SPACE 2                                                                
ENQR5    CLI   DDS,1               IF IT'S DDS                                  
         BNE   NOTDDS                                                           
         CLC   =C'DDS',LREQOHDR+94 AND WANT TO SEE ALL                          
         BE    ENQR5C                                                           
                                                                                
NOTDDS   TM    REQFLTR,X'01'       REQUESTOR FILTER REQUIRED                    
         BZ    ENQR5A              NO                                           
         CLC   RNAME,LREQOHDR+94   YES TEST REQUESTOR NAME                      
         BNE   ENQR2               IGNORE IF DIFFERENT                          
*                                                                               
ENQR5A   TM    REQFLTR,X'02'       MEDIA FILTER REQUIRED                        
         BZ    ENQR5A1                                                          
         CLC   RMED,LREQOHDR+30                                                 
         BNE   ENQR2                                                            
*                                                                               
ENQR5A1  OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    ENQR5B                                                           
         CLC   REQOUT,LREQOHDR+4   FILTER ON OUTPUT TYPE                        
         BNE   ENQR2                                                            
*                                                                               
ENQR5B   OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    ENQR5C                                                           
         CLC   REQDEST,LREQOHDR+11 FILTER ON DESTINATION ID                     
         BNE   ENQR2                                                            
*                                                                               
ENQR5C   EQU   *                                                                
         SPACE 2                                                                
ENQR6    LH    R4,REQINCR                    IGNORE (REQINCR-1) REQS            
         BCTR  R4,0                                                             
         CH    R4,SKIPCTR                                                       
         BE    ENQR7                                                            
         LH    R4,SKIPCTR                                                       
         LA    R4,1(R4)                                                         
         STH   R4,SKIPCTR                                                       
         B     ENQR2                                                            
         SPACE 2                                                                
ENQR7    LH    R5,READCTR                    UPDATE REQ READ COUNTER            
         LA    R5,1(R5)                                                         
         STH   R5,READCTR                                                       
         MVC   TEMP,REQREC              SAVE REQ REC                            
         CLC   RNUM,=C'99'                                                      
         BNE   *+16                                                             
         LH    R5,CANCCTR                    UPDATE CANCELLED COUNTER           
         LA    R5,1(R5)                                                         
         STH   R5,CANCCTR                                                       
         CLI   REQOPTN,C'L'        DISPLAY LAST OPTION                          
         BE    ENQR2                         YES DO NOT DISPLAY                 
         SPACE 2                                                                
ENQR8    BRAS  RE,ENQDISP                    DISPLAY REQUEST                    
         CLC   DISPCTR,DISPMAX               END OF SCREEN                      
*        BNE   ENQR2                         NO BACK FOR NEXT                   
         BL    ENQR2                            BACK FOR NEXT                   
         B     ENQRE3                                                           
         SPACE 2                                                                
ENQREOF  OC    READCTR,READCTR                                                  
         BZ    ENQRE2                        NO REQUESTS FOUND                  
         CLI   REQOPTN,C'L'                                                     
         BE    ENQRE1                                                           
         OC    DISPCTR,DISPCTR                                                  
         BZ    ENQRE2                        NO REQUESTS DISPLAYED              
         B     ENQRE3                                                           
         SPACE 2                                                                
ENQRE1   MVC   REQREC,TEMP              OPTION TOTAL                            
         BRAS  RE,ENQDISP                    DISPLAY THE LAST REQ               
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(24),=C'REQUEST TOTAL = NNN LIVE'                            
         MVC   TEMP+24(33),=C' - LAST REQUEST NUM NNN DISPLAYED'                
         LH    R6,READCTR                                                       
         SH    R6,CANCCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+16(3),DUB                                                   
         LH    R6,READCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+44(3),DUB                                                   
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE2   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(18),=C'REQUESTS NOT FOUND'                                  
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE3   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'REQUESTS NNN THRU NNN DISPLAYED'                     
         MVC   TEMP+31(25),=C' - CHANGE CANCEL STATUS ?'                        
         LH    R6,DISPFLDS+2                 GET FIRST SEQ NUM                  
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+09(3),DUB                                                   
         AH    R6,DISPFLDS                   GET LAST = FIRST+TOTAL-1           
         BCTR  R6,0                                                             
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+18(3),DUB                                                   
         MVI   STATUS,1                      SET STATUS FOR INPUT               
         B     ENQRX                                                            
*                                                                               
ENQRX    DS    0H                                                               
*                                                                               
ENQRX4   LR    R2,R3                                                            
         USING T208FFD,R2                                                       
         MVC   BVRHDR,TEMP                   SET HDR MSG                        
         MVC   FERN,=AL2(FF)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*        CHANGE REQUEST CANCEL STATUS                                           
*                                                                               
CANREQ   LA    R5,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R5,R6                                                            
         AR    R2,R5                                                            
         USING T208FED,R2                                                       
         LA    R4,ENQFRSTH                   R4=A(NEXT TWA LINE)                
         USING DISPLD,R4                                                        
         LH    R5,DISPFLDS                   R5=NUM OF TWA LINES                
         LA    R6,DISPFLDS+4                 R6=A(DISK ADR)                     
         LTR   R5,R5                                                            
         BNZ   CANR1                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
CANR1    TM    DLHDR+4,X'C0'                 ANY INPUT IN CANCEL FIELD          
         BZ    CANR6                         NO                                 
         MVC   ADR,0(R6)                                                        
         GOTO1 DATAMGR,DMCB,(DMIN,DMRDIR),REQUEST,ADR,REQREC                    
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
         CLI   DLCANC,0                                                         
         BE    *+12                                                             
         CLI   DLCANC,C' '                                                      
         BNE   CANR1B                                                           
CANR1A   CLC   RNUM,=C'99'         UNCANCELL REQUIRED                           
         BNE   CANR6                                                            
         MVC   DUB(1),REQNUMB                                                   
         BRAS  RE,GETREQID                                                      
         MVC   RNUM,DUB+1                                                       
         MVI   DLCANC,C' '                                                      
         B     CANR4                                                            
*                                                                               
CANR1B   CLI   DLCANC,C'U'                                                      
         BNE   *+18                                                             
         CLC   RNUM,=C'99'         ALLOW EXPLICIT UNCANCEL                      
         BE    CANR1A                                                           
         B     CANR1C                                                           
         CLI   DLCANC,C'A'                                                      
         BE    CANR3                                                            
         CLI   DLCANC,C'C'                                                      
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CANCELL REQUIRED                             
         BE    CANR6                                                            
         MVC   RNUM,=C'99'                                                      
         B     CANR4                                                            
*                                                                               
CANR1C   MVC   FERN,=AL2(2)        INVALID CODE                                 
         ST    R4,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CANR3    CLI   DDS,1               ONLY DDS CAN AMEND                           
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CAN ONLY AMEND ACTIVE REQ                    
         BE    CANR1C                                                           
*        CLI   5(R4),78                                                         
*        BE    CANR3B                                                           
*        SR    R1,R1               REDISPLAY IF TRUNC INPUT                     
*        IC    R1,5(R4)                                                         
*        SH    R1,=H'1'                                                         
*        BM    CANR3A                                                           
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   TEMP(0),8(R4)                                                    
*CANR3A   BAS   RE,ENQDISP                                                      
*        LTR   R1,R1                                                            
*        BM    *+18                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   8(0,R4),TEMP                                                     
CANR3B   CLI   RHDR+10,234         IF K1                                        
         BNE   *+14                                                             
         MVC   RMED(1),DLREQ+2     ONLY AMEND MEDIA                             
         B     CANR3C                                                           
         MVC   RMED(22),DLREQ+2                                                 
         MVC   R3037(8),DLREQ+27                                                
CANR3B1  MVC   RSTRD(18),DLREQ+35                                               
**       CLI   RHDR+10,234         IF K1                                        
**       BE    *+10                DON'T MOVE DISP LINE BACK TO REC             
*                                  DISP2 LINE ACTS ERRATICALLY DON'T            
*                                  HAVE TIME TO TRACK IT DOWN                   
         MVC   RCARD2(77),DLREQ2   PXZ FOR ZEN                                  
         CLI   RHDR+10,X'04'                                                    
         BNE   CANR3C                                                           
*                                  CAN'T CHANGE MANUAL AMTS ON BILL             
         MVC   RNAME(9),DLREQ+66                                                
         B     CANR4                                                            
*                                                                               
CANR3C   DS    0H                                                               
*                                                                               
CANR3D   DS    0H                                                               
         CLI   RHDR+10,100                                                      
         BNL   CANR3E                                                           
         MVC   R5668(22),DLREQ+53                                               
         B     CANR4                                                            
*                                                                               
CANR3E   DS    0H                  NEW REQS - CAN'T CHANGE COL 59               
         MVC   R5668(3),DLREQ+53                                                
*                                  CAN'T CHG PROGRAM TYPE - BINARY              
         MVC   R5668+4(18),DLREQ+57                                             
*                                                                               
         B     CANR4                                                            
         SPACE 2                                                                
CANR4    GOTO1 DATAMGR,DMCB,(DMIN,DMWRT),REQUEST,ADR,REQREC                     
         CLI   DMCB+8,0                                                         
         BE    CANR6                                                            
CANR5    MVC   FERN,=AL2(0)                  DISK ERROR                         
         ST    R4,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CANR6    LA    R4,DLNEXT                     BUMP TO NEXT LINE                  
         TM    DLCANCH+1,X'20'        IF CANCEL FIELD PROTECTED                 
         BZ    *+8                                                              
         LA    R4,DLNEXT              BUMP ONE MORE                             
         ST    R4,DISPADR                                                       
         LA    R6,4(R6)                      BUMP TO NEXT DISK ADR              
         BCT   R5,CANR1                                                         
         SPACE 2                                                                
CANRX    XC    TEMP,TEMP                                                        
         MVC   TEMP(29),=C'REQUEST CANCEL STATUS AMENDED'                       
         B     ENQRX4                                                           
         EJECT                                                                  
*        DISPLAY TOTAL REQUEST COUNTS                                           
*                                                                               
TOTREQ   XC    TOTCTR(256),TOTCTR                                               
         XC    TOTCTR+256(256),TOTCTR+256                                       
         XC    ADR,ADR                                                          
         LR    R2,R3                                                            
         USING T208FFD,R2                                                       
         GOTO1 ,DMCB,(DMIN,DMRSEQ),REQUEST,ADR,REQREC,SPTREC                    
         SPACE 2                                                                
TOTR1    GOTO1 DATAMGR,DMCB,,,,,SPTREC        READ NEXT REQ REC                 
         CLI   DMCB+8,0                                                         
         BE    TOTR2                                                            
         TM    DMCB+8,X'80'                                                     
         BO    TOTR3                                                            
         B     REQIOERR                                                         
         SPACE 2                                                                
TOTR2    CLC   RNUM,=X'FFFF'       IGNORE DUMMYS                                
         BE    TOTR1                                                            
         CLC   RNUM,=C'99'         IGNORE CANCELLED                             
         BE    TOTR1                                                            
*                                                                               
         CLI   LREQOHDR+28,C'*'                                                 
         BE    *+14                                                             
         CLC   RAGY,LREQOHDR+28     FILTER ON AGY                               
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR(4),LREQOHDR                                             
         BZ    *+14                                                             
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE                             
         BNE   TOTR1                                                            
*                                                                               
**       CLI   DDS,1               IF IT'S DDS                                  
**       BNE   NOTDDS                                                           
**       CLC   =C'DDS',LREQOHDR+94 AND WANT TO SEE ALL                          
**       BE    TOTR2B                                                           
**NOTDDS DS    0H                                                               
                                                                                
         TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQOHDR+94    FILTER ON REQUESTOR                         
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    *+14                                                             
         CLC   REQOUT,LREQOHDR+4    FILTER ON OUTPUT TYPE                       
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    *+14                                                             
         CLC   REQDEST,LREQOHDR+11  FILTER ON DESTINATION                       
         BNE   TOTR1                                                            
*                                                                               
TOTR2B   SR    RE,RE                                                            
         IC    RE,REQNUMB                                                       
         SLL   RE,1                                                             
         LA    RF,TOTCTR(RE)       POINT TO COUNTER AND BUMP                    
         LH    R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,0(RF)                                                         
         B     TOTR1                                                            
         SPACE 2                                                                
TOTR3    SR    R5,R5               DISPLAY COUNTERS ON MENU SCREEN              
         LA    R6,TOTCTR                                                        
TOTR4    CHI   R5,256                                                           
         BE    TOTRX                                                            
         STC   R5,DUB                                                           
         BRAS  RE,GETREQID         SEARCH REQTBL FOR BINARY REQ NUM             
         CLI   DUB+3,0                                                          
         BE    TOTR7               NOT IN REQTBL                                
         SPACE 2                                                                
         LA    RE,BVRFRSTH                                                      
         SR    RF,RF                                                            
TOTR5    CLI   0(RE),0             SEARCH SCREEN FOR ALPHA REQ ID               
         BE    TOTR7                                                            
         CLC   8(2,RE),DUB+1                                                    
         BNE   TOTR6                                                            
         LH    R0,0(R6)            MOVE COUNT TO SCREEN FIELD                   
         CVD   R0,DUB                                                           
         UNPK  10(4,RE),DUB+5(3)                                                
         OI    13(RE),X'F0'                                                     
         CLI   10(RE),C'0'                                                      
         BNE   *+8                                                              
         MVI   10(RE),C'='                                                      
         B     TOTR7                                                            
TOTR6    IC    RF,0(RE)            BUMP SCREEN FIELD                            
         AR    RE,RF                                                            
         B     TOTR5                                                            
         SPACE 2                                                                
TOTR7    LA    R5,1(R5)            BUMP REQNUM                                  
         LA    R6,2(R6)            BUMP TABLE                                   
         B     TOTR4                                                            
         SPACE 2                                                                
TOTRX    XC    TEMP(60),TEMP                                                    
         MVC   TEMP(30),=C'TOTAL REQUEST COUNTS DISPLAYED'                      
         B     ENQRX4                                                           
         EJECT                                                                  
*        ADD A NEW REQUEST RECORD TO THE REQUEST CHAIN                          
*                                                                               
NEWREQ   EQU   *                                                                
         L     R1,=A(IDTABLE)                                                   
         A     R1,RELO                                                          
NWQ0     CLC   RNUM,0(R1)     ARE WE WORKING WITH FUDGED IDS ?                  
         BE    NWQ00          YES                                               
         LA    R1,5(R1)                                                         
         CLI   0(R1),0                                                          
         BE    NWQ1                                                             
         B     NWQ0                                                             
NWQ00    MVC   RNUM,2(R1)      SET BACK TO 'REAL' IDS                           
         MVC   REQNUMB(1),4(R1)                                                 
         MVC   LREQOHDR+26(2),2(R1)                                             
NWQ1     BRAS  RE,SETREQ           SET REMAINING REQ RECORD DETAILS             
         LA    RE,BVRNAMEH                                                      
         B     TISINPUT                                                         
                                                                                
* TEST TO ENSURE SOMETHING HAS BEEN INPUT THIS TIME                             
TISINPUT TM    4(RE),X'80'         INPUT THIS TIME                              
         BO    NWQ1A               YES/GO ON                                    
         ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         CHI   R1,9                                                             
         BH    TISINPUT                                                         
         LR    RF,RE                                                            
         ZIC   R1,0(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0             ,,,IF NO MORE FIELDS                         
         BNE   TISINPUT                                                         
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,SPACES                                                    
         MVC   BVRHDR(27),=C'INVALID - NO NEW DATA INPUT'                       
         B     CLEARADR                                                         
*        BE    NWQ1A                                                            
*        MVI   STATUS,0            ..DON'T WRITE REQUEST                        
*        B     EXIT                                                             
*                                                                               
NWQ1A    CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   NWQ2                                                             
         GOTO1 =A(DOSOON),DMCB,(R9),(RC),RR=RELO                                
         CLC   RNUM,=C'I2'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'L2'                                                      
         BNE   *+8                                                              
         MVI   STATUS,0                                                         
         CLI   MULTNUM,0           ARE WE PASSING MULTIPLE REQS                 
         BNE   NWQ5                                                             
         B     EXIT                                                             
NWQ2     DS    0H                                                               
         OI    RHDR+15,X'02'         SET 2-CARD LINKED                          
         TM    RFPSTAT,RFPINUSE    ADDING REQUEST TO GROUP                      
         BZ    NWQ4                                                             
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPRADD                                                
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    CLEARADR                                                         
*                                                                               
         MVC   BVRHDR,SPACES                                                    
         LAY   RE,BVRMSG1          CANT ADD TO SUBMITTED GROUP                  
         MVC   BVRHDR(60),0(RE)                                                 
*                                                                               
         MVC   FERN,=AL2(FE)                                                    
         CLI   QRFPMODE,QRFPNORO                                                
         BNE   CLEARADR                                                         
*                                                                               
         LAY   RE,BVRMSG2          MAX OF 50 REQUESTS                           
         MVC   BVRHDR(60),0(RE)                                                 
         B     CLEARADR                                                         
*                                                                               
NWQ4     DS    0H                                                               
         MVC   ADR,=X'AAAAAAAA'                                                 
*                                                                               
         CLI   COMSCORE,C'Y'                                                    
         JNE   *+8                                                              
         MVI   REQCTRL,1                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMIN,DMADD),REQUEST,ADR,REQREC,,L'REQREC           
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
*                                  REQUEST ADDED                                
************************************************                                
         CLC   =C'BU',RNUM             IF BU                                    
         BNE   NWQ4F                                                            
         CLC   =C'R=',RBOOK        IF REVERSAL                                  
         BE    NWQ4F               NO ECOST                                     
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         CLC   TWAUSRID,=X'250E'       IF BRSA                                  
         BE    NWQ4B                                                            
         CLC   TWAUSRID,=X'2599'       IF MVBROM                                
         BE    NWQ4B                                                            
         CLC   TWAUSRID,=X'2B15'       MVTAPNY, CLT HPG                         
         BNE   *+14                                                             
         CLC   RCLI,=C'HPG'                                                     
         BE    NWQ4B                                                            
*                                                                               
*        CLC   TWAUSRID,=X'0105'       FMNY, CLT RBI                            
*        BNE   *+14                                                             
*        CLC   RCLI,=C'RBI'                                                     
*        BE    NWQ4B                                                            
*                                                                               
         CLC   =C'DU',RAGY             OR MEDIAVEST                             
         BNE   NWQ4F                                                            
*                        NOTIFY VIA EMAIL, THAT DU REQUESTED BILLING            
*        CLI   RBOOK,C'R'          IN CASE OF REVERSAL DON'T BOTHER             
*        BE    NWQ4B                                                            
*        CLC   RCLI,=C'PG3'                                                     
*        BE    *+24                                                             
*        CLC   RCLI,=C'PG4'                                                     
*        BE    *+14                                                             
*        CLC   RCLI,=C'PG5'                                                     
*        BNE   NWQ4B                                                            
*                                                                               
*        CLC   RPRO,=C'ALL'        ONLY ALL PROD, ALL EST                       
*        BNE   NWQ4B                                                            
*        CLC   REST,=C'ALL'                                                     
*        BNE   NWQ4B                                                            
*        LA    RF,OPMSG                                                         
*        MVC   53(1,RF),RMED                                                    
*        MVC   60(3,RF),RCLI                                                    
*        GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'OPMSG,OPMSG)                           
*                                                                               
NWQ4B    TM    ODDMNTS,X'38'           CLIENT EQUIVALENCES COST ?               
         BZ    NWQ4F                   NO                                       
*                                                                               
         BRAS  RE,DOREQEC          SET ECOST REQUEST IN REQRECSV                
*                                                                               
         XC    RHDR,RHDR           ADD IT TO REQUEST FILE                       
         CLI   DDS,1                   IF NOT DDS                               
         BE    *+10                                                             
         MVC   RHDR+11(2),REQDEST      SET DESTINATION                          
         MVC   RNUM(80),REQRECSV      MOVE NEW CARD INTO REQ AREA               
         MVC   RCARD2(80),SPACES        CLEAR 2ND REQUEST CARD                  
         XC    TEMP,TEMP                                                        
         GOTO1 DATAMGR,DMCB,DMADD,REQUEST,TEMP,RHDR,0                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
NWQ4F    EQU   *                                                                
*&&DO                                                                           
         CLC   =C'GZ',RAGY         IS IT AGENCY GZ?                             
         BNE   NWQ4X                - NOPE                                      
*                                                                               
         CLC   =C'B1',RNUM                                                      
         BNE   NWQ4X                                                            
         CLI   RMED,C'T'                                                        
         BNE   NWQ4X                                                            
         CLC   RCLI,=C'G1M'                                                     
         BNE   NWQ4X                                                            
         LA    RF,GZMSG                                                         
         MVC   38(1,RF),RMED                                                    
         MVC   45(3,RF),RCLI                                                    
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'GZMSG,GZMSG)                           
*&&                                                                             
NWQ4X    EQU   *                                                                
**************************************************                              
***      CLC   =C'SX',RNUM         IF SX OR SC, SEND A 2ND REQUEST              
***      BE    NWQ4D                                                            
***      CLC   =C'SC',RNUM                                                      
***      BNE   *+8                                                              
*NWQ4D    CLI   RCARD2+19,C'Y'      HAVE WE SENT A 2ND REQ ALREADY?             
***      BE    NWQ4F                                                            
***      MVI   RCARD2+19,C'Y'                                                   
***      B     CHKREQ                                                           
*NWQ4F    EQU   *                                                               
*                                                                               
         CLI   MLTREQSW,C'Y'       ARE WE PASSING MULTIPLE REQS                 
         BNE   SAVEADR                                                          
         CLI   MULTNUM,0           MULTNUM=MULT REQ CNTR                        
         BE    SAVEADR                                                          
NWQ5     CLI   MULTNUM,10                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   REQREC,REQRECSV     SET BEFORE POSTVAL REQREC                    
         MVC   RMARK(4),STATSV                                                  
         MVC   RSTA(5),STATSV+4                                                 
         MVC   STATSV(L'STATSV-9),STATSV+9                                      
         ZIC   R1,MULTNUM                                                       
         BCTR  R1,0                                                             
         STC   R1,MULTNUM                                                       
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         B     CHKREQ                                                           
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
DISPMAX  DC    H'13'                                                            
REQUEST  DC    CL8'REQUEST'                                                     
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
REQDDS   EQU   X'04'                                                            
         SPACE 2                                                                
NUMINV   EQU   10                  REQUEST NUMBER INVALID                       
*                                                                               
         LTORG                                                                  
*                                                                               
OPMSG    DC    C'AUTONOTE*YKVA,TZIH:*DU REQUESTED BU. MEDIA=X, CLT=XXX'         
*                                                                               
*  FUDGE ID/ ACTUAL ID/ REQUEST NUMBER                                          
IDTABLE  DS    0CL5                                                             
         DC    C'N2',C'I2',AL1(23)                                              
         DC    C'W4',C'D4',AL1(160)                                             
         DC    C'WC',C'DC',AL1(158)                                             
         DC    C'W2',C'M2',AL1(112)                                             
         DC    C'W9',C'M9',AL1(119)                                             
         DC    C'm4',C'M4',AL1(114)                                             
***      DC    C'NN',C'NN',AL1(203)                                             
         DC    X'00'                                                            
*                                                                               
GZMSG    DC    C'AUTONOTE*YKVA,TZIH:*GZ REQUESTED B1. MEDIA=X, CLT=XXX'         
*EMSG    DC    C'AUTONOTE*YKVA:*MC REQUESTED MANUAL BILL. CLT=XXX,PRD=X+        
               XX, EST=XXX'                                                     
*                                                                               
         DS    0H             ALIGN ON HALFWORD-ZERO FOR LARL                   
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
         EJECT                                                                  
B8PROF   NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP,TEMP                                                        
         MVI   TEMP+30,C'S'           B8 PROFILE READ                           
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+32(2),=C'B8'                                                
         MVC   TEMP+34(6),RAGY                                                  
         CLI   CLIOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
         CLI   TEMP+3,C'Y'        DRAFT BILL INCLUDE REGISTER?                  
         BNE   B8NO                                                             
         SR    R1,R1                                                            
B8NO     LTR   R1,R1                                                            
B8XX     XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
GTPROAGY NTR1  BASE=*,LABEL=*                                                   
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(2),RAGY     READ AGY LEVEL PROFILE                       
         L     R5,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,(X'90',TEMP+30),TEMP,(0(R3),(R5))                   
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
GTPROCLT NTR1  BASE=*,LABEL=*                                                   
         NI    TEMP+30,X'BF'              LOWER CASE                            
         MVC   TEMP+34(6),RAGY                                                  
         CLI   CLIOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
VALCROSS NTR1  BASE=*,LABEL=*                                                   
         L     RF,ASAVE            CANADA ONLY                                  
         CLI   CANAGY-TWAD(RF),C'C'                                             
         BNE   VALCRSX                                                          
         CLC   RSTRD(6),=X'FBF9F1F2F2F9' START AFTER 12/29/19, DONE             
         BH    VALCRSX                                                          
         CLI   RENDD,C' '          IS THERE END DATE?                           
         BNH   VALCRSX             NO, DONE                                     
         CLC   RENDD,=X'FBF9F1F2F3F0' IS IT BEFORE DEC30/19?                    
         BL    VALCRSX                                                          
         MVI   ROUTNUM,X'09'       CURSOR TO DATE                               
         MVC   FERN,=AL2(FLDINV)                                                
         B     *+6                                                              
VALCRSX  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
VVALR02  NTR1  BASE=*,LABEL=*                                                   
         CLI   RO2,C'T'                                                         
         BNE   *+12                                                             
         MVI   RO1,C'T'                                                         
         MVI   RO2,C'Y'                                                         
*                                                                               
         CLI   RO2,C'Y'                                                         
         BE    VLR02                                                            
         BRAS  RE,CKUPDT                                                        
         BNE   VLR02                                                            
         MVI   ROUTNUM,X'74'                                                    
         MVC   FERN,=AL2(1250)                                                  
*                                                                               
VLR02    CLC   BVROUT(4),=C'SOON'      IF SOON                                  
         BNE   VLR02A                                                           
         MVI   ROUTNUM,X'74'                                                    
         CLI   RO2,C'Y'                MUST BE TEST RUN                         
         BE    VLR02A                                                           
         MVC   FERN,=AL2(FLDINV)                                                
         B     VALRXX                                                           
VLR02A   CLI   RHUT,X'40'                                                       
         BNH   VALRXX                                                           
         MVI   ROUTNUM,X'F9'                                                    
         CLC   RHUT,RBOOK          INV # IN RHUT > INV # IN RBOOK ?             
         BNL   VALRXX              NO                                           
         MVC   FERN,=AL2(FLDINV)                                                
VALRXX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR03  NTR1  BASE=*,LABEL=*                                                   
         CLC   RAGY,=C'SJ'                                                      
         BE    VAL03X                                                           
         CLC   RCLI,=C'PMT'                                                     
         BE    VAL03X                                                           
         CLC   RCLI,=C'YSP'                                                     
         BE    VAL03X                                                           
         CLC   RCLI,=C'ALT'                                                     
         BE    VAL03X                                                           
         CLC   RCLI,=C'PCS'                                                     
         BE    VAL03X                                                           
         CLC   RCLI,=C'PCR'                                                     
         BE    VAL03X                                                           
         MVI   ROUTNUM,X'02'           NO-ERROR                                 
VAL03X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR24  NTR1  BASE=*,LABEL=*                                                   
         CLC   RCLI,=C'CA1'     MUST BE CLIENT CA1 - CONTINENTAL                
         BE    VR24X                                                            
         CLC   RCLI,=C'CA2'          OR CLIENT CA2 - CONTINENTAL                
         BE    VR24X                                                            
         CLC   RCLI,=C'CA3'          OR CLIENT CA3 - CONTINENTAL                
         BE    VR24X                                                            
*                                                                               
         MVI   ROUTNUM,X'02'         CURSOR TO CLIENT                           
VR24X    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR27  NTR1  BASE=*,LABEL=*                                                   
         CLC   RCLI,=C'COS'     MUST BE CLIENT COS - L'OREAL                    
         BE    VR27C                                                            
         CLC   RCLI,=C'CO2'          OR CLIENT CO2 - L'OREAL 2010               
         BE    VR27C                                                            
         CLC   RCLI,=C'CSH'          OR CLIENT CSH - L'OREAL HISPANIC           
         BE    VR27C                                                            
         CLC   RCLI,=C'LRH'          OR CLIENT LRH - L'OREAL                    
         BE    VR27C                                                            
         CLC   RCLI,=C'LCP'          OR CLIENT LCP - L'OREAL                    
         BE    VR27C                                                            
         CLC   RCLI,=C'LCH'          OR CLIENT LCH - L'OREAL                    
         BE    VR27C                                                            
*                                                                               
         MVI   ROUTNUM,X'02'         CURSOR TO CLIENT                           
VR27X    XIT1                                                                   
*                                                                               
VR27C    DS    0H                                                               
         CLC   RPRO,SPACES                                                      
         BNE   *+10                                                             
         MVC   RPRO(3),=C'ALL'                                                  
*                                                                               
         CLC   REST(3),=C'ALL'      IF ALL INPUT CHANGE TO 001255               
         BE    VR27D                                                            
         CLC   REST(3),SPACES                                                   
         BNE   VR27D5                                                           
VR27D    MVC   REST(6),=C'001255'                                               
         B     VR27DX                                                           
*                                                                               
VR27D5   CLC   REST+3(3),=C'255'    MIGHT BE LEFT OVER FROM DEFAULT             
         BNE   *+10                                                             
         MVC   REST+3(3),SPACES     CLEAR IT                                    
VR27DX   CR    RE,RE                SET CC CODE                                 
         B     VR27X                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
VVALR41  NTR1  BASE=*,LABEL=*                                                   
         CLI   RO1,C'P'               SEE IF DOING PRODUCT LIST                 
         BNE   VR41XX                                                           
         OC    RPRO,=X'404040'                                                  
         CLC   RPRO,=X'404040'                                                  
         BNE   VR41C                                                            
         MVC   RPRO(3),=C'ALL'                                                  
*                                                                               
VR41C    CLC   RPRO(3),=C'ALL'                                                  
         BNE   VR41XX                                                           
         CLC   BVROUT(4),=C'SOON'    SEE IF SOON                                
         BNE   VR41XX                                                           
         CLC   RCLI(3),=C'ALL'       DISALLOW ALL                               
         BE    VR41ERR                                                          
         CLC   RCLI(2),=C'*-'        DISALLOW ALL EXCEPT ONE OFFICE             
         BE    VR41ERR                                                          
         CLI   RCLI,C'$'             OR OFFICE LIST REQUESTS                    
         BNE   VR41XX                                                           
*                                                                               
VR41ERR  MVC   FERN(2),=AL2(7)                                                  
         MVI   ROUTNUM,X'02'         CURSOR TO CLIENT                           
VR41XX   XIT1                                                                   
         LTORG                                                                  
VVALR42  NTR1  BASE=*,LABEL=*                                                   
         EJECT                                                                  
*                                                                               
         CLC   RPRO,SPACES     IF PRODUCT IS MISSING SET TO ALL                 
         BNE   *+10                                                             
         MVC   RPRO,=C'ALL'                                                     
         B     VVR42D                                                           
*                                                                               
*        IF I NEED TO CHECK FOR CERTAIN CLTS                                    
*        DO SO ABOVE THE PRIOR INSTRUCTION                                      
*                                                                               
VVR42E   MVI   ROUTNUM,X'02'       CURSOR TO CLIENT                             
         MVC   FERN,=AL2(FLDINV)                                                
         B     VVR42X                                                           
*                                                                               
VVR42D   CLC   BVROUT(4),=C'SOON'                                               
         BNE   VVR42X                                                           
         CLI   RO6,C'Y'             MUST BE TEST FOR SOON REQ                   
         BE    VVR42X                                                           
         MVI   ROUTNUM,X'74'       CURSOR TO TEST RUN                           
         MVC   FERN,=AL2(FLDINV)                                                
VVR42X   XIT1                      EXIT WITH CONDITON CODE                      
         LTORG                                                                  
         EJECT                                                                  
VVALR91  NTR1  BASE=*,LABEL=*                                                   
         CLI   RENDD,C' '                                                       
         BE    VR91C                                                            
         CLC   R5055(2),=C'NO'     CUT-OFF DATE ONLY FOR 'NO' CUR MTH           
         BE    VR91C                                                            
         MVI   ROUTNUM,X'3B'                                                    
         LTR   RE,RE               SET CC NEQ                                   
         B     VR91X                                                            
*                                                                               
VR91C    CLC   RPRO,=C'ALL'                                                     
         BE    *+16                                                             
         CLI   RDIV,C' '           PRODUCT GROUP?                               
         BH    *+8                                                              
         MVI   RO2,C'N'            SET ALL PRDS TO NO                           
*                                                                               
         CLI   RO3,X'40'           PROD SUMMARY REQUEST GETS FUNKY              
         BNH   VR91J                                                            
         CLI   RO3,C'N'            IF N, CLEAR IT AND FORGET IT                 
         BNE   *+12                                                             
         MVI   RO3,X'40'                                                        
         B     VR91J                                                            
         MVI   RO3,X'40'           ELSE-CLEAR RO3                               
         MVI   RO4,C'B'            OVERRIDE RO4                                 
         MVI   RO1,C'N'            OVERRIDE RO1                                 
         CLC   =C'ALL',RPRO        AND RO2 IF RPRO=ALL                          
         BNE   *+8                                                              
         MVI   RO2,C'Y'                                                         
VR91J    CR    RE,RE               SET CC EQ                                    
*                                                                               
VR91X    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR102 NTR1  BASE=*,LABEL=*                                                   
         MVI   ROUTNUM,2           POINT TO CLIENT                              
         MVC   HALF,RNUM           READ SX PROFILE                              
         BAS   RE,RDPROF                                                        
         CLI   TEMP,C'Y'           CLIENT SPECIFIC ONLY ?                       
         BNE   *+24                                                             
         CLC   RCLI,=C'ALL'        CANNOT REQUEST 'ALL'                         
         BNE   *+14                                                             
         MVC   FERN,=AL2(FLDINV)                                                
         B     VR102X                                                           
*                                                                               
         CLC   RMARK(4),RPRO1      OLD MKT CAN'T MATCH NEW MKT                  
         BNE   VR102X                                                           
         MVI   ROUTNUM,X'06'       CURSOR TO MKT                                
         MVC   FERN,=AL2(MKTMISM)                                               
VR102X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR74  NTR1  BASE=*,LABEL=*                                                   
         MVI   RMED,C'T'           ARE WE IN SPOT OR NET                        
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   *+8                                                              
         MVI   RMED,C'N'                                                        
         XIT1                                                                   
         DROP  R1,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
VVALR07  NTR1  BASE=*,LABEL=*                                                   
         MVI   ROUTNUM,X'09'                                                    
         GOTO1 ADDAY,DMCB,RSTRD,TEMP,F'14'                                      
         CLC   RENDD,TEMP                                                       
         BNH   VALR07C                                                          
         MVC   FERN,=AL2(103)                                                   
         B     VVALR07X                                                         
VALR07C  CLC   RBOOK,SPACES             IF RBOOK = SPACES                       
         BNE   VVALR07X                                                         
         CLI   RBOOK1,X'40'        RERATE TYPE MUST = X'40'                     
         BE    VVALR07X                                                         
         CLI   RBOOK1,C'P'         RERATE TYPE MUST = P                         
         BE    VVALR07X                                                         
         MVI   ROUTNUM,X'55'                                                    
         LTR   RE,RE                                                            
         B     *+6                                                              
VVALR07X CR    RE,RE               EXIT WITH CONDITON CODE                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR08  NTR1  BASE=*,LABEL=*                                                   
         MVI   ROUTNUM,X'55'                                                    
         CLI   RBOOK1,C'P'        IF RERATE TYPE=PUR                            
         BNE   *+14                                                             
         CLC   RBOOK(6),SPACES     NO BOOK/HUT                                  
         BNE   VVALR8X             EXIT WITH CC NEQ                             
*                                                                               
         CLI   RBOOK1,C'I'         IF RERATE TYPE=INV                           
         BNE   *+20                                                             
         CLC   =C'ACT',RBOOK       BOOK CANNOT = ACT                            
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     VVALR8X                                                          
*                                                                               
         CLI   RBOOK1,C'A'         IF RERATE TYPE=AFF                           
         BNE   *+14                                                             
         CLC   =C'ACT',RBOOK       BOOK CANNOT = ACT                            
         BNE   VVALR8X                                                          
*                                                                               
         CR    RB,RB                                                            
VVALR8X  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR14  NTR1  BASE=*,LABEL=*                                                   
         CLC   RAGY,=C'DU'         FOR DU ONLY PG1, PG2, *O                     
         BNE   VALR14H9                                                         
         CLI   RMED,C'N'                                                        
         BNE   VVALR14X                                                         
         CLC   RCLI,=C'PG1'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG2'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG3'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG4'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG5'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PGG'                                                     
         BE    VVALR14X                                                         
         CLC   =C'*O',RCLI                                                      
         BE    VVALR14X                                                         
VALR14H9 CLC   RAGY,=C'H9'         FOR H9 PGB OR HPG                            
         BNE   VALR14TC                                                         
         CLC   RCLI,=C'PGB'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PGG'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG6'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'HPG'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'HP1'                                                     
         BE    VVALR14X                                                         
         CLI   RMED,C'N'                                                        
         BNE   *+10                                                             
         LTR   RE,RE                                                            
         B     VVALR14X            SET CC NEWQ                                  
*                                                                               
         CLC   RCLI,=C'PG '                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'PG1'                                                     
         BE    VVALR14X                                                         
VALR14TC CLC   RAGY,=C'TC'         FOR TRC ONLY PGT                             
         BNE   VALR14O0                                                         
         CLC   RCLI,=C'PGT'                                                     
         BE    VVALR14X                                                         
VALR14O0 CLC   RAGY,=C'O0'         FOR O0 ONLY PGM                              
         BNE   VALR14DR                                                         
         CLC   RCLI,=C'PGM'                                                     
         BE    VVALR14X                                                         
*****    CLC   RCLI,=C'PSM'        NO-OPED (FOR NOW)                            
*****    BE    VVALR14X                                                         
VALR14DR CLC   RAGY,=C'DR'         FOR DR ONLY PG                               
         BNE   VALR14T1                                                         
         CLC   RCLI,=C'PG '                                                     
         BE    VVALR14X                                                         
*                                                                               
VALR14T1 CLC   RAGY,=C'T1'         FOR TCH1 ONLY PG1                            
         BNE   VALR14HY                                                         
         CLC   RCLI,=C'PG1'                                                     
         BE    VVALR14X                                                         
*                                                                               
VALR14HY CLC   RAGY,=C'HY'         FOR HY ONLY PG1 AND P12                      
         BNE   VALR14UB                                                         
         CLC   RCLI,=C'PG1'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'P12'                                                     
         BE    VVALR14X                                                         
         CLC   RCLI,=C'P13'                                                     
         BE    VVALR14X                                                         
*                                                                               
VALR14UB CLC   =C'SOON',BVROUT     SOON REQUEST?                                
         BE    VALR14NE            YES - NO SOON GT FOR UB/OO/OU                
         CLC   RAGY,=C'UB'         AGENCY UB?                                   
         BE    VVALR14X            YES - ALL CLIENTS VALID                      
         CLC   RAGY,=C'OO'         AGENCY OO?                                   
         BE    VVALR14X            YES - ALL CLIENTS VALID                      
         CLC   RAGY,=C'OU'         AGENCY OU?                                   
         BE    VVALR14X            YES - ALL CLIENTS VALID                      
*                                                                               
VALR14NE LTR   RE,RE               SET CONDITON CODE TO NEQ                     
*                                                                               
VVALR14X XIT1                      EXIT WITH CONDITON CODE                      
         LTORG                                                                  
         EJECT                                                                  
VVALR20  NTR1  BASE=*,LABEL=*                                                   
         CLC   RCLI,=C'SPR'     MUST BE CLIENT SPR - SPRINT INTERFACE           
         BE    VVR20D                                                           
         CLC   RCLI,=C'BMB'     OR CLIENT BMB - SPRINT INTERFACE                
         BE    VVR20D                                                           
         CLI   RMED,C'N'        NETWORK?                                        
         BNE   *+18             ONLY SPR, BMB, OR SBV FOR NET                   
         CLC   RCLI,=C'SBV'     OR CLIENT SBV                                   
         BE    VVR20D                                                           
         B     VVR20E                                                           
*                                                                               
         CLC   RCLI,=C'BOD'     OR CLIENT BOD - SPRINT INTERFACE                
         BE    VVR20D                                                           
         CLC   RCLI,=C'SPD'     ALSO SPD IS ALLOWED FOR MEDIA T                 
         BNE   VVR20E                                                           
         CLI   RMED,C'T'                                                        
         BE    VVR20D                                                           
*                                                                               
VVR20E   MVI   ROUTNUM,X'02'       CURSOR TO CLIENT                             
         MVC   FERN,=AL2(FLDINV)                                                
         B     VVALR20X                                                         
*                                                                               
VVR20D   CLC   BVROUT(4),=C'SOON'                                               
         BNE   VVALR20X                                                         
         CLI   RO6,C'Y'             MUST BE TEST FOR SOON REQ                   
         BE    VVALR20X                                                         
         MVI   ROUTNUM,X'74'       CURSOR TO TEST RUN                           
         MVC   FERN,=AL2(FLDINV)                                                
VVALR20X XIT1                      EXIT WITH CONDITON CODE                      
         LTORG                                                                  
         EJECT                                                                  
VVALR103 NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP,TEMP                                                        
         MVC   TEMP(L'KEY+L'KEYS),KEY  SAVE KEY & KEYS JUST IN CASE             
         XC    KEY,KEY                                                          
         XC    KEYD,KEYD                                                        
K@       USING STAFXRCD,KEY                                                     
         MVI   K@.STFKTYP,STFKTYQ     (X'0D') RECORD TYPE                       
         MVI   K@.STFKSBTY,STFKSBTQ   (X'6E') SUBRECORD TYPE                    
         MVC   K@.STFKAGMD,BAGYMD     AGENCY/MEDIA                              
         MVC   K@.STFKCLI,BCLT     CLIENT (PACKED)                              
         CLC   RCLI,=CL3'ALL'      ALL CLIENTS?                                 
         BNE   *+10                NOPE                                         
         MVC   K@.STFKCLI,=X'FFFF'                                              
*                                                                               
         LA    R5,TEMP+30                                                       
         XC    TEMP+30(40),TEMP+30                                              
         USING STAPACKD,R5                                                      
         MVI   STAPACT,C'P'         PACK                                        
         MVC   STAPAGY,AGY                                                      
         MVC   STAPMED,REQMED                                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,RMARK        MARKET                                     
         MVC   STAPQSTA(4),RSTA      OLD STATION                                
         LR    R1,R5                                                            
         GOTO1 STAPACK,(R1)                                                     
******   GOTO1 STAPACK,DMCB,(C'P',FULL),DUB,TEMP+30                             
         MVC   K@.STFKMKT,STAPMKT     MARKET                                    
         MVC   K@.STFKNSTA,STAPSTA    OLD STATION (NEW FOR STAFIX REC)          
         MVC   STAPQMKT,RMARK        MARKET                                     
         MVC   STAPQSTA(4),RBOOK     NEW STATION                                
         LR    R1,R5                                                            
         GOTO1 STAPACK,(R1)                                                     
*        GOTO1 STAPACK,DMCB,(C'P',FULL),DUB,TEMP+30                             
         MVC   K@.STFKOSTA,STAPSTA    NEW STATION (OLD FOR STAFIX REC)          
         DROP  R5                                                               
*                                                                               
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         DROP  K@                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                   
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY,KEYD                                                         
         BNE   VR103X                                                           
         L     R4,AIORFP           USE THIS IO FOR NETDEF REC                   
         USING STAFXRCD,R4                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,(R4),       X        
               SPTWORK                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'KEY+L'KEYS),TEMP       RESTORE KEY & KEYS                  
         LA    R6,STFFRST                                                       
         SR    R1,R1                                                            
VR103C   IC    R1,1(R6)                                                         
         AR    R1,R6               R1 POINTS TO NEXT ELEM (IF EXISTS)           
         CLI   0(R1),STFIDELQ      HAVE MORE DETAILS ELEMENTS (X'10')?          
         BNE   VR103D              NO                                           
         LR    R6,R1                                                            
         B     VR103C                                                           
         USING STFIDELD,R6                                                      
VR103D   MVI   GETDAY,1            MAKE SUNDAY FIRST DAY OF WEEK                
*                                                                               
         GOTO1 DATCON,DMCB,(2,STFIDDAT),(0,TEMP+30)   SET YYMMDD DATE           
         GOTO1 =V(NSIWEEK),DMCB,TEMP+30,GETDAY,ADDAY,DATCON,RR=RELO             
         MVC   TEMP+40(1),0(R1)    WEEK NUMBER IN BINARY FOR PREV REQ           
         MVC   TEMP+41(1),4(R1)    YEAR NUMBER IN BINARY FOR PREV REQ           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TEMP+30)   TODAYS DATE                      
         GOTO1 =V(NSIWEEK),DMCB,TEMP+30,GETDAY,ADDAY,DATCON,RR=RELO             
         MVC   TEMP+45(1),0(R1)    WEEK NUMBER IN BINARY FOR TODAY              
         MVC   TEMP+46(1),4(R1)    YEAR NUMBER IN BINARY FOR TODAY              
*                                                                               
         CLC   TEMP+40(2),TEMP+45  ARE THEY BOTH IN THE SAME WEEK?              
         BNE   VR103X              NO, THIS IS FINE TO REQUEST                  
         MVC   FERN,=AL2(FE)       SET MY ERROR MSG                             
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(41),=C'REVERSE STATION FIX WAS REQSTED THIS WEEK'         
         B     VR103X                                                           
         DROP  R4,R6                                                            
*                                                                               
*******                                                                         
*                                                                               
*                                                                               
*                                                                               
VR103X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR132 NTR1  BASE=*,LABEL=*                                                   
         TM    SEDSAVE,X'04'       SEE IF YYMM INPUT                            
         BZ    VR132A                                                           
         MVC   RSTRD+4(2),SPACES        YES - RESET DAYS TO SPACES              
         MVC   RENDD+4(2),SPACES                                                
*                                                                               
VR132A   DS    0H                                                               
         CLC   REST1,=C'   '        FILTERS                                     
         BE    VR132C              NO                                           
         CLC   RSTRD,SPACES                                                     
         BNE   VR132C                                                           
VR132B   MVC   FERN,=AL2(FLDMIS)         REQUIRE DATES FOR FILTERS              
VR132B2  MVI   ROUTNUM,X'09'                                                    
         B     VR132X                                                           
*                                                                               
VR132C   CLI   RO4,C'Y'            $ FOR REQ MTHS ONLY                          
         BNE   VR132X              NO                                           
         CLC   RSTRD,SPACES                                                     
         BE    VR132B              DATES MISSING                                
         CLC   RSTRD+4(2),SPACES                                                
         BE    VR132E              DAY                                          
         MVI   ROUTNUM,X'09'                                                    
         MVC   FERN,=AL2(FLDINV)       NO DAY ALLOWED FOR THIS OPTION           
         B     VR132X                                                           
*                                                                               
VR132E   CLI   RO3,C'D'                                                         
         BNE   VR132X                                                           
         MVC   FERN,=AL2(FLDINV)     DEMOS ONLY INVALID WITH THIS OPT           
         MVI   ROUTNUM,X'54'                                                    
*                                                                               
VR132X   XIT1                                                                   
         LTORG                                                                  
VVALR205 NTR1  BASE=*,LABEL=*                                                   
         EJECT                                                                  
*                                                                               
         MVI   RO2,C'N'            FOR NISSAN FILE                              
         MVI   RO3,C'N'                                                         
         CLC   REST(3),=C'NO '                                                  
         BNE   *+10                                                             
         MVC   REST(3),=C'ALL'                                                  
*                                                                               
         B     VVR205D                                                          
*                                                                               
*        IF I NEED TO CHECK FOR CERTAIN CLTS                                    
*        DO SO ABOVE THE PRIOR INSTRUCTION                                      
*                                                                               
VVR205E  MVI   ROUTNUM,X'02'       CURSOR TO CLIENT                             
         MVC   FERN,=AL2(FLDINV)                                                
         B     VVR205X                                                          
*                                                                               
VVR205D  DS    0H                                                               
*&&DO                                                                           
* REMOVE CODE PREVENTING LIVE SOON REQUESTS TO AN OUTPUT FILE                   
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VVR205X                                                          
         CLI   RO1,C'N'             MUST BE TEST FOR SOON REQ                   
         BE    VVR205X                                                          
         MVI   ROUTNUM,X'9C'       CURSOR TO TEST RUN                           
         MVC   FERN,=AL2(FLDINV)                                                
*&&                                                                             
VVR205X  XIT1                      EXIT WITH CONDITON CODE                      
         LTORG                                                                  
VVALR210 NTR1  BASE=*,LABEL=*                                                   
         CLI   RENDD,C' '          WAS END DATE INPUT?                          
         BH    *+10                                                             
         MVC   RENDD,RSTRD         IF NOT COPY START INTO END                   
*                                                                               
         CLI   RO1,C'N'                                                         
         BE    VR210K                                                           
         BRAS  RE,CKUPDT                                                        
         BNE   VR210K                                                           
         MVI   ROUTNUM,X'9C'                                                    
VR210E   MVC   FERN,=AL2(1250)                                                  
         B     VR210X                                                           
VR210K   CLI   RO1,C'Y'                                                         
         BE    VR210T                                                           
         CLI   RO1,C'N'                                                         
         BE    VR210X                                                           
         MVC   RO2,RO1                                                          
         MVI   RO1,C'Y'                                                         
*                                                                               
VR210T   DS    0H                                                               
         CLI   RO5,C'S'         SOON BILLS ONLY?                                
         BNE   VR210X           DISALLOW A TAPE REQUEST                         
         MVI   ROUTNUM,X'9C'    CURSOR TO TAPE FIELD                            
         MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
VR210X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VVALR212 NTR1  BASE=*,LABEL=*                                                   
         CLI   RNUM+62,C'F'        FAX OPTION ON                                
         BE    V212FX                                                           
         CLI   RNUM+62,C'S'        FAX OPTION ON                                
         BNE   V212                                                             
V212FX   CLC   =C'DIRECT',BVROUT   HAS USER SET OUTPUT ?                        
         BE    V212                                                             
         CLC   =C'BFXS10',BVROUT                                                
         BE    V212                                                             
******** MVC   HALF,=C'NV'                                                      
         XC    TEMP(20),TEMP       NO / READ NV PROFILE                         
         MVC   TEMP+30(2),=C'SO'                                                
         MVC   TEMP+32(2),=C'NV'                                                
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
V212PROF GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
******** BAS   RE,RDPROF                                                        
         MVC   BVROUT(6),=C'DIRECT'                                             
         CLI   TEMP+4,C'Y'                                                      
         BE    V212FOK             FAX OPT OK                                   
         MVC   BVROUT(6),=C'BFXS10'                                             
         CLI   TEMP+4,C'N'                                                      
         BE    V212FOK             FAX OPT OK                                   
         XC    BVROUT(6),BVROUT                                                 
         MVI   ROUTNUM,X'9F'                                                    
         MVC   FERN,=AL2(645)      ERROR-MUST HAVE NV PROFILE SET               
         B     VR212X                                                           
V212FOK  OI    BVROUTH+6,X'80'                                                  
         MVC   REQOUT(6),=C'DIRECT'     SET TO REQ CARD                         
                                                                                
V212     CLC   RPRO,=C'POL'                                                     
         BE    VR212AA                                                          
         CLI   PROSAVE,X'21'       PRO=XXX AND NON SPEC CLI                     
         BNE   VR212AA                                                          
         MVI   ROUTNUM,X'5D'                                                    
         B     VR212INV                                                         
VR212AA  CLI   RPRO,C' '                                                        
         BNE   *+10                                                             
         MVC   RPRO,=C'ALL'                                                     
         CLI   RMARK,C' '                                                       
         BNE   VR212A                                                           
         MVC   RSTA(3),=C'ALL'                                                  
         MVC   RMARK(3),=C'ALL'                                                 
VR212A   CLI   RSTA,C' '                                                        
         BNE   *+10                                                             
         MVC   RSTA(3),=C'ALL'                                                  
         CLI   RO1,C'N'                                                         
         BNE   VR212D                                                           
         CLI   RO2,C'B'                                                         
         BE    VR212ERR                                                         
         CLI   RO2,C'L'                                                         
         BE    VR212ERR                                                         
VR212D   CLC   =C'ALL',RCLI        CLIENT=ALL/PROD=X NOT ALLOWED                
         BNE   VR212F                                                           
         CLI    RPRO,C'X'                                                       
         BNE   VR212F                                                           
         MVI   ROUTNUM,X'5D'                                                    
         B     VR212INV                                                         
VR212F   CLI   RBOOK,C' '                                                       
         BE    VR212G                                                           
         GOTO1 DATCON,DMCB,(5,TEMP),(0,TEMP)                                    
         CLC   RBOOK(6),TEMP                  IF PRIOR TO TODAY                 
         BNL   VR212G                        ERROR                              
         MVI   ROUTNUM,X'AD'                                                    
         B     VR212INV                                                         
VR212G   GOTO1 DATCON,DMCB,(5,TEMP),(0,TEMP)                                    
         PRINT GEN                                                              
         GOTO1 GETBROAD,DMCB,(1,TEMP),TEMP+6,GETDAY,ADDAY                       
         PRINT NOGEN                                                            
         CLC   RENDD,TEMP+12                 MUST BE SAME MONTH                 
         BNH   VR212J                        ERROR                              
         MVI   ROUTNUM,X'09'                                                    
         B     VR212INV                                                         
*                                                                               
VR212ERR MVI   ROUTNUM,X'9F'                                                    
         MVC   FERN,=AL2(62)       ERROR-NEEDS GROSS OR GROSS AND NET           
         B     VR212X                                                           
*                                  WITH LETTERS                                 
VR212J   CLI   RMED,C'N'           FOR MEDIA 'N' DISALLOW MKT                   
         BNE   VR212X                                                           
         CLI   RSTA,C' '           ANYTHING IN STATION?                         
         BNH   *+14                                                             
         CLC   =C'ALL',RSTA                                                     
         BNE   VR212X              IF STATION INPUT, DON'T CHK MKT              
*                                                                               
         CLI   RMARK,C'0'          ANY MARKET?                                  
         BL    VR212X                                                           
         MVI   ROUTNUM,X'5E'       YES, INVALID                                 
         B     VR212INV                                                         
*                                                                               
VR212INV MVC   FERN,=AL2(FLDINV)   INVALID                                      
*                                                                               
VR212X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
SETREQ   NTR1  BASE=*,LABEL=*                                                   
         MVI   RHDR+15,X'01'       SET CHAIN REQUEST                            
         CLC   RCARD2,SPACES                                                    
         BE    *+12                                                             
         OI    RHDR+15,X'10'       SET 2 REQUEST CARDS IN HEADER                
         MVI   RREPNO+1,C'*'       LET CONTROLLER KNOW ANOTHER COMIN            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  VALIDATES SYMBOLIC ESCAPE SEQUENCE - EXPECTS R5=SYMBOLIC NAME FIELD          
*                                                                               
VALRFP   NTR1  BASE=*,LABEL=*                                                   
         TM    RFPSTAT,RFPINUSE    ..$RPF                                       
         BZ    VRFPNO                                                           
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         ZIC   R0,RFPVNUMS                                                      
VALRFP5  CLC   0(3,R5),RFPVSYME                                                 
         BE    VALRFPX                                                          
         LA    R4,RFPVSYML(R4)                                                  
         BCT   R0,VALRFP5                                                       
VRFPNO   LTR   RE,RE                                                            
*                                                                               
VALRFPX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        CONVERT BINARY REQNUM IN DUB(1) TO ALPHA IN DUB+1(2)                   
*                                                                               
GETREQID NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB                                                           
         STC   R0,DUB                                                           
         UNPK  DUB+1(2),DUB+6(2)                                                
         OI    DUB+2,X'F0'                                                      
         MVI   DUB+3,0             SET NOT FOUND FLAG AND NUMBER VALUE          
         SR    R1,R1                                                            
         L     R5,AREQTBL                                                       
         SPACE 2                                                                
GETRID1  CLI   0(R5),0             SEARCH REQTBL FOR BINARY REQ NUM             
         BE    GETRIDX                                                          
         CLC   DUB(1),1(R5)                                                     
         BE    GETRID2                                                          
         IC    R1,0(R5)                                                         
         AR    R5,R1                                                            
         B     GETRID1                                                          
         SPACE 2                                                                
GETRID2  ST    R5,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         IC    R1,0(R5)            POINT TO LAST TWO BYTES OF ENTRY             
         AR    R5,R1                                                            
         SH    R5,=H'2'                                                         
         MVC   DUB+1(2),0(R5)      RETURN REQ ALPHA ID                          
         SPACE 2                                                                
GETRIDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE TO FORMAT REQUEST DATA IN SCREEN DISPLAY LINE                  
*                                                                               
ENQDISP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,DISPADR                    R4=A(NEXT SCR DISP LINE)           
         USING DISPLD,R4                                                        
         XC    DLINE,DLINE                                                      
**       CLC   RPRO,=C'POL'        PXZ TEST                                     
**       BE    *+6                                                              
**       DC    H'0'                PXZ TEST                                     
         OC    RCARD2,RCARD2                                                    
         BZ    *+10                                                             
         XC    DLINE2,DLINE2                                                    
         MVI   DLCANC,C' '                                                      
         MVC   DLNUM,RNUM                                                       
         CLC   RNUM,=C'99'                                                      
         BNE   ENQD0                                                            
         MVI   DLCANC,C'C'                                                      
         MVC   DUB(1),REQNUMB                                                   
         BRAS  RE,GETREQID                                                      
         MVC   DLNUM,DUB+1                                                      
*                                                                               
ENQD0    CLI   REQNUM,255          DON'T DISPLAY REQ ID FOR SPECIFICS           
         BE    *+10                                                             
         MVC   DLNUM,=C'  '                                                     
         MVC   DLREQ,RAGY                                                       
         CLC   RCARD2,SPACES                                                    
         BE    *+10                                                             
         MVC   DLREQ2,RCARD2                                                    
*                                                                               
         DS    0H                                                               
***************************************************                             
* 2000 CHECKING - SCAN OUTPUT LINE FOR X'FA', X'FB', X'FC', X'FD'               
         LA    RE,150                                                           
         LA    RF,DLREQ                                                         
ENQD02   CLI   0(RF),X'FA'                                                      
         BNE   ENQD03                                                           
         MVI   0(RF),C'0'                                                       
         B     ENQD06                                                           
ENQD03   CLI   0(RF),X'FB'                                                      
         BNE   ENQD04                                                           
         MVI   0(RF),C'1'                                                       
         B     ENQD06                                                           
ENQD04   CLI   0(RF),X'FC'                                                      
         BNE   ENQD06                                                           
         MVI   0(RF),C'2'                                                       
         B     ENQD06                                                           
ENQD05   CLI   0(RF),X'FD'                                                      
         BNE   ENQD06                                                           
         MVI   0(RF),C'3'                                                       
         B     ENQD06                                                           
ENQD06   LA    RF,1(RF)                                                         
         BCT   RE,ENQD02                                                        
*******************************************************                         
ENQD09   CLI   DLREQ+56,C'A'                                                    
         BNL   *+8                                                              
         MVI   DLREQ+56,C' '       MIGHT BE BINARY PROGRAM TYPE                 
**       CLI   RHDR+10,4           REMOVED PXZ FOR NEW 04 REQ                   
**       BNE   *+8                                                              
**       OI    DLREQ+63,X'20'                                                   
         CLC   DLREQ+21(2),=C'NO'  SEE IF EST=NO                                
         BE    *+14                                                             
         CLC   DLREQ+21(3),=C'ALL' SEE IF EST=ALL                               
         BNE   *+10                                                             
         OC    DLREQ+24(3),=C'   '     NEGATIVE FILTERS HAVE X'40'              
*                                      SET OFF                                  
         CLI   RHDR+10,131         DEMO MENU LISTING - LD                       
         BNE   *+10                                                             
         OC    DLREQ+31(4),=C'    '   MIGHT HAVE NEGATIVE FILTER                
*                                     X'40'  SET OFF                            
*                                                                               
ENQDC    FOUT  DLCANCH                                                          
         FOUT  DLHDR                                                            
         OC    RCARD2,RCARD2                                                    
         BZ    ENQDCA                                                           
         OC    DLINE2,SPACES                                                    
         FOUT  DLHDR2                                                           
ENQDCA   CLI   STATUS,3                                                         
         BNE   ENQDISPX                                                         
*                                                                               
         LA    R4,DLNEXT           BUMP TO NEXT LINE                            
         OC    DLINE,SPACES                                                     
         CLC   DLINE,SPACES  DO WE ALREADY HAVE SOMETHING ON NEXT LINE          
         BE    ENQD1                                                            
         OI    DLCANCH+1,X'20'        YES - PROTECT CANCEL FIELD ON IT          
         CLI   DDS,1                           AND IF IT'S NOT DDS              
         BE    *+8                                                              
         OI    DLHDR+1,X'20'                   PROTECT 2ND LINE AS WELL         
         LA    R4,DLNEXT                    AND BUMP TO NEXT LINE               
         LH    R6,DISPMAX                   DECREASE MAX ALLOWED                
         BCTR  R6,0                                                             
         STH   R6,DISPMAX                                                       
         SPACE 2                                                                
ENQD1    ST    R4,DISPADR                                                       
         LH    R6,DISPCTR                    UPDATE DISPLAY COUNTER             
         LA    R6,1(R6)                                                         
         STH   R6,DISPCTR                                                       
         STH   R6,DISPFLDS                                                      
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   0(4,R6),ADR                   SAVE DISK ADR                      
         B     ENQDISPX                                                         
*                                                                               
*                                                                               
ENQDISPX XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
VALGMCGR NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP,TEMP                                                        
         MVC   TEMP+100(L'KEY+L'KEYS),KEY  SAVE KEY & KEYS JUST IN CASE         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D04'                                                  
         MVC   KEY+2(4),DEMS       AGY/MED + GROUP ID                           
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR',KEY,KEY,0                   
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(6),KEYD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
*                                                                               
         LA    R7,SPTREC                                                        
         LA    R7,24(R7)                                                        
         LA    R3,TEMP+50                                                       
         LHI   R0,10               CHECK FIRST 10 CLIENTS                       
         USING GRPVALD,R7                                                       
VALGMC20 CLI   0(R7),0                                                          
         BE    VALGMC50            DONE WITH CLIENTS                            
         CLI   0(R7),X'30'         CLIENTS                                      
         BE    VALGMC30                                                         
VALGMC25 ZIC   R1,1(R7)            NXT ELEM                                     
         AR    R7,R1                                                            
         B     VALGMC20                                                         
VALGMC30 GOTO1 CLPACK,DMCB,GRPVALUE,TEMP+90                                     
         MVC   0(2,R3),TEMP+90          SAVE CLIENT IN TEMP                     
         LA    R3,2(R3)                 UP TO 10 CLTS IN TEMP                   
         BCT   R0,VALGMC25                                                      
VALGMC50 MVI   0(R3),X'FF'              END OF CLIENT LIST                      
         LA    R3,TEMP+50               START OF CLIENT LIST                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),DEMS       AGY/MED                                      
VALGMC60 MVC   KEY+2(2),0(R3)      CLIENT                                       
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
*                                                                               
         LA    R7,SPTREC                                                        
         USING CLTHDR,R7                                                        
         CLI   R2USER+32,C' '      LMG/REGIONAL REQUEST?                        
         BH    *+16                                                             
         TM    COPT4,COP4BPCT                                                   
         BNZ   VALGMCER                                                         
         B     *+12                                                             
*                                                                               
         TM    COPT4,COP4BPCT                                                   
         BZ    VALGMCER                                                         
*                                                                               
         LA    R3,2(R3)            NXT CLT IN TEMP                              
         CLI   0(R3),X'FF'                                                      
         BE    *+8                 DONE: NO ERRORS                              
         B     VALGMC60                                                         
*                                                                               
         SR    R1,R1                                                            
VALGMCER LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* READ DARE ORDER REC AND IF IT EXISTS, GIVE AN ERROR, IF NOT READ              
* DESTINE RECORD TO VALIDATE FAX #                                              
*&&DO                                                                           
DXCHKS   NTR1  BASE=*,LABEL=*                                                   
         CLI   RMED,C'T'         DO THOSE CHECKS ONLY FOR MEDIA T               
         BNE   DXCHKOK                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DCKTYPE,X'0D'                                                    
         MVI   DCKSUBTY,X'B5'                                                   
         MVC   DCKAGMD,BAGYMD                                                   
         MVC   DCKCLT,BCLT                                                      
         DROP  R6                                                               
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         XC    DMCB,DMCB           MAKE SURE IT'S CLEAN                         
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(DCKPRD-DCKTYPE),KEYD   IF MATCH INCLUDING CLIENT             
         BNE   *+12                                                             
         MVC   FERN,=AL2(DARERR)     HAVE NO BUSINESS REQUESTING DX             
         B     DXCHKNO                                                          
*                                                                               
         OC    RSTA,RSTA           ANYTHING IN STATION FIELD ?                  
         BZ    DXCHKOK             IF NOT, CAN'T VALIDATE FURTHER               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DESTNRCD,R6                                                      
         MVI   DSRKTYPE,X'0D'                                                   
         MVI   DSRKSBTY,X'3D'                                                   
         MVC   DSRKAGMD,BAGYMD                                                  
         MVC   DSRKCLT,BCLT                                                     
         MVC   FULL,=C'0000'                                                    
         OC    RMARK,RMARK         ANYTHING IN MARKET FIELD ?                   
         BZ    *+10                                                             
         MVC   FULL,RMARK                                                       
         MVC   DUB(4),RSTA                                                      
         MVI   DUB+4,C'T'                                                       
         GOTO1 STAPACK,DMCB,(C'P',FULL),DUB,DUB+5                               
         MVC   DSRKSTA,DUB+5                                                    
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYD                                                     
         BE    DXCHKOK               REC FOUND, GOOD                            
         MVC   FERN,=AL2(DSTNMIS)    NO REC -> NO FAX NUMBER, ERROR             
         B     DXCHKNO                                                          
         DROP  R6                                                               
DXCHKOK  SR    R1,R1                                                            
DXCHKNO  LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
*  COMPARE 2 STATIONS UNIQUE IDS AND EXIT WITH CC                               
*                                                                               
*&&DO                                                                           
CHKUIDS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           READ AGYHEADER TO CHECK ID FLAG              
         MVC   KEY+1(2),RAGY                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
*                                                                               
         LA    R7,SPTREC                                                        
         USING AGYHDRD,R7                                                       
         TM    AGYFLAG2,AGYFLAG2_UID                                            
         BO    *+10                IF FLAG IS OFF NO FURTHER CHECK              
         CR    RB,RB               EXIT WITH CC EQ                              
         B     UIDX                                                             
         DROP  R7                                                               
*                                                                               
         XC    TEMP+30(12),TEMP+30                                              
*                                                                               
         LA    R7,KEYS                                                          
         USING STAREC,R7                                                        
         MVI   KEYS,C'0'            READ STATION REC                            
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,RMED                                                     
         MVC   STAKCALL,RSTA       OLD STATION                                  
         MVC   STAKAGY,RAGY                                                     
         GOTO1 ARSTA                                                            
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
*                                                                               
         LA    R7,SPTREC                                                        
         MVC   TEMP+30(6),STUNIQID    SAVE UNIQUE ID                            
*                                                                               
         LA    R7,KEYS                                                          
         MVC   STAKCALL,RBOOK      NEW STATION                                  
         GOTO1 ARSTA                                                            
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
*                                                                               
         LA    R7,SPTREC                                                        
         MVC   TEMP+36(6),STUNIQID                                              
         DROP  R7                                                               
*                                                                               
         CLC   TEMP+30(6),TEMP+36      DO THEY MATCH?                           
UIDX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*        EACH REQUEST THAT REQUIRES INDIVIDUAL POST VALIDATION HAS THE          
*        ADDRESS OF THE ROUTINE IN THIS TABLE.                                  
*                                                                               
VALROUTS DS    0CL5                                                             
         DC    AL1(02,0),AL3(VALR02)                                            
         DC    AL1(03,0),AL3(VALR03)       PH PHILIP MORRIS                     
         DC    AL1(05,0),AL3(VALR05)                                            
         DC    AL1(06,0),AL3(VALR06)                                            
         DC    AL1(07,0),AL3(VALR07)                                            
         DC    AL1(08,0),AL3(VALR08)                                            
         DC    AL1(12,0),AL3(VALR12)        SPOT UNBILLING                      
         DC    AL1(13,0),AL3(VALR12)        NET UNBILING (SAME AS SPOT)         
         DC    AL1(14,0),AL3(VALR14)        GT-EXTRACT                          
         DC    AL1(15,0),AL3(VALR15)        SUPERDESK STATUS                    
         DC    AL1(17,0),AL3(VALR17)        DEMO LOOKUP FIX                     
         DC    AL1(18,0),AL3(VALR18)                                            
         DC    AL1(19,0),AL3(VALR19)        LABATT INTERFACE                    
         DC    AL1(20,0),AL3(VALR20)        SPRINT INTERFACE                    
         DC    AL1(21,0),AL3(VALR21)        SAME AS 20                          
         DC    AL1(22,0),AL3(VALR23)        SAME AS 23                          
         DC    AL1(23,0),AL3(VALR23)                                            
         DC    AL1(24,0),AL3(VALR24)        CONTINENTAL INTERFACE               
         DC    AL1(25,0),AL3(VALR02)        EX-SAME AS EB                       
         DC    AL1(27,0),AL3(VALR27)        L'OREAL INTERFACE                   
         DC    AL1(29,0),AL3(VALR15)        JW- SAME AS VALR15                  
         DC    AL1(41,0),AL3(VALR41)                                            
         DC    AL1(42,0),AL3(VALR42)         CH - CHOICE HOTELS                 
         DC    AL1(43,0),AL3(VALR42)         PZ - SAME AS CH                    
         DC    AL1(44,0),AL3(VALR44)                                            
         DC    AL1(47,0),AL3(VALR47)                                            
         DC    AL1(48,0),AL3(VALR48)                                            
         DC    AL1(49,0),AL3(VALR49)                                            
         DC    AL1(50,0),AL3(VALR50)                                            
         DC    AL1(53,0),AL3(VALR53)                                            
         DC    AL1(74,0),AL3(VALR74)                                            
         DC    AL1(90,0),AL3(VALR90)                                            
         DC    AL1(91,0),AL3(VALR91)                                            
*        DC    AL1(92,0),AL3(VALR92)                                            
         DC    AL1(98,0),AL3(VALR98)                                            
         DC    AL1(101,0),AL3(VALR101)                                          
         DC    AL1(102,0),AL3(VALR102)                                          
         DC    AL1(11,0),AL3(VALR102)       CM REPORT LIKE SX REPT              
         DC    AL1(103,0),AL3(VALR103)                                          
         DC    AL1(104,0),AL3(VALR101)      SAME AS 101                         
         DC    AL1(106,0),AL3(VALR106)                                          
         DC    AL1(107,0),AL3(VALR107)                                          
         DC    AL1(109,0),AL3(VALR98)                  SAME AS 98               
         DC    AL1(111,0),AL3(VALR61)         SAME AS 61                        
         DC    AL1(112,0),AL3(VALR1120)                                         
         DC    AL1(113,0),AL3(VALR112)                                          
         DC    AL1(114,0),AL3(VALR114)                                          
         DC    AL1(115,0),AL3(VALR112A)                                         
         DC    AL1(118,0),AL3(VALR118)                                          
         DC    AL1(119,0),AL3(VALR119)                                          
         DC    AL1(121,0),AL3(VALR59)        SAME AS 59                         
         DC    AL1(125,0),AL3(VALR59)        SAME AS 59                         
         DC    AL1(127,0),AL3(VALR59)        SAME AS 59                         
         DC    AL1(128,0),AL3(VALR160)                                          
         DC    AL1(132,0),AL3(VALR132)                                          
         DC    AL1(137,0),AL3(VALR137)                                          
         DC    AL1(138,0),AL3(VALR211)        SAME AS 211                       
         DC    AL1(140,0),AL3(VALR140)                                          
         DC    AL1(142,0),AL3(VALR140)       SAME AS 140                        
****     DC    AL1(150,0),AL3(VALR150)                                          
         DC    AL1(152,0),AL3(VALR153B)                                         
         DC    AL1(153,0),AL3(VALR153)                                          
         DC    AL1(156,0),AL3(VALR156)                                          
*        DC    AL1(159,0),AL3(VALR159)                                          
         DC    AL1(160,0),AL3(VALR160)                                          
         DC    AL1(161,0),AL3(VALR160)                                          
         DC    AL1(162,0),AL3(VALR160)                                          
         DC    AL1(163,0),AL3(VALR163)                                          
         DC    AL1(164,0),AL3(VALR163)        SAME AS 163                       
         DC    AL1(166,0),AL3(VALR166)                                          
         DC    AL1(167,0),AL3(VALR166)                                          
         DC    AL1(168,0),AL3(VALR168)                                          
         DC    AL1(169,0),AL3(VALR169)                                          
         DC    AL1(170,0),AL3(VALR170)                                          
         DC    AL1(171,0),AL3(VALR171)                                          
         DC    AL1(172,0),AL3(VALR172)                                          
         DC    AL1(173,0),AL3(VALR170)       SAME AS 170 (J6)                   
         DC    AL1(174,0),AL3(VALR150)       SAME AS 150                        
         DC    AL1(180,0),AL3(VALR112B)                                         
         DC    AL1(192,0),AL3(VALR192)                                          
         DC    AL1(193,0),AL3(VALR192)           SAME AS 192                    
         DC    AL1(194,0),AL3(VALR194)                                          
         DC    AL1(195,0),AL3(VALR195)                                          
         DC    AL1(196,0),AL3(VALR196)                                          
         DC    AL1(197,0),AL3(VALR197)                                          
         DC    AL1(199,0),AL3(VALR199)                                          
         DC    AL1(202,0),AL3(VALR202)                                          
         DC    AL1(205,0),AL3(VALR205)            IN - NISSAN FILE              
         DC    AL1(206,0),AL3(VALR228)                                          
         DC    AL1(207,0),AL3(VALR196)            SAME AS 196                   
         DC    AL1(210,0),AL3(VALR210)                                          
         DC    AL1(211,0),AL3(VALR211)                                          
         DC    AL1(212,0),AL3(VALR212)                                          
         DC    AL1(214,0),AL3(VALR214)                                          
         DC    AL1(215,0),AL3(VALR215)                                          
         DC    AL1(217,0),AL3(VALR217)                                          
         DC    AL1(218,0),AL3(VALR217)                                          
         DC    AL1(219,0),AL3(V235E)                                            
         DC    AL1(220,0),AL3(VALR220)                                          
         DC    AL1(221,0),AL3(VALR221)                                          
         DC    AL1(222,0),AL3(VALR196)            SAME AS 196                   
         DC    AL1(223,0),AL3(VALR196)            SAME AS 196                   
         DC    AL1(226,0),AL3(VALR192)            SAME AS 192                   
         DC    AL1(227,0),AL3(VALR227)            SAME AS 192                   
         DC    AL1(228,0),AL3(VALR228)                                          
         DC    AL1(229,0),AL3(VALR163)            SAME AS 163                   
         DC    AL1(230,0),AL3(VALR23)             SAME AS 23                    
         DC    AL1(231,0),AL3(VALR231)                                          
         DC    AL1(232,0),AL3(VALR232)                                          
         DC    AL1(233,0),AL3(VALR233)                                          
         DC    AL1(234,0),AL3(VALR234)                                          
         DC    AL1(235,0),AL3(VALR235)                                          
         DC    AL1(236,0),AL3(VALR236)                                          
         DC    AL1(238,0),AL3(VALR23)             SAME AS 23                    
*****    DC    AL1(240,0),AL3(VALR240)                                          
         DC    AL1(241,0),AL3(VALR241)                                          
         DC    AL1(242,0),AL3(VALR242)                                          
         DC    AL1(243,0),AL3(VALR23)                                           
         DC    AL1(244,0),AL3(VALR244)                                          
         DC    AL1(245,0),AL3(VALR245)                                          
         DC    AL1(246,0),AL3(VALR246)                                          
         DC    AL1(247,0),AL3(VALR160)            W4 SAME AS D4                 
         DC    AL1(251,0),AL3(VALR211)            KL SAME AS K4/K5              
         DC    AL1(252,0),AL3(VALR252)                                          
         DC    AL1(254,0),AL3(VALR252)            KB SAME AS KA                 
VALROUTX DC    AL1(00,0)                                                        
         EJECT                                                                  
***********************************************************                     
* SET UP ECOST REQUEST                                                          
* ONLY FOR MEDIAVEST/OR SJR  BU REQUEST                                         
*                                                                               
DOREQEC  NTR1  BASE=*,LABEL=*                                                   
         MVC   REQRECSV,SPACES                                                  
         MVC   REQRECSV(2),=C'O8'      ECOST                                    
         MVC   REQRECSV+2(2),RAGY                                               
         MVC   REQRECSV+4(22),=CL22'*.DDS.ECOST.ON,DDS....'                     
         MVC   REQRECSV+26(3),RCLI                                              
         MVI   REQRECSV+29,C'.'                                                 
         MVC   REQRECSV+30(3),=C'POL'                                           
         MVI   REQRECSV+33,C'.'                                                 
         MVC   REQRECSV+34(3),REST                                              
         MVI   REQRECSV+37,C'.'                                                 
         MVI   REQRECSV+38,C'.'      NETWORK                                    
         MVI   REQRECSV+39,C'.'      PACKAGE                                    
         MVI   REQRECSV+40,C'.'      DAYPART                                    
         GOTO1 DATCON,DMCB,(0,RSTRD),(10,REQRECSV+41)                           
         MVI   REQRECSV+49,C'.'                                                 
         GOTO1 DATCON,DMCB,(0,RENDD),(10,REQRECSV+50)                           
         TM    SEDSAVE,X'40'          YYMM AND NO END DATE?                     
         BO    DQ10                                    YES                      
         GOTO1 DATCON,DMCB,(0,RSTRD),(10,REQRECSV+41)  NO                       
         GOTO1 DATCON,DMCB,(0,RENDD),(10,REQRECSV+50)                           
         MVC   TEMP(6),RSTRD           SAVE START                               
         B     DQ30                    SKIP PROFILES IF YYMMDD-YYMMDD           
*  BU ENTERS YYMM AND NO END DATE                                               
* SET START TO FIRST OF MONTH AND END TO LAST DAY OF MONTH                      
DQ10     MVC   TEMP(6),RSTRD                                                    
         MVI   TEMP+5,C'1'                          SET START TO DAY 1          
         GOTO1 DATCON,DMCB,(0,TEMP),(10,REQRECSV+41)                            
         GOTO1 ADDAY,DMCB,(C'Y',TEMP),(X'80',TEMP+6),0   END OF MONTH           
         GOTO1 DATCON,DMCB,(0,TEMP+6),(10,REQRECSV+50)                          
*                                                                               
* NOW DEAL WITH PRIOR MONTHS                                                    
* TEMP HAS YYMMDD OF START DATE                                                 
DQ15     CLC   R2USER+22(2),=X'4040'   OVERRIDE OF PRIOR MONTHS?                
         BE    DQ20                    NO                                       
         PACK  DUB,R2USER+22(2)        YES                                      
         CVB   R2,DUB                                                           
         LNR   R2,R2                                                            
         B     DQ25                                                             
* NO OVERRIDE OF PRIOR MONTHS, CLCOPT2 HAS PRIOR BILLING OPTION                 
DQ20     CLI   CLCOPT2,C'N'            PRIOR OPTION ?                           
         BE    DQ30                    NO                                       
         CLI   CLCOPT2,X'40'           PRIOR OPTION ?                           
         BNH   DQ30                    NO                                       
         LA    R1,PRIORTBL             CHECK TABLE AGAINST INPUT                
         LA    RE,11                                                            
DQ21     CLC   CLCOPT2,0(R1)                                                    
         BE    DQ23                                                             
         LA    R1,2(R1)                                                         
         BCT   RE,DQ21                                                          
         DC    H'0'                                                             
DQ23     ZIC   R2,1(R1)                                                         
         LNR   R2,R2                                                            
DQ25     ST    R2,DMCB+8             SET MONTHS TO GO BACK                      
         GOTO1 ADDAY,DMCB,(C'M',TEMP),TEMP                                      
         GOTO1 DATCON,DMCB,(0,TEMP),(10,REQRECSV+41)                            
*                                                                               
DQ30     EQU   *                                                                
* BACK UP START 7 DAYS IN CASE OF BRADCAST CALENDAR                             
         GOTO1 DATCON,DMCB,(4,REQRECSV+41),(0,TEMP)                             
         GOTO1 ADDAY,DMCB,(C'D',TEMP),TEMP,F'-7'                                
         GOTO1 DATCON,DMCB,(0,TEMP),(10,REQRECSV+41)                            
*                                                                               
         MVI   REQRECSV+58,C'.'                                                 
         MVI   REQRECSV+59,C'G'        LIVE RUN                                 
         MVC   REQRECSV+60(2),=C'.*'                                            
         TM    ODDMNTS,ECINT           EQUIVALENCE ON TIME+INT?                 
         BNO   DQ40                                                             
         MVI   REQRECSV+60,C'.'                                                 
         MVI   REQRECSV+61,C'Y'        YES                                      
         MVC   REQRECSV+62(2),=C'.*'                                            
         B     DQ50                                                             
DQ40     TM    ODDMNTS,ECINTSP         TIME + INT + SPEC                        
         BNO   DQ50                                                             
         MVI   REQRECSV+60,C'.'                                                 
         MVI   REQRECSV+61,C'S'        YES                                      
         MVC   REQRECSV+62(2),=C'.*'                                            
DQ50     XIT1                                                                   
PRIORTBL DS    0H                                                               
         DC    CL1'S',XL1'C'                                                    
         DC    CL1'T',XL1'C'                                                    
         DC    CL1'M',XL1'C'                                                    
         DC    CL1'A',XL1'1'                                                    
         DC    CL1'B',XL1'2'                                                    
         DC    CL1'C',XL1'3'                                                    
         DC    CL1'1',XL1'1'                                                    
         DC    CL1'2',XL1'2'                                                    
         DC    CL1'3',XL1'3'                                                    
         DC    CL1'U',XL1'19'                                                   
         DC    CL1'V',XL1'19'                                                   
*                                                                               
         EJECT                                                                  
**************************                                                      
         PRINT NOGEN                                                            
VESTRTN  DS    0H                                                               
         NMOD1 0,VEST                                                           
         L     R9,0(R1)                      R9=A(W/S)                          
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LR    R5,R3                                                            
         MVI   ROUTNUM,05                                                       
         TM    ESTSAVE,X'02'                                                    
         BZ    VALESTX                                                          
         TM    REQFMT,REQDDS                                                    
         BO    VALESTX                                                          
         LA    R6,EALLTAB                                                       
*                                                                               
VEST2    CLI   0(R6),0             END OF TABLE                                 
         BE    VESTERR             EST=ALL NOT ALLOWED                          
         CLC   RAGY(6),0(R6)       AGY/MED/CLT                                  
         BE    VALESTX                                                          
         CLC   RAGY(3),0(R6)       AGY/MED                                      
         BNE   VEST4                                                            
         CLC   3(3,R6),=C'ALL'     ALL CLIENTS                                  
         BE    VALESTX                                                          
*                                                                               
VEST4    LA    R6,6(R6)            NEXT ENTRY                                   
         B     VEST2                                                            
*                                                                               
VESTERR  DS    0H                                                               
         MVC   FERN,=AL2(ESTALLX)               ERROR INV ESTIMATE              
         MVC   FULL(3),=C'ERR'                                                  
VALESTX  XIT1                                                                   
         SPACE 2                                                                
EALLTAB  DS    0C            TABLE OF AGY/MED/CLT ALLOWED EST=ALL               
         DC    C'JWTTM '                                                        
         DC    C'JWTTN '                                                        
         DC    C'JWTTO '                                                        
         DC    C'JWTTP '                                                        
         DC    C'JWTTQ '                                                        
         DC    C'JWTTR '                                                        
         DC    C'JWTTMA'                                                        
         DC    C'JWTTNA'                                                        
         DC    C'JWTTOA'                                                        
         DC    C'JWTTPA'                                                        
         DC    C'JWTTQA'                                                        
         DC    C'JWTTRA'                                                        
         DC    C'JWTTSA'                                                        
         DC    C'JWRTM '                                                        
         DC    C'JWRTN '                                                        
         DC    C'JWRTO '                                                        
         DC    C'JWRTP '                                                        
         DC    C'JWRTQ '                                                        
         DC    C'JWRTR '                                                        
         DC    C'JWRTMA'                                                        
         DC    C'JWRTNA'                                                        
         DC    C'JWRTOA'                                                        
         DC    C'JWRTPA'                                                        
         DC    C'JWRTQA'                                                        
         DC    C'JWRTRA'                                                        
         DC    C'JWRTSA'                                                        
         DC    XL6'00'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************                                                   
CHKRNUM  DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,CHKRNU                                                         
         L     R9,0(R1)                      R9=A(W/S)                          
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LR    R5,R3                                                            
*                                                                               
         CLC   BVRNUM(2),=C'E2'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'E5'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'E8'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'Q2'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'Q3'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'Q4'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'Q6'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'Q7'                                                 
         BE    CHKYES                                                           
         CLC   BVRNUM(2),=C'QP'                                                 
         BNE   CHKX                                                             
*                                                                               
CHKYES   MVC   LREQOHDR+26(2),BVRNUM                                            
         MVC   RNUM(2),BVRNUM                                                   
*                                                                               
CHKX     XIT1                                                                   
         EJECT                                                                  
VVALR23  DS    0H                                                               
         NMOD1 0,VL23                                                           
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     RC,4(R1)                                                         
         USING WORKD,RC                                                         
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
*                                                                               
         CLC   RNUM,=C'I2'         ..IF I2                                      
         BE    *+24                                                             
         CLC   RNUM,=C'N2'         ..OR N2                                      
         BE    *+14                                                             
         CLC   RNUM,=C'IL'         ..OR IL                                      
         BNE   CKI4I5                                                           
*                                                                               
         CLI   RO5,C'N'            IF POST AFFINDS NOT EQUAL N                  
         BE    CKI4I5                                                           
         BRAS  RE,CKUPDT           CHECK IF UPDATE IS ALLOWED                   
         BNE   CKI4I5                                                           
         MVI   ROUTNUM,X'86'       POST AFFIDS FIELD                            
         MVC   FERN,=AL2(1250)                                                  
         B     VR23X                                                            
CKI4I5   CLC   RNUM,=C'IL'         ..IF IL, DONE                                
         BE    VR23X                                                            
         CLC   RNUM,=C'I4'         ..IF I4                                      
         BE    *+14                                                             
         CLC   RNUM,=C'I5'         ..OR I5                                      
         BNE   VR230                                                            
*                                                                               
         CLI   RO1,C'Y'            IF NOT ADDING BUYS                           
         BNE   I4PROF                                                           
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   CKI4I5UP                                                         
         MVI   ROUTNUM,X'8C'                                                    
         MVC   FERN,=AL2(0007)     SOON NOT ALLOWED FOR UPDATES                 
         B     VR23X                                                            
CKI4I5UP DS    0H                                                               
         BRAS  RE,CKUPDT           CHECK IF UPDATE IS ALLOWED                   
         BNE   I4PROF                                                           
         MVI   ROUTNUM,X'8C'                                                    
         MVC   FERN,=AL2(1250)                                                  
         B     VR23X                                                            
I4PROF   CLC   RNUM,=C'I5'          IF I5, DONE                                 
         BE    VR23X                                                            
         XC    TEMP(20),TEMP       NO / READ NV PROFILE                         
         MVC   TEMP+30(2),=C'SO'                                                
         MVC   TEMP+32(2),RNUM                                                  
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
V23PRO   GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
******   MVC   HALF,RNUM          ..READ I4 PROFILE                             
******   BAS   RE,RDPROF                                                        
         CLI   TEMP+2,C'Y'        ..SEE IF I4 ALLOWED                           
         BE    I4OK                                                             
         MVC   FERN,=AL2(NUMINV)    ..NO                                        
         B     VR23X                                                            
*                                                                               
I4OK     TM    ESTSAVE,X'04'       ..AND IF ONE ESTIMATE                        
         BNO   VR230                                                            
         XC    TEMP(20),TEMP       NO / READ NV PROFILE                         
         MVC   TEMP+30(2),=C'SO'                                                
         MVC   TEMP+32(2),RNUM     READ I4/I5 PROFILE                           
         MVC   TEMP+34(6),RAGY                                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
         L     R5,DATAMGR                                                       
V23PROF  GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
******   MVC   HALF,RNUM           ..READ I4/I5 PROFILE                         
******   BAS   RE,RDPROF                                                        
         CLI   TEMP,C'Y'                                                        
         BE    VR230                                                            
         MVI   ROUTNUM,X'05'                                                    
         B     VR23ERR                                                          
*                                                                               
VR230    CLC   RNUM,=C'N2'         IF N2 (NET FUDGE OF I2)                      
         BE    VR230AA                                                          
         CLC   RNUM,=C'L2'         OR L2                                        
         BE    VR230AA                                                          
         CLC   RNUM,=C'I2'         OR I2                                        
         BNE   VR23A                                                            
VR230AA  DS    0H                                                               
* - RRS=Y IN OPTION FIELD NOW GENERATES OTHER REPORTS                           
*        MVC   TEMP+30(4),=C'SI2Y'        I2Y PROFILE READ                      
*        NI    TEMP+30,X'BF'              LOWER CASE                            
*        MVC   TEMP+34(6),RAGY                                                  
*        CLI   CLIOFFC,C' '                                                     
*        BNH   *+14                                                             
*        MVI   TEMP+40,C'*'                                                     
*        MVC   TEMP+41(1),CLIOFFC                                               
*        L     R5,DATAMGR                                                       
*        BAS   R4,GTPROF                                                        
*        CLI   TEMP+7,C'A'                                                      
*        BL    *+10                                                             
*        MVC   R2USER(1),TEMP+7             TO GENERATE OTHER REPORTS           
*                                                                               
         CLC   RNUM,=C'N2'         IF N2(NETWORK I2)                            
         BNE   SKIPGZ                                                           
         CLC   RAGY,=C'DU'         FOR DU DISALLOW PRD=ALL                      
         BE    *+14                                                             
         CLC   RAGY,=C'GZ'         AND AGY=GZ                                   
         BNE   SKIPGZ                                                           
*                                                                               
         CLC   =C'NO',REST         ONLY IF ESTIMATE=NO                          
         BNE   SKIPGZ                                                           
         CLC   =C'ALL',RPRO         AND PRD=ALL                                 
         BNE   SKIPGZ                                                           
*                                                                               
         MVI   ROUTNUM,X'04'                                                    
         MVC   FERN,=AL2(828)      SPECIFIC PRD/PRD GROUP REQD                  
         B     VR23X                                                            
SKIPGZ   EQU   *                                                                
*                                                                               
         CLC   RNUM,=C'I2'         FOR SPOT I2 ONLY                             
         BNE   VR230BB                                                          
         CLC   BVROUT(4),=C'SOON'  IF NON UPDATIVE SOON                         
         BNE   *+12                                                             
         CLI   RO5,C'N'            I.E. IF POST AFFIDS=N                        
         BE    VR230BB             SKIP THE CHECK                               
         MVI   ROUTNUM,X'04'       PROD FIELD                                   
         TM    PROSAVE,X'08'       IS IT POL ?                                  
         BNO   VR230BB                                                          
         CLI   CLIPROF+0,C'0'      CLIPROF+0 MUST = C'0'                        
         BE    *+14                                                             
         MVC   FERN,=AL2(1132)     MUST REQUEST SPECIFIC PRODUCT                
         B     VR23X                                                            
*                                                                               
*&&DO                                                                           
VR230BB  CLC   REST,=C'NO '        IF EST=NO                                    
         BNE   VR230BK                                                          
         CLI   RO5,C'N'                                                         
         BE    VR230BK             AND UPDATIVE                                 
         MVC   TEMP+30(4),=C'SI2N'     I2N PROFILE READ                         
*****    BRAS  RE,GTPROAGY                                                      
         BRAS  RE,GTPROCLT                                                      
         CLI   TEMP+3,C'Y'         EST NO NOT ALLOWED                           
         BNE   VR230BK                                                          
         MVI   ROUTNUM,X'05'       EST FIELD                                    
         B     VR23ERR                                                          
*&&                                                                             
VR230BB  CLI   RO5,C'N'                                                         
         BE    VR230BK             AND UPDATIVE                                 
         MVC   TEMP+30(4),=C'SI2N' I2N PROFILE READ                             
*****    BRAS  RE,GTPROAGY                                                      
         BRAS  RE,GTPROCLT                                                      
         CLI   TEMP+3,C'Y'         EST NO NOT ALLOWED                           
         BNE   VR230BC                                                          
         CLI   TEMP+5,C'Y'         CHECK FOR PROFILE INCOMPAT                   
         BE    VR23I2NE                                                         
         CLI   TEMP+7,C'Y'                                                      
         BE    VR23I2NE                                                         
         CLC   REST,=C'NO '        IF EST=NO                                    
         BNE   *+12                                                             
         MVI   ROUTNUM,X'05'       EST FIELD                                    
         B     VR23I2NE            ERROR                                        
*                                                                               
VR230BC  TM    ODDMNTS,X'04'       IS IT NET?                                   
         BO    VR230BG             GO CHK NET PROFILE FIELDS                    
         CLI   TEMP+5,C'Y'         ONLY EST=NO  ALLOWED                         
         BNE   VR230BD                                                          
         CLC   REST,=C'NO '        IF EST=NO                                    
         BE    *+12                                                             
         MVI   ROUTNUM,X'05'       EST FIELD                                    
         B     VR23I2NE            ERROR                                        
*                                                                               
VR230BD  CLI   TEMP+4,C'N'         CHECK PRODUCT FIELD                          
         BE    VR230BK             DONE                                         
         MVI   ROUTNUM,X'04'       PRD FIELD                                    
         CLI   TEMP+4,C'A'         PRD=ALL OR SPECIFIC PRD ALLOWED              
         BNE   *+14                                                             
         CLC   RPRO,=C'POL'        POL NOT ALLOWED                              
         BE    VR23I2NE                                                         
*                                                                               
         CLI   TEMP+4,C'P'         ONLY POL ?                                   
         BNE   *+14                                                             
         CLC   RPRO,=C'POL'                                                     
         BNE   VR23I2NE                                                         
         B     VR230BK                                                          
*                                                                               
VR230BG  DS    0H                  NET I2 PROFILE FIELDS                        
         CLI   TEMP+7,C'Y'         ONLY EST=NO  ALLOWED                         
         BNE   VR230BH                                                          
         CLC   REST,=C'NO '        IF EST=NO                                    
         BE    *+12                                                             
         MVI   ROUTNUM,X'05'       EST FIELD                                    
         B     VR23I2NE            ERROR                                        
*                                                                               
VR230BH  CLI   TEMP+6,C'N'         CHECK PRODUCT FIELD                          
         BE    VR230BK             DONE                                         
         MVI   ROUTNUM,X'04'       PRD FIELD                                    
         CLI   TEMP+6,C'A'         PRD=ALL OR SPECIFIC PRD ALLOWED              
         BNE   *+14                                                             
         CLC   RPRO,=C'POL'        POL NOT ALLOWED                              
         BE    VR23I2NE                                                         
*                                                                               
         CLI   TEMP+6,C'P'         POL ?                                        
         BNE   *+14                                                             
         CLC   RPRO,=C'POL'                                                     
         BNE   VR23I2NE                                                         
*                                                                               
*                                                                               
VR230BK  DS    0H                                                               
         XC    TEMP(60),TEMP                                                    
         LA    R5,TEMP+30                                                       
         USING PROFKD,R5                  READ I2Z TO F/O IF NEED TO            
         MVC   PROFKSYS(4),=C'SI2Z'       RE-READ BY SUB-MEDIA                  
         NI    PROFKSYS,X'BF'                                                   
         MVC   PROFKAGN,RAGY                                                    
         MVC   PROFKMED,RMED                                                    
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,CLIOFFC                                                 
         DROP  R5                                                               
         L     R5,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
*******  BAS   R4,GTPROF                                                        
         LA    R5,TEMP+30                                                       
         USING PROFKD,R5                                                        
         MVC   PROFKSYS(4),=C'SI2A'       NOW READ I2A                          
         NI    PROFKSYS,X'BF'                                                   
         CLC   RNUM,=C'N2'                IF NETWORK, CHECK IF NEED TO          
         BNE   VR230BN                    READ BY SUB-MEDIA                     
         CLI   TEMP+15,C'Y'        TEMP HAS I2Z PROFILE                         
         BNE   VR230BN                                                          
         CLI   RNUM+56,C' '                                                     
         BNH   VR230BN             IF SUB-MEDIA FILTER IS PRESENT...            
         MVC   PROFKMED,RNUM+56    READ PROFILE FOR IT                          
         DROP  R5                                                               
VR230BN  L     R5,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,(0(R3),(R5))                       
*******  BAS   R4,GTPROF                                                        
         OC    TEMP+9(2),TEMP+9    CHECK IF ANYTHING THERE                      
         BZ    VR230BP             IF NOT, NO FURTHER CHECK REQUIRED            
         ZIC   R1,TEMP+9           CHECK YEAR                                   
         CHI   R1,80               IF YEAR < 80 ADD 100 FOR CENTURY             
         BH    *+8                                                              
         AHI   R1,100                                                           
         STC   R1,TEMP+9                                                        
         MVC   FULL(2),TEMP+9      PUT IN YEAR AND MONTH                        
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,PLIST,(3,FULL),DUB                                        
         CLC   RENDD(4),=C'    '   CHECK IF THERE'S AN END DATE                 
         BNH   *+18                IF NOT CHECK AGAINST START DATE              
         CLC   RENDD(4),DUB                                                     
         BH    VR230BP                                                          
         B     *+14                                                             
*                                                                               
         CLC   RSTRD(4),DUB        IF REQUESTING BEFORE LOCK DATE               
         BH    VR230BP             DISALLOW POST AFFIDS =Y&P                    
*                                                                               
         CLI   RO5,C'N'                                                         
         BNH   VR230BP                                                          
         MVC   FERN,=AL2(FE)          SET MY MESSAGE                            
         XC    BVRHDR,BVRHDR                                                    
         MVI   ROUTNUM,X'86'       POST AFFIDS FIELD                            
         MVC   BVRHDR(37),=C'POST AFFIDS LOCKED DUE TO I2A PROFILE'             
         B     VR23X                                                            
*                                                                               
VR230BP  MVI   ROUTNUM,X'86'       POST AFFIDS FIELD                            
         CLI   RO5,X'40'           CHECK IF REQUIRED                            
         BNH   *+18                                                             
         CLC   BVROUT(4),=C'SOON'    IF NOT SOON                                
         BNE   VR23A                 BRANCH OUT                                 
         B     VR230BS                                                          
*                                                                               
         MVC   TEMP+30(4),=C'SI2A'   IF OVNGHT READ I2A                         
         CLC   BVROUT(4),=C'SOON'    IF SOON - I2Z                              
         BNE   *+10                                                             
         MVC   TEMP+30(4),=C'SI2Z'        I2Z PROFILE READ                      
         BRAS  RE,GTPROAGY                                                      
         CLC   BVROUT(4),=C'SOON'                                               
         BE    *+16                                                             
         CLI   TEMP,C'Y'                                                        
         BE    VR23ERR                                                          
         B     VR23A                                                            
*                                                                               
         CLI   TEMP+13,C'Y'        IF PROFILE NOT Y -- ERROR                    
         BNE   VR23ERR             IF SOON IS REQUIRED                          
         MVI   RO5,C'Y'            IF 'Y', PUT IT IN                            
VR230BS  CLI   RO5,C'Y'            ...IF POST AFFIDS=Y                          
         BE    *+12                                                             
         CLI   RO5,C'P'            OR IF P                                      
         BNE   VR23A                                                            
*&&DO                                                                           
         MVI   ROUTNUM,X'04'       PROD FIELD                                   
         CLC   =C'ALL',RPRO        ...PRD=ALL NOT ALLOWED                       
         BNE   *+14                                                             
         MVC   FERN,=AL2(823)      SPECIFIC PRD/POL REQUIRED                    
         B     VR23X                                                            
*&&                                                                             
**************************************************************                  
         MVI   ROUTNUM,X'86'       POST AFFIDS FIELD                            
         CLC   RNUM,=C'I2'       UPDATIVE SOON FOR SPOT I2 ONLY                 
         BE    *+14                                                             
         CLC   RNUM,=C'N2'       UPDATIVE SOON FOR NET I2 ONLY                  
         BNE   VR23ERR                                                          
         CLC   =C'CK',RAGY         BUT NOT FOR COKE                             
         BE    VR23ERR                                                          
*                                                                               
*                                  I2/N2 FOR SPOT AND NET                       
         MVI   ROUTNUM,2                                                        
         TM    CLISAVE,X'04'       SINGLE CLIENT                                
         BNO   VR23ERR                                                          
         MVI   ROUTNUM,7                                                        
         TM    STASAVE,X'04'       SINGLE STATION?                              
*        BO    VR23A                                                            
         BO    *+8                                                              
         B     VR23ERR                                                          
         CLC   RPRO,=C'ALL'        IF PROD=ALL                                  
         BNE   VR23A                                                            
         CLI   CLIPROF+0,C'0'      AND IF TPOL                                  
         BNE   VR23A                                                            
         CLC   RNUM,=C'N2'       UPDATIVE SOON FOR NET I2 ONLY                  
         BE    VR23A             OK FOR NETPAK - NO TPOL THERE                  
         TM    ODDMNTS,X'01'       POL-EST?                                     
         BNO   VR23A                                                            
         MVI   ROUTNUM,4                                                        
         MVC   FERN,=AL2(FE)       SET MY ERROR MSG                             
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(36),=C'***TPOL NEEDS SPECIFIC PROD FOR SOON'              
         B     VR23X                                                            
**********************************************************                      
VR23ERR  MVC   FERN,=AL2(FLDINV)                                                
         B     VR23X                                                            
*                                                                               
VR23I2NE MVC   FERN,=AL2(I2NERR)                                                
         B     VR23X                                                            
*                                                                               
VR23A    BAS   RE,R23ACC                                                        
         CLC   FULL(3),=C'ERR'                                                  
         BE    VR23X                                                            
*                                                                               
         CLI   RENDD,C' '                                                       
         BE    VR23C                                                            
         MVC   HALF,=X'0C00'       12 MTHS                                      
         BAS   RE,V23MAX                                                        
         CLC   FULL(3),=C'ERR'     FERN AND ROUTNUM WILL BE SET                 
         BE    VR23X                                                            
*                                                                               
VR23C    CLC   RSTRD+4(2),=C'  '                                                
         BE    VR23X                                                            
*                                                                               
         CLC   RNUM,=C'N2'         NET I2 ?                                     
         BNE   *+12                                                             
         CLI   RBOOK1+1,C'N'       YES, CHECK IF SUB-MED N                      
         BE    VR23X               YES, DON'T DO MONDAY CHECK                   
*                                                                               
         LA    R6,1                MONDAY                                       
         LA    R5,RSTRD                                                         
         BAS   RE,V23DAY                                                        
         CLC   FULL(3),=C'ERR'     FERN AND ROUTNUM WILL BE SET                 
         BE    VR23X                                                            
         CLC   RENDD+4(2),=C'  '                                                
         BE    VR23X                                                            
         LA    R6,7                SUNDAY                                       
         LA    R5,RENDD                                                         
         BAS   RE,V23DAY           FERN WILL BE SET IF ERROR                    
VR23X    XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*                                                                               
V23DAY   LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         BRAS  RE,V23RFP           ..ARE WE DEALING WITH SYMBOLIC NAME          
         BE    V23DX               ..YES/SKIP DAY VALIDATION                    
         GOTO1 GETDAY,DMCB,(0,(R5)),TEMP                                        
         CLC   TEMP(3),=C'   '                                                  
         BE    V23DERR                                                          
         SR    R4,R4                                                            
         IC    R4,DMCB                                                          
         CR    R4,R6                                                            
         BNE   V23DERR         WRONG DAY                                        
V23DX    LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
V23DERR  MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,9                                                        
         MVC   FULL(3),=C'ERR'                                                  
         B     V23DX                                                            
         EJECT                                                                  
R23ACC   EQU   *                                                                
         MVI   ROUTNUM,X'5E'       CURSOR TO MKT/MKTGRP                         
         B     R23ACC1                                                          
*                                                                               
R23ACCM  MVI   ROUTNUM,X'06'           CURSOR TO MARKET                         
*                                                                               
R23ACC1  EQU   *                                                                
         CLI   6(R3),C'+'          CHECK FOR LIMIT ACCESS                       
         BNE   R23ACCX                                                          
         CLC   RMARK(3),=C'ALL'                                                 
         BE    R23ACC2                                                          
         CLC   RMARK,=C'    '                                                   
         BNE   R23ACCX                                                          
R23ACC2  EQU   *                                                                
         CLC   RSTA(3),=C'   '                                                  
         BE    R23ACCE                                                          
         CLC   RSTA(3),=C'ALL'                                                  
         BNE   R23ACCX                                                          
R23ACCE  MVC   FERN,=AL2(ACCERR)                                                
         MVC   FULL,=C'ERR'           SET ERROR RETURN                          
R23ACCX  BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
V23RFP   NTR1  BASE=*,LABEL=*                                                   
         TM    RFPSTAT,RFPINUSE    ..$RPF                                       
         BZ    V23PNO                                                           
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         ZIC   R0,RFPVNUMS                                                      
V23RFP5  CLC   0(3,R5),RFPVSYME                                                 
         BE    V23RFPX                                                          
         LA    R4,RFPVSYML(R4)                                                  
         BCT   R0,V23RFP5                                                       
V23PNO   LTR   RE,RE                                                            
*                                                                               
V23RFPX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
V23MAX   EQU   *                             START,END DATES DURATION           
         LR    R0,RE                                                            
         GOTO1 AVVALMAX,DMCB,(R9)                                               
*                          VVALMAX SET FULL TO =C'ERR' ON ERRORS                
*                          IT SHOULD ALSO SET FERN AND ROUTNUM                  
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
VVALR228 DS    0H                                                               
         NMOD1 0,V228                                                           
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
*                                                                               
         CLI   RO1,C'Y'            TEST RUN?                                    
         BE    VV228C              YES, DON'T CHECK SOX                         
         BRAS  RE,CKUPDT                                                        
         BNE   VV228C                                                           
         MVI   ROUTNUM,X'74'                                                    
         MVC   FERN,=AL2(1250)                                                  
         B     VV228X                                                           
VV228C   CLC   RNUM,=C'Z7'         Z5, Z7 NEED SYSTEM ID                        
         BE    VV228D                                                           
         CLC   RNUM,=C'Z5'                                                      
         BNE   VV228F                                                           
         CLI   RMARK,X'40'                                                      
         BH    VV228D                                                           
         CLI   RSTA,X'40'                                                       
         BH    VV228D                                                           
         MVI   ROUTNUM,7                                                        
         MVC   FERN,=H'800'        MUST ENTER STATION OR MARKET                 
         B     VV228X                                                           
                                                                                
VV228D   L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF' SYS=FE GETS SYSFAC ADD                      
         BASR  RE,RF                                                            
         L     RE,0(R1)                                                         
         L     RF,VSSB-SYSFACD(RE)                                              
         MVC   RNUM+59(1),SSBSYSN1-SSBD(RF)    SYSTEM ID SEE SSB                
*                                                                               
VV228F   CLC   =C'SOON',BVROUT                                                  
         BNE   VV228X                                                           
         CLI   RO1,C'Y'            TEST RUN?                                    
         BE    VV228X              YES                                          
         CLI   RO4,C'Y'            NO-REPLACE INVOICES?                         
         BE    *+20                YES                                          
         CLI   RO4,C'B'                                                         
         BE    *+12                YES                                          
         CLI   RO4,C'P'                                                         
         BNE   VV228H              NO                                           
*                                                                               
         MVI   ROUTNUM,X'C7'       YES-ERROR                                    
         MVC   FERN,=AL2(FLDINV)                                                
         B     VV228X                                                           
*                                                                               
VV228H   CLC   RNUM,=C'Z7'                                                      
         BE    *+12                                                             
         TM    STASAVE,X'04'       LIVE SOON 1 STATION ONLY                     
         BNO   VV228ERR                                                         
*                                                                               
         CLC   RSTRD,RENDD         LIVE SOON ONE DATE                           
         BE    VV228X                                                           
VV228ERR MVI   ROUTNUM,X'74'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
VV228X   XIT1                                                                   
         EJECT                                                                  
*                                                                               
VVALR231 DS    0H                                                               
         NMOD1 0,V231                                                           
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         CLC   RREPNO+2(3),=C'REG'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'AOR'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'RET'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'ALL'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'COM'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'FIN'                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'CD '                                              
         BE    VR231B                                                           
         CLC   RREPNO+2(3),=C'UF '                                              
         BE    VR231B                                                           
         MVI   ROUTNUM,X'CD'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         B     V231X                                                            
VR231B   DS    0H                                                               
         CLI   RO4,X'40'                                                        
         BNE   V231H                                                            
         GOTO1 DATCON,PLIST,(5,0),(0,TEMP)          TODAY'S DATE                
*  GET DATES IN YMD BINARY FORMAT                                               
         GOTO1 DATCON,PLIST,(0,RBOOK1),(3,TEMP+30)                              
         GOTO1 DATCON,PLIST,(0,TEMP),(3,TEMP+40)                                
         CLC   RBOOK1(6),TEMP                     TODAY VS DATE IN REQ          
         BNL   V231H                                                            
         MVI   TEMP+42,1           SET IN 1ST DAY OF MONTH                      
         CLI   TEMP+41,1           IS TODAY JANUARY                             
         BE    V2310                                                            
         ZIC   R1,TEMP+41          NO/GET TODAYS MONTH                          
         BCTR  R1,0                DROP TO PREVIOUS MOTH                        
         STC   R1,TEMP+41                                                       
         B     V2310A                                                           
V2310    DS    0H                                                               
         MVI   TEMP+41,12          TODAYS MONTH IS JAN/SET DEC                  
         ZIC   R1,TEMP+40                                                       
         BCTR  R1,0                                                             
         STC   R1,TEMP+40          AND SET PREVIOUS YEAR                        
V2310A   DS    0H                                                               
* PER SALLY SAAD MAKE IT 5 MORE MONTHS                                          
         GOTO1 DATCON,PLIST,(3,TEMP+40),(0,TEMP+50)    CONV TO YYMMDD           
         GOTO1 ADDAY,PLIST,TEMP+50,TEMP+60,F'-150'    +5 MONTHS BACK            
         GOTO1 DATCON,PLIST,(0,TEMP+60),(3,TEMP+40)    CONV TO BINARY           
         MVI   TEMP+42,1                               1ST DAY OF MONTH         
*                                                                               
         CLC   TEMP+30(3),TEMP+40     IS REQUESTED DATE LOWER                   
         BNL   V231H               NO/OK SO FAR                                 
*                                                                               
VR231F   MVI   ROUTNUM,X'D6'       YES/ERROR                                    
         MVC   FERN,=AL2(FLDINV)                                                
         XIT1                                                                   
*                                                                               
V231H    DS    0H                                                               
* VALID DATE = YESTERDAY TO 1ST DAY OF MONTH BEFORE LAST                        
         CLI   RPRO1,X'40'                                                      
         BE    V231K                                                            
         GOTO1 DATCON,PLIST,(0,RPRO1),(3,TEMP+30)                               
         CLC   RPRO1(6),TEMP                AGAINST DATE IN REQ                 
         BNL   V231ER                                                           
         CLC   TEMP+30(2),TEMP+40       TEMP+40=1DAY OF LAST MONTH              
         BNL   V231K                    YES/OK                                  
V231ER   MVI   ROUTNUM,X'CE'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         XIT1                                                                   
*                                                                               
V231K    DS    0H                                                               
*                                                                               
V231X    MVI   RO1,C'R'                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VVALR234 DS    0H                                                               
         NMOD1 0,VL234                                                          
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     RC,4(R1)                                                         
         USING WORKD,RC                                                         
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
***      CLC   RPRO,=C'POL'                                                     
***      BNE   V234A                                                            
*                                                                               
         CLI   RNUM+64,C'Y'        TEST 'INCLUDE PIGGYBACKS'                    
         BNE   V2340                                                            
         CLC   RNUM+49(3),SPACES   YES-ONLY VALID IF THERE ARE NO               
         BH    V234ERR1                PARTNER PRODUCTS                         
*        LA    R0,5                                                             
*        LA    R4,RCARD2+23                                                     
*        CLC   0(3,R4),SPACES                                                   
*        BH    V234ERR1                                                         
*        LA    R4,13(R4)                                                        
*        BCT   R0,*-14                                                          
*                                                                               
V2340    DS    0H                                                               
         L     RF,ASAVE                                                         
         CLI   CANAGY-TWAD(RF),C'C'                                             
         BNE   V234A                                                            
         CLI   RMED,C'T'           FOR MEDIA T, R DISALLOW MKT 0000             
         BE    *+12                                                             
         CLI   RMED,C'R'                                                        
         BNE   V234A                                                            
         CLC   RMARK,=C'0000'                                                   
         BNE   V234A                                                            
         MVI   ROUTNUM,X'5E'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         B     V234X                                                            
*                                                                               
V234A    CLC   =C'UNMATCHED',RCARD2+20     ..UNMATCHED                          
****     BNE   V234ERR                                                          
***      B     V234X                                                            
         BE    V234X               NO PROD RESTRICTIONS                         
         CLC   =C'UNAL',STATWRX                                                 
         BE    V234X                                                            
         CLC   =C'DELETE',RCARD2+20                                             
         BE    V234X                                                            
*                                                                               
***      XC    KEY,KEY                                                          
                                                                                
         CLI   RCARD2+32,X'40'                                                  
         BE    V234ERR                                                          
         ZIC   R1,RCARD2+32                                                     
         LA    R2,1                                                             
         CLI   RCARD2+45,X'40'                                                  
         BE    V234B                                                            
         LA    R2,2                                                             
         ZIC   R4,RCARD2+45                                                     
         AR    R1,R4                                                            
V234B    CLI   RCARD2+58,X'40'                                                  
         BE    V234C                                                            
         CHI   R2,2                                                             
         BNE   V234ERR                                                          
         LA    R2,3                                                             
         ZIC   R4,RCARD2+58                                                     
         AR    R1,R4                                                            
V234C    CLI   RCARD2+71,X'40'                                                  
         BE    V234CC                                                           
         CHI   R2,3                                                             
         BNE   V234ERR                                                          
         LA    R2,4                                                             
         ZIC   R4,RCARD2+71                                                     
         AR    R1,R4                                                            
V234CC   CLI   RCARD2+78,X'40'                                                  
         BE    V234D                                                            
         CHI   R2,4                                                             
         BNE   V234ERR                                                          
         ZIC   R4,RCARD2+78                                                     
         AR    R1,R4                                                            
V234D    CHI   R1,100                                                           
         BE    V234E                                                            
         MVI   ROUTNUM,X'DC'                                                    
**       MVI   FERN,X'F0'                                                       
         MVC   FERN,=X'F0F0'                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'*** ERROR WEIGHTS MUST = 100 **'                     
         B     V234X                                                            
*                                                                               
V234ERR  MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'DC'                                                    
         B     V234X                                                            
*                                                                               
V234E    SR    R5,R5               GET SPOT LENGTHS                             
         CLC   RNUM+52(3),SPACES                                                
         BNH   V234F                                                            
         LA    R1,RNUM+52                                                       
         BAS   RE,GETLEN                                                        
         LR    R5,RF               R5 = TOTAL 'FROM' LENGTH                     
         CLC   RNUM+55(3),SPACES                                                
         BNH   V234F                                                            
         LA    R1,RNUM+55                                                       
         BAS   RE,GETLEN                                                        
         AR    R5,RF                                                            
*                                                                               
V234F    XC    FULL,FULL                                                        
         LA    R0,4                                                             
         LA    R2,FULL                                                          
         LA    R4,RCARD2+26        FULL = 'TO' SPOT LENGTHS                     
*                                                                               
V234G    CLC   0(3,R4),SPACES                                                   
         BNH   V234H                                                            
         LR    R1,R4                                                            
         BAS   RE,GETLEN                                                        
         LR    R6,RF                                                            
         CLC   3(3,R4),SPACES      TEST SECONDARY 'TO' SPOT LENGTH              
         BNH   *+14                                                             
         LA    R1,3(R4)            YES-ADD TO FIRST TO GET TOTAL LENGTH         
         BAS   RE,GETLEN                                                        
         AR    R6,RF                                                            
         STC   R6,0(R2)                                                         
*                                                                               
V234H    LA    R2,1(R2)                                                         
         LA    R4,13(R4)                                                        
         BCT   R0,V234G                                                         
*                                                                               
         OC    FULL,FULL           TEST ANY 'TO' LENGTHS                        
         BZ    V234K                                                            
         LTR   R5,R5               YES-MUST HAVE 'FROM' LENGTH                  
         BZ    V234ERR2                                                         
         LA    R0,4                CHECK THAT 'FROM' LENGTH                     
         LA    R2,FULL             AND 'TO' LENGTHS MATCH                       
         SR    RE,RE                                                            
         LA    RF,X'DC'                                                         
*                                                                               
V234J    ICM   RE,1,0(R2)                                                       
         BZ    *+10                                                             
         CR    R5,RE                                                            
         BNE   V234ERR4                                                         
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,V234J                                                         
*                                                                               
V234K    LTR   RE,R5               TEST 'FROM' LENGTH                           
         BNZ   *+18                                                             
         CLC   RNUM+49(3),SPACES   NO-TEST 2ND 'FROM' PRODUCT                   
         BH    V234Q               YES-OK                                       
         B     V234M               NO-CHECK NO 'TO' PAIRS WITHOUT LENS          
         SRDL  RE,1                TEST THAT HALF OF 'FROM' LENGTH              
         LTR   RF,RF               IS A VALID LENGTH                            
         BM    V234M                                                            
         STC   RE,HALF                                                          
         BRAS  RE,CKSLN                                                         
         BNE   V234Q                                                            
*        LA    R2,SLNTAB                                                        
*        LA    R0,15                                                            
*                                                                               
*234L    CLI   0(R2),0                                                          
*        BE    V234M                                                            
*        CLM   RE,1,0(R2)                                                       
*        BE    V234Q               HALF OF 'FROM' LENGTH IS VALID               
*        LA    R2,1(R2)                                                         
*        B     V234L                                                            
*                                                                               
*234L    CLM   RE,1,0(R2)                                                       
*        BE    V234Q               HALF OF 'FROM' LENGTH IS VALID               
*        LA    R2,1(R2)                                                         
*        BCT   R0,V234L                                                         
*                                                                               
V234M    LA    R0,5                                                             
         LA    R2,FULL             FIND ALL 'TO' PRODUCT PAIRS                  
         LA    R4,RCARD2+23                                                     
*                                                                               
V234N    CLC   0(3,R4),SPACES                                                   
         BNH   V234P                                                            
         CHI   R0,1               TEST PAIR HAS LENGTH ASSIGNED                 
         BE    *+12                                                             
         CLI   0(R2),0                                                          
         BNE   V234P                                                            
         LTR   R5,R5               NO-IF THERE'S A 'FROM' LENGTH,               
         BNZ   V234ERR3               IT CANNOT BE SPLIT IN TWO                 
         B     V234ERR2               ELSE, 'FROM' LENGTH MISSING               
*                                                                               
V234P    LA    R2,1(R2)                                                         
         LA    R4,13(R4)                                                        
         BCT   R0,V234N                                                         
*                                                                               
V234Q    CLI   RNUM+64,C'Y'        TEST 'INCLUDE PIGGYBACKS'                    
         BNE   V234R                                                            
         CLC   RNUM+49(3),SPACES   YES-ONLY VALID IF THERE ARE NO               
         BH    V234ERR1                PARTNER PRODUCTS                         
         LA    R0,5                                                             
         LA    R4,RCARD2+23                                                     
         CLC   0(3,R4),SPACES                                                   
         BH    V234ERR1                                                         
         LA    R4,13(R4)                                                        
         BCT   R0,*-14                                                          
*                                                                               
V234R    LA    RF,X'DB'            CHECK THAT PARTNER PRODUCTS                  
         CLC   RPRO,RNUM+49        ARE NOT THE SAME AS THE FIRST                
         BE    V234ERR5                                                         
         LA    R0,5                                                             
         LA    R4,RCARD2+20                                                     
         LA    RF,X'DC'                                                         
*                                                                               
V234S    CLC   0(3,R4),SPACES                                                   
         BNH   *+14                                                             
         CLC   0(3,R4),3(R4)                                                    
         BE    V234ERR5                                                         
         LA    R4,13(R4)                                                        
         LA    RF,1(RF)                                                         
         CHI   R0,2                                                             
         BNE   *+8                                                              
         LA    RF,X'E1'                                                         
         BCT   R0,V234S                                                         
*                                                                               
* ALLOW UPDATIVE SOON (DEC03)                                                   
VR234T   CLC   =C'SOON,',BVROUT    IF SOON                                      
         BNE   V234U                                                            
         CLC   RNUM,=C'K1'         SHOULD ONLY BE K1                            
         BE    V234U                                                            
*                                                                               
V234TER  MVI   ROUTNUM,X'74'                                                    
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(37),=C'*** ERROR UPDATIVE SOON NOT VALID ***'             
         MVC   FERN,=AL2(FE)       MY OWN ERROR MESSAGE                         
V234U    B     V234X                                                            
*                                                                               
V234ERR1 MVC   FERN,=AL2(FLDINV)       'INCLUDE PIGGYBACKS' INVALID             
         MVI   ROUTNUM,X'E0'                                                    
         B     V234X                                                            
*                                                                               
V234ERR2 MVC   FERN,=AL2(NOLEN)          'FROM' LENGTH MISSING                  
         MVI   ROUTNUM,X'DB'                                                    
         B     V234X                                                            
*                                                                               
V234ERR3 MVC   FERN,=AL2(INVLEN)         'FROM' LENGTH INVALID                  
         MVI   ROUTNUM,X'DB'                                                    
         B     V234X                                                            
*                                                                               
V234ERR4 MVC   FERN,=AL2(INVLEN)         'TO' LENGTH INVALID                    
         STC   RF,ROUTNUM                                                       
*                                                                               
V234ERR5 MVC   FERN,=AL2(INVPRD2)        INVALID SECOND PRODUCT                 
         STC   RF,ROUTNUM                                                       
*                                                                               
V234X    XIT1                                                                   
         SPACE 2                                                                
GETLEN   LA    RF,2                GET SPOT LENGTH                              
         CLI   2(R1),C'0'          R1=A(3-BYTE LENGTH)                          
         BNL   GETLEN2                                                          
         BCTR  RF,0                                                             
         CLI   1(R1),C'0'                                                       
         BNL   GETLEN2                                                          
         BCTR  RF,0                                                             
GETLEN2  EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   RF,DUB              RETURN LENGTH IN RF                          
         BR    RE                                                               
         SPACE 2                                                                
*LNTAB   DS    0XL1                                                             
*        DC    AL1(10,15,20,30,40,45,50,60,75,90,120),AL1(0)                    
*LNTAB   DS    0CL15                                                            
*      ++INCLUDE SPSLNTAB                                                       
CKSLN    NTR1                                                                   
         L     R1,SLNTAB                                                        
         LH    RE,0(R1)              GET ENTRY LENGTH                           
         L     RF,2(R1)              DISPL TO EOT                               
         AR    RF,R1                 RELOCATE EOT ADDRESS                       
         AHI   R1,6                  POINT TO FIRST ENTRY                       
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'R'                                                          
         CLI   RMED,C'R'                                                        
         BE    CKSL10                                                           
         CLI   RMED,C'X'                                                        
         BE    CKSL10                                                           
         LA    R0,C'T'                                                          
*                                                                               
CKSL10   CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CKSL15                                                           
         CLC   RAGY,0(R1)          MATCH AGY                                    
         BNE   *+12                                                             
CKSL15   CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    CKSL20                                                           
*                                                                               
         BXLE  R1,RE,CKSL10        NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CKSL20   AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,HALF             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
VVALMAX  DS    0H                                                               
         NMOD1 0,VMAX                                                           
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
*                                                                               
*                                  RFP OR                                       
         L     R4,AIORFP           SYMBOLIC NAME ESC SEQ/SKIP VMAX              
         USING RFPBLK,R4                                                        
         TM    RFPSTAT,RFPINUSE  ..ARE WE IN $RFP                               
         BZ    VALMAX00                                                         
         B     VALMAXX           ..JUST EXIT VMAX - WITH NEW NAMES              
*                                .. CHECKING IS TOO CONVOLUTED                  
*                                .. AND RFP = DATE FLEXIBLE SO NO               
*                                .. REASON TO CHECK ANYWAY                      
****************************************************                            
         CLI   RSTRD,C'!'          SYMBOLIC NAME?                               
         BE    VALMAXX             YES/EXIT VMAX                                
         ZIC   R0,RFPVNUMS                                                      
VALMAX0  CLC   RSTRD(3),RFPVSYME                                                
         BE    VALMAXX                                                          
         LA    R4,RFPVSYML(R4)                                                  
         BCT   R0,VALMAX0                                                       
*******************************************************                         
*                                                                               
         DROP R4                                                                
VALMAX00 MVI   ROUTNUM,09                                                       
         TM    SEDSAVE,X'10'                                                    
*                                                                               
         BO    VALMAX4      CHANGE TO VALMAX4   WHEN READY                      
*                                                                               
*                                   FROM ESTIMATE                               
                                                                                
*  CHANGE FOR 21 CENTURY DATES                                                  
*                                                                               
**       LA    R5,6                          CONVERT TO BINARY AT TEMP          
**       LA    R6,RSTRD                                                         
**       LA    R7,TEMP                                                          
**VALMAX1  PACK  DUB,0(2,R6)                                                    
**       CVB   R0,DUB                                                           
**       STC   R0,0(R7)                                                         
**       LA    R6,2(R6)                                                         
**       LA    R7,1(R7)                                                         
**       BCT   R5,VALMAX1                                                       
*                                                                               
         GOTO1 DATCON,PLIST,(0,RSTRD),(3,TEMP)                                  
         GOTO1 DATCON,PLIST,(0,RENDD),(3,TEMP+3)                                
*                                                                               
         SR    R5,R5                         CALCULATE DELTA MONTHS             
         SR    R6,R6                                                            
         IC    R5,TEMP+1                                                        
         IC    R6,TEMP+4                                                        
         SR    R6,R5                         R6=(END-STR) MONTH                 
         SR    R7,R7                                                            
         IC    R5,TEMP                                                          
         IC    R7,TEMP+3                                                        
         SR    R7,R5                         R7=(END-STR) YEAR                  
         BM    VALMAXE                                                          
         LR    R5,R6                                                            
         LA    R6,12                                                            
         MR    R6,R6                         R7=(END-STR) YEAR * 12             
         AR    R5,R7                                                            
         STC   R5,HALF1                      HALF1(1) = DELTA MONTHS            
*                                                                               
         SLL   R5,2                          CALCULATE DELTA WEEKS              
         BCTR  R5,0                                                             
         BCTR  R5,0                          R5=(MONTHS*4)-2                    
         SR    R6,R6                                                            
         IC    R7,TEMP+5                                                        
         ST    R8,FULL                                                          
         LA    R8,7                                                             
         DR    R6,R8                                                            
         AR    R5,R7                         R5=R5+(ENDDAY/7)                   
         SR    R6,R6                                                            
         IC    R6,TEMP+2                                                        
         LA    R7,28                                                            
         SR    R7,R6                                                            
         LPR   R7,R7                                                            
         SR    R6,R6                                                            
         DR    R6,R8                                                            
         L     R8,FULL             RESTORE BASE REGISTER                        
         AR    R5,R7                         R5=R5+(28-STRDAY)/7                
         STC   R5,HALF1+1                    HALF1+1(1) = DELTA WEEKS           
*                                                                               
         SR    R5,R5                                                            
         IC    R5,HALF+1                     POINT TO MONTHS OR WEEKS           
         LA    R5,HALF1(R5)                                                     
         CLC   HALF(1),0(R5)                 COMPARE INPUT MAX                  
         BNL   VALMAXX                                                          
*                                                                               
VALMAXE  MVC   FERN,=AL2(SEDBIG)            ERROR DURATION TOO LONG             
         MVC   FULL,=C'ERR'                                                     
         B     VALMAXX                                                          
*                                                                               
*                                                                               
VALMAX4  LA    R5,ESTABLE                                                       
         LA    R6,4                                                             
VALMAX5  CLC   RNUM,0(R5)                                                       
         BE    VALMAX6                                                          
         LA    R5,2(R5)                                                         
         BCT   R6,VALMAX5                                                       
         B     VALMAXX                                                          
*                                                                               
VALMAX6  CLI   ESTDATES,0                                                       
         BE    VALMAXX                                                          
         MVC   RSTRD(12),ESTDATES                                               
         MVC   RES,SPACES                                                       
         B     VALMAXX                                                          
*                                                                               
ESTABLE  DC    C'18196080'                                                      
VALMAXX  XIT1  REGS=(R5)                                                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VVALIDS  DS    0H                                                               
         NMOD1 0,VIDS                                                           
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           MUST READ AGYHEADER                          
         MVC   KEY+1(2),RAGY                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+6                                                              
         DC    H'0'                DISK ERROR OR NOT FOUND                      
         LA    R7,SPTREC                                                        
         USING AGYHDRD,R7                                                       
         CLI   AGYPROF+9,C'A'      IDS ALLOWED                                  
         BE    VALIDSX                                                          
         CLI   AGYPROF+9,C'Y'                                                   
         BE    VALIDSX                                                          
         MVI   ROUTNUM,32                                                       
         MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
VALIDSX  XIT1                                                                   
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*        SOON REQUESTS                                                          
*                                                                               
DOSOON   DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,DOSOON,RR=R6                                                   
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                                                       
         L     RC,4(R1)                                                         
         USING WORKD,RC                                                         
         L     R3,ASAVE                                                         
****->   USING TWAD,R3                                                          
         USING SPOOK,R2                                                         
         XC    TEMP(SPOOKXL),TEMP   BUILD SPOOK BLOCK                           
         LA    R2,TEMP                                                          
         MVC   SPOOKXT,=C'XT='      SET EXTRA LENGTH                            
         MVC   SPOOKUID,USRID      CONNECT ID                                   
         LR    R5,R3                                                            
         USING TWAD,R5                                                          
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
*                                                                               
         CLI   COMSCORE,C'Y'       TEST RERATE REQ WITH COMSCORE DEMS           
         JNE   *+8                                                              
         MVI   SPOOKSML,C'C'                                                    
*                                                                               
         USING T208FFD,R5                                                       
         MVC   SPOOKDID,BVROUT+5   USER INITIALS (ID)                           
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
*  ARE WE IN SPOT OR NET                                                        
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   DSO3                                                             
         MVC   SPOOKSYS,=C'NE'     NET SYSTEM                                   
         MVC   SENUMBER,FASYS      PASS SE NUMBER                               
         B     *+10                                                             
DSO3     MVC   SPOOKSYS,=C'SP'     SPOT SYSTEM                                  
         MVC   SPOOKEOD,RNUM                                                    
         MVC   SPOOKJCL,RNUM                                                    
         MVI   SPOOKWEN,2          SET SOON STATUS                              
*                                                                               
         CLC   RNUM,=C'SS'         ,,IF SS                                      
         BNE   DSZ5                                                             
         CLI   RO3,C'D'            IS REPORT DOWNLOADABLE?                      
         BNE   DSZ5                                                             
         OI    SPOOKTY,X'10'       DOWNLOADABLE REPORT                          
*                                                                               
DSZ5     CLC   RNUM,=C'Z5'         ,,IF Z5                                      
         BE    *+14                                                             
         CLC   RNUM,=C'Z7'         ,,IF Z7                                      
         BNE   DSDX                                                             
*                                                                               
         CLI   RO1,C'Y'            IF TEST RUN -> NON UPDATIVE                  
         BE    DSO5                                                             
         MVI   SPOOKWEN,5          ,,SPOOKWEN=5 + NO LOCKET                     
         B     DSO5                                                             
*                                                                               
DSDX     CLC   RNUM,=C'DX'         ,,IF DX                                      
         BNE   *+12                                                             
         MVI   SPOOKWEN,5          ,,SPOOKWEN=5 + NO LOCKET                     
         B     DSO5                                                             
*                                                                               
         CLC   RNUM,=C'07'         ,,IF 07 (SPOT UNBILLING)                     
         BNE   *+12                                                             
         MVI   SPOOKWEN,5          ,,SPOOKWEN=5 + NO LOCKET                     
         B     DSO5                                                             
*                                                                               
         CLC   RNUM,=C'7U'         ,,IF 7U (NET UNBILLING)                      
         BNE   *+12                                                             
         MVI   SPOOKWEN,5          ,,SPOOKWEN=5 + NO LOCKET                     
         B     DSO5                                                             
*                                                                               
         BAS   RE,DOUPSOON         CHECK IF UPDATIVE SOON                       
         BE    DSOONX              PROBLEM/DONT WRITE OUT REQUEST               
*                                                                               
DSO5     DS    0H                  CHECK UPD SOONS FOR SOX                      
         CLI   SPOOKWEN,5          IF NON-UPDATIVE, NO CKECK                    
         BNE   DSO10                                                            
         BRAS  RE,CKUPDT                                                        
         BE    DSOONX              ERROR, NOT AUTHORIZED TO UPDATE              
*                                                                               
DSO10    L     R4,APARM                                                         
         L     R4,16(R4)           A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A73'        REQTWA                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(5,(R3)),REQREC+54,DATAMGR,(R4),(R2)                   
         CLI   MULTNUM,0           IF PASSING MULTIPLE REQUESTS                 
         BNE   DSOONX              ONLY PUT OUT MESSAGE ONCE                    
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(38),=C'REPORT XXX,9999 WILL BE PROCESSED SOON'            
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   BVRHDR+7(3),2(RE)                                                
         LH    RF,6(RE)                                                         
         LA    R4,BVRHDR+11                                                     
         EDIT  (RF),(4,(R4)),ALIGN=LEFT,WRK=TEMP+60                             
         CLC   BVRHDR+7(3),SPACES                                               
         BH    *+10                                                             
         MVC   BVRHDR(38),=CL38'REPORT IS NOT SET UP FOR SOON'                  
*                                                                               
         MVC   FERN,=AL2(FE)            SET FOR MY MESSAGE                      
         B     DSOONX                                                           
         EJECT                                                                  
* WRITES OUT LOCKET RECORDS FOR UPDATIVE SOON                                   
DOUPSOON NTR1                                                                   
* - UPDATIVE SOON REQUESTS                                                      
         CLC   RNUM,=C'I2'         I2/N2 UPDATIVE SOON                          
         BNE   DS06                (N2 REQUEST CHANGED AT THIS POINT            
******** CLC   =C'ALL',RSTA        (BACK TO I2)                                 
******** BNE   *+8                 STEV PEOPLE'S REQ TO COMNT OUT               
******** MVI   SPOOKSML,C'L'       BELONGS TO 'LONG' SOON CLASS                 
         CLI   RO5,C'Y'                                                         
         BE    DS04                                                             
         CLI   RO5,C'P'                                                         
         BNE   DSOK                                                             
DS04     MVI   SPOOKWEN,5          SET SOON UPDATIVE STATUS                     
         BRAS  RE,CKUPDT                                                        
         BE    DSNOK                                                            
         BRAS  RE,ADDLOCKS                                                      
         BE    DSNERR                                                           
         B     DSOK                                                             
*                                                                               
DS06     CLC   RNUM,=C'BU'         NET LIVE BILLING ?                           
         BNE   DS10                                                             
         BRAS  RE,BLDNBLK                                                       
         BE    DSNERR                                                           
         B     DS40                                                             
*                                                                               
DS10     CLC   RNUM,=C'B1'         B1 UPDATIVE SOON ?                           
         BNE   DS10A                                                            
         BAS   RE,BLDB1LK                                                       
         B     DS40                                                             
*                                                                               
         USING LKKEYD,IFLD                                                      
DS10A    XC    LOCKEY,LOCKEY                                                    
         MVC   LOCKAGY,RAGY                                                     
         CLC   RNUM,=C'K1'         K1 UPDATIVE SOON                             
         BE    DS21                (NO TEST RUN FOR K1)                         
         CLC   RNUM,=C'K4'         K4,KL & K5 ALLOW UPDATIVE SOONS              
         BE    DS20                                                             
         CLC   RNUM,=C'KL'                                                      
         BE    DS20                                                             
         CLC   RNUM,=C'K5'                                                      
         BNE   DSOONX                                                           
*                                                                               
         CLI   RO4,C'Y'            TEST RUN                                     
         BE    DSOK                YES/EXIT                                     
         OC    LOCKLST,LOCKLST                                                  
         BNZ   *+6                                                              
         DC    H'0'                LOCKLST MUST HAVE DATA AT THIS POINT         
* - LOCK RECORD - DON'T USE TEMP TO BUILD KEY/TEMP IN USE BY CALLER             
         MVC   LOCKRTY,=C'UN'      LOCKING UNIT RECORDS                         
         MVC   LKUNCLT,LOCKLST                                                  
         MVC   LKUNSTA,SPACES                                                   
         B     DS40                                                             
*                                                                               
DS20     CLI   RO4,C'Y'            TEST RUN                                     
         BE    DSOK                YES/EXIT                                     
*                                                                               
DS21     CLI   MLTREQSW,C'Y'       DISALLOW MULT REQUEST FOR UPSOON             
         BNE   DS21C                                                            
         MVC   BVRHDR(51),=C'MULTI-STATION REQUEST NOT ALLOWED FOR UPDAX        
               TIVE SOON'                                                       
         MVI   ROUTNUM,X'7'                                                     
         MVC   FERN,=AL2(FE)                                                    
         B     DSNOK                                                            
*                                                                               
DS21C    MVC   LOCKRTY,=C'BA'      SET BRAND ALLOCATION LOCK                    
         MVC   LOCKKEY,SPACES                                                   
         MVC   LKBAMED,RMED                                                     
         MVC   LKBACLT,RCLI                                                     
* FOR 'ALL' ESTIMATE REQUEST THERE CAN'T BE ANY ESTIMATES LOCKED!               
         XC    LKBAEST,LKBAEST     NULL TO TEST FOR ANY ESTIMATE                
         CLC   REST,=C'ALL'        TEST ALL ESTIMATE REQUEST                    
         BE    DS22                                                             
         CLC   REST,=C'NO '        TEST NO  ESTIMATE REQUEST                    
         BE    DS22                                                             
         CLC   REST1,SPACES        TEST RANGE OF ESTIMATES                      
         BNE   DS22                                                             
* SPECIFIC ESTIMATE, NEITHER THAT EST NOR ALL ESTS CAN BE LOCKED!               
         MVC   LKBAEST,REST        CHECK FOR SPECIFIC ESTIMATE LOCK             
*                                  ENSURE ESTIMATE(S) ARE NOT LOCKED            
DS22     GOTO1 VLOCKET,DMCB,('LKTESTQ',LKKEYD),ACOMFACS                         
         CLI   4(R1),2                                                          
         BE    DS22                                                             
         CLI   4(R1),0                                                          
         BNE   DSNERR                                                           
*                                                                               
         L     RF,ASAVE            CANADA - CHECK ALL APPROPRIATE MEDIA         
         CLI   CANAGY-TWAD(RF),C'C'                                             
         BNE   DS25                                                             
         CLI   RMED,C'C'           TEST CANADIAN COMBINED MEDIA                 
         BNE   DS23                                                             
         CLI   LKBAMED,C'C'                                                     
         BNE   *+12                                                             
         MVI   LKBAMED,C'N'        ALSO TEST FOR NETWORK                        
         B     DS22                                                             
         CLI   LKBAMED,C'N'                                                     
         BNE   DS24                                                             
         MVI   LKBAMED,C'T'        ALSO TEST FOR SELECTIVE TELEVISION           
         B     DS22                                                             
*                                                                               
DS23     CLI   RMED,C'T'           TEST CANADIAN TELEVISION                     
         BE    *+12                                                             
         CLI   RMED,C'N'           OR CANADIAN NETWORK                          
         BNE   DS24                                                             
         CLI   LKBAMED,C'C'        ALSO TEST FOR COMBINED MEDIA                 
         BE    DS24                -DONE                                        
         MVI   LKBAMED,C'C'                                                     
         B     DS22                                                             
DS24     MVC   LKBAMED,RMED        RESTORE ORIGINAL REQUEST MEDIA               
*                                                                               
DS25     CLC   LKBAEST,SPACES                                                   
         BE    *+14                                                             
         MVC   LKBAEST,SPACES      ALSO CHECK FOR ALL ESTIMATE LOCK             
         B     DS22                                                             
*                                                                               
         MVC   LKBAEST,SPACES      SET ALL ESTIMATE LOCK                        
         CLC   REST,=C'ALL'        TEST ALL ESTIMATE REQUEST                    
         BE    DS40                                                             
         CLC   REST,=C'NO '        TEST NO  ESTIMATE REQUEST                    
         BE    DS40                                                             
         CLC   REST1,SPACES        TEST RANGE OF ESTIMATES                      
         BNE   DS40                                                             
         MVC   LKBAEST,REST        NO - SET SINGLE ESTIMATE FOR LOCKING         
*                                                                               
DS40     BRAS  RE,CKUPDT                                                        
         BE    DSNOK                                                            
         GOTO1 VLOCKET,DMCB,('LKLOCKQ',LKKEYD),ACOMFACS                         
         CLI   4(R1),2                                                          
         BE    DS40                                                             
         MVI   SPOOKWEN,5          SET SOON UPDATIVE STATUS                     
         CLI   4(R1),0                                                          
         BE    DSOK                OK                                           
*                                                                               
DSNERR   XC    BVRHDR,BVRHDR       ERRORS                                       
         CLC   =C'B1',RNUM                                                      
         BE    *+14                                                             
         CLC   =C'BU',RNUM                                                      
         BNE   *+14                                                             
*                                                                               
         MVC   BVRHDR(30),=C'*LB REQUEST PENDING-TRY LATER*'                    
         B     *+10                                                             
*                                                                               
         MVC   BVRHDR(33),=C'*** CLIENT LOCKED - TRY LATER ***'                 
*                                                                               
         MVC   FERN,=AL2(FE)                                                    
         B     DSNOK                                                            
*                                                                               
DSNOK    SR    RE,RE                                                            
DSOK     LTR   RE,RE                                                            
DSOONX   XIT1                                                                   
                                                                                
L        USING LKKEYD,IFLD                                                      
BLDB1LK  DS    0H                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         CLC   =C'LOCKOUT',RNAME   CHECK IF DOING ALL AGENCY LOCKOUT            
         BNE   BLDB1L5                                                          
         L     RF,ASAVE                                                         
         USING TWAD,RF                                                          
         CLI   DDS,1               IS IT DDS TERMINAL?                          
         BNE   BLDB1L5             NO, CAN'T DO THAT                            
         DROP  RF                                                               
         CLC   =C'TSO',BVROUT+5                                                 
         BNE   BLDB1L5                                                          
         XC    L.LOCKAGY,L.LOCKAGY ISSUE LOCK FOR ALL AGENCIES                  
         B     *+10                                                             
BLDB1L5  MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'B1'    B1 REPORT                                    
         BR    RE                                                               
         DROP  L                                                                
         SPACE 2                                                                
BLDNBLK  NTR1  BASE=*,LABEL=*                                                   
L        USING LKKEYD,IFLD         TEST IF LOCK THERE FIRST                     
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'NB'    BU REPORT                                    
*                                                                               
BLDNB5   GOTO1 VLOCKET,DMCB,('LKTESTQ',LKKEYD),ACOMFACS                         
         CLI   4(R1),2                                                          
         BE    BLDNB5                                                           
         CLI   4(R1),0                                                          
         BNE   BLDNBERR            IF YES, CAN'T DO ANOTHER LB                  
*                                                                               
         XC    L.LOCKEY,L.LOCKEY   NO, BUILD LOCK                               
         MVC   L.LOCKRTY,=C'NB'                                                 
         CLC   =C'LOCKOUT',RNAME   CHECK IF DOING ALL AGENCY LOCKOUT            
         BNE   BLDNB15                                                          
         L     RF,ASAVE                                                         
         USING TWAD,RF                                                          
         CLI   DDS,1               IS IT DDS TERMINAL?                          
         BNE   BLDNB15             NO, CAN'T DO THAT                            
         DROP  RF                                                               
         CLC   =C'TSO',BVROUT+5                                                 
         BE    BLDNBOK             ISSUE LOCK FOR ALL AGENCIES                  
BLDNB15  MVC   L.LOCKAGY,RAGY                                                   
         CLI   RBOOK,C'G'          FOR MANUAL BILL NO FURTHER LOCK              
         BE    BLDNBOK                                                          
         MVC   L.LKNBCLT,RCLI                                                   
         CLI   REST,C'0'           PUT IN SINGLE EST                            
         BL    *+10                                                             
         MVC   L.LKNBEST,REST                                                   
*                                                                               
         CLI   RO5,C' '            ANY SUBMEDIA ?                               
         BNH   *+14                                                             
         MVI   L.LKNBNET,C'*'                                                   
         MVC   L.LKNBNET+1(1),RO5                                               
*                                                                               
         CLI   RSTA,C' '           ANY STATION ?                                
         BNH   BLDNBOK                                                          
         CLC   =C'ALL',RSTA                                                     
         BE    BLDNBOK                                                          
         MVC   L.LKNBNET,RSTA                                                   
         B     *+6                                                              
*                                                                               
BLDNBERR SR    RE,RE                                                            
BLDNBOK  LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  L                                                                
**************************************************************                  
ADDLOCKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,BLDNVLK                                                       
* TEST NV LOCK - IF NOT AVAILABLE, STOP                                         
ADDLK2   GOTO1 VLOCKET,DMCB,('LKTESTQ',IFLD),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK2                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         GOTOR TSTBALK             TEST FOR BUY ALLOCATION LOCKS                
         BNE   ADDLKERR                                                         
* ADD BUY LOCK                                                                  
         L     R1,ASAVE                                                         
         USING TWAD,R1                                                          
         TM    ODDMNTS,X'04'       IS IT NET?                                   
         BNO   ADDLK3                                                           
         BAS   RE,BLDUNLK          YES/UNIT LOCK                                
         B     ADDLK4                                                           
ADDLK3   BAS   RE,BLDBULK                                                       
         DROP  R1                                                               
*                                                                               
ADDLK4   GOTO1 VLOCKET,DMCB,('LKLOCKQ',IFLD),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         BAS   RE,BLDNVLK                                                       
*                                                                               
ADDLK6   GOTO1 VLOCKET,DMCB,('LKLOCKQ',IFLD),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK6                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
         B     ADDLKOK                                                          
*                                                                               
ADDLKX   XIT1                                                                   
*                                                                               
ADDLKERR SR    RE,RE                                                            
ADDLKOK  LTR   RE,RE                                                            
         B     ADDLKX                                                           
*                                                                               
L        USING LKKEYD,IFLD                                                      
*                                                                               
BLDNVLK  DS    0H                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'NV'    INVOICE RECORDS                              
         MVC   L.LKNVMED,RMED                                                   
         MVC   L.LKNVCLT,RCLI                                                   
         MVC   L.LKNVSTA,RSTA                                                   
         CLI   RMED,C'X'           IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKNVSTA+4,C'X'    MAKE STATION X                               
*                                                                               
         L     R1,ASAVE                                                         
         USING TWAD,R1                                                          
         CLI   CANAGY,C'C'         IF CANADIAN                                  
         BNE   BLDNV10                                                          
         DROP  R1                                                               
**       CLI   RSTA+4,X'40'                                                     
         CLI   L.LKNVSTA+4,X'40'                                                
         BHR   RE                                                               
         MVC   L.LKNVSTA+4(1),RMED                                              
         BR    RE                                                               
*BLDNV10  CLI   RSTA+4,X'40'        ELSE                                        
BLDNV10  CLI   L.LKNVSTA+4,X'40'   ELSE                                         
         BH    *+8                                                              
         MVI   L.LKNVSTA+4,C'T'    USE T                                        
         L     R1,ASAVE                                                         
         USING TWAD,R1                                                          
         TM    ODDMNTS,X'04'       IS IT NET?                                   
         BNO   BLDNV15                                                          
         CLI   RSTA+4,X'40'        AND NO NETWORK INDICATOR                     
         BH    BLDNV15                                                          
         MVI   L.LKNVSTA+4,C'N'    MAKE IT N                                    
BLDNV15  BR    RE                                                               
         DROP  R1                                                               
         DROP  L                                                                
*                                                                               
L        USING LKKEYD,IFLD                                                      
*                                                                               
BLDBULK  XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'BU'    BUY RECORDS                                  
         MVC   L.LKBUMED,RMED                                                   
         MVC   L.LKBUCLT,RCLI                                                   
         MVC   L.LKBUSTA,RSTA                                                   
                                                                                
         CLI   RMED,C'X'           IF MEDIA = X                                 
         BNE   *+8                                                              
         MVI   L.LKBUSTA+4,C'X'    MAKE STATION X                               
                                                                                
         L     R1,ASAVE                                                         
         USING TWAD,R1                                                          
         CLI   CANAGY,C'C'         IF CANADIAN                                  
         BNE   BLDBU10                                                          
         DROP  R1                                                               
**       CLI   RSTA+4,X'40'                                                     
         CLI   L.LKBUSTA+4,X'40'                                                
         BHR   RE                                                               
         MVC   L.LKBUSTA+4(1),RMED                                              
         BR    RE                                                               
*BLDBU10  CLI   RSTA+4,X'40'                                                    
BLDBU10  CLI   L.LKBUSTA+4,X'40'                                                
         BH    *+8                                                              
         MVI   L.LKBUSTA+4,C'T'                                                 
         BR    RE                                                               
         DROP  L                                                                
*                                                                               
L        USING LKKEYD,IFLD                                                      
*                                                                               
BLDUNLK  XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'UN'    UNIT RECORDS                                 
         MVC   L.LKUNCLT,RCLI                                                   
         MVC   L.LKUNSTA,RSTA                                                   
         BR    RE                                                               
         DROP  L                                                                
*                                  TEST FOR BUY ALLOCATION LOCKS                
TSTBALK  NTR1  ,                                                                
         LA    R2,FULL             R2=A(MEDIA LIST FOR LOCK TESTS)              
         MVC   0(L'RMED,R2),RMED   ALWAYS TEST FOR REQUESTED MEDIA              
         LA    R0,1                (R0=MEDIA LOOP COUNT)                        
         L     RF,ASAVE                                                         
         CLI   CANAGY-TWAD(RF),C'C'                                             
         BNE   TSTBALK4                                                         
         CLI   RMED,C'C'           TEST CANADIAN COMBINED MEDIA                 
         BNE   TSTBALK2                                                         
         MVI   1(R2),C'N'          YES - TEST FOR NETWORK                       
         MVI   2(R2),C'T'          AND TELEVISION ALSO                          
         LA    R0,3                                                             
         B     TSTBALK4                                                         
*                                                                               
TSTBALK2 CLI   RMED,C'T'           TEST CANADIAN TELEVISION                     
         BE    *+12                                                             
         CLI   RMED,C'N'           OR CANANDIAN NETWORK                         
         BNE   TSTBALK4                                                         
         MVI   1(R2),C'C'          YES - TEST FOR COMBINED MEDIA ALSO           
         LA    R0,2                                                             
*                                                                               
L        USING LKKEYD,IFLD                                                      
TSTBALK4 XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,RAGY                                                   
         MVC   L.LOCKRTY,=C'BA'    BUY ALLOCATION LOCK TYPE                     
         MVC   L.LOCKKEY,SPACES                                                 
         MVC   L.LKBAMED,0(R2)                                                  
         MVC   L.LKBACLT,RCLI                                                   
* FOR 'ALL' ESTIMATE REQUEST THERE CAN'T BE ANY ESTIMATES LOCKED                
         XC    L.LKBAEST,L.LKBAEST NULL TO TEST FOR ANY ESTIMATE                
         CLC   REST1,SPACES        TEST ESTIMATE RANGE OR FILTERS               
         BNE   TSTBALK6                                                         
         TM    REST,X'F0'          TEST SINGLE ESTIMATE REQUEST                 
         BNO   TSTBALK6                                                         
* SPECIFIC ESTIMATE, NEITHER THAT EST NOR ALL ESTS CAN BE LOCKED!               
         MVC   L.LKBAEST,REST      YES - TEST FOR SPECIFIC ESTIMATE             
*                                                                               
TSTBALK6 GOTOR TSTLOK              TEST FOR SPECIFIC/ALL ESTIMATES              
         BNE   TSTBALKX                                                         
         CLC   L.LKBAEST,SPACES    TEST ALL ESTIMATES CHECKED                   
         BE    TSTBALK8                                                         
         MVC   L.LKBAEST,SPACES                                                 
         GOTOR TSTLOK              TEST FOR ALL ESTIMATES                       
         BNE   TSTBALKX                                                         
*                                                                               
TSTBALK8 LA    R2,1(R2)            BUMP TO NEXT MEDIA LETTER                    
         BCT   R0,TSTBALK4         DO FOR NUMBER OF MEDIAS                      
         CR    R0,R0               SET CC=EQUAL IF NO LOCKS PRESENT             
*                                                                               
TSTBALKX B     ADDLKX                                                           
*                                                                               
TSTLOK   NTR1  ,                                                                
TSTLOK2  GOTOR VLOCKET,DMCB,('LKTESTQ',L.LKKEYD),ACOMFACS                       
         CLI   4(R1),2                                                          
         BE    TSTLOK2                                                          
         CLI   4(R1),0                                                          
         B     ADDLKX                                                           
         DROP  L                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
CKUPDT   NTR1  BASE=*,LABEL=*                                                   
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         ICM   RF,15,(CXTRAINF-COMFACSD)(RF)                                    
         BZ    UPDOK               ? I GUESS ALLOW IT ?                         
         USING XTRAINFD,RF                                                      
******   TM    XIFLAG1,XITSTADV    IS THIS TST SYSTEM                           
******   BNZ   UPDOK               YES, WHO CARES                               
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    UPDOK                                                            
         DROP  RF                                                               
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(36),=CL36'NOT AUTHORIZED TO UPDATE THIS SYSTEM'           
         MVC   FERN,=AL2(FE)            SET FOR MY MESSAGE                      
UPDNOK   SR    RE,RE                                                            
UPDOK    LTR   RE,RE                                                            
         XIT1                                                                   
*                                                                               
BVRMSG1  DS    CL60                                                             
         ORG   BVRMSG1                                                          
         DC    C'Request Cannot Be Added'                                       
         DC    C'to a submitted group'                                          
         ORG                                                                    
BVRMSG2  DS    CL60                                                             
         ORG   BVRMSG2                                                          
         DC    C'Request Cannot Be Added.'                                      
         DC    C'Maximum is 50 requests'                                        
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* ERROR EQUATES                                                                 
DSTNMIS  EQU   1140                DESTINE RECORD MISSING                       
DARERR   EQU   1139                DARE ORDER EXISTS                            
UIDMISM  EQU   1177                UNIQU IDS DONT MATCH TO STATIONS             
MKTMISM  EQU   1181                OLD MKT CAN'T MATCH NEW MKT                  
ECMISS   EQU   1185                EC= MISSING ON CLT REC                       
ECMISM   EQU   1186                ECOST/CTYP MISMATCH                          
PRDTOG   EQU   1198                PRODUCTS HAVE TO BE TOGETHER                 
ESTTOG   EQU   1200                ESTIMATES HAVE TO BE TOGETHER                
NOSUBM   EQU   1213                NO STATION ON BN - NO SUBMEDIA               
I2NERR   EQU   1282                I2N PROFILE ERROR                            
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LKBUMED  DS    XL1                                                              
LKBUCLT  DS    XL3                                                              
LKBUSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKBAMED  DS    CL1                                                              
LKBACLT  DS    CL3                                                              
LKBAEST  DS    CL3                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKNVMED  DS    XL1                                                              
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL5                                                              
         ORG   LOCKKEY                                                          
LKUNCLT  DS    XL3                                                              
LKUNSTA  DS    XL4                                                              
         ORG   LOCKKEY                                                          
LKNBCLT  DS    XL3                                                              
LKNBEST  DS    XL3                                                              
LKNBNET  DS    XL4                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
AMTMIS   EQU   01                            AMOUNT MISSING                     
AMTPRO   EQU   100                           PRO/EST CANT BE ALL/NO             
PROPOL   EQU   101                           PRODUCT MUST BE POL                
PROPOLX  EQU   102                           PRODUCT CANT BE POL                
PRONOF   EQU   41                            PRODUCT NOT ON FILE                
MOSERR   EQU   79                                                               
SEDSGE   EQU   80                            START DATE GT END DATE             
SEDBIG   EQU   103                           DATE SPREAD TOO LONG               
CLIPRF   EQU   110                 NOT DEFINED BY CLT PROF                      
CLIPRF1  EQU   104                 SINGLE EST BILLING                           
CLIPRF2  EQU   105                 EST=NO BILLING                               
CLIPRF3  EQU   106                 EST=NNN-NNN BILLING                          
CLIPRF4  EQU   107                 RATING SERVICE UNLIKE CLT PROF               
ESTNO2   EQU   108                           CANT HAVE SECOND EST               
ESTALLX  EQU   109                           EST CANT BE ALL                    
CLIALL   EQU   112        CLIENT MUST BE ALL                                    
MOSINV   EQU   113                    MONTH OF SERVICE FOR MC ONLY              
DPTNFD   EQU   53                                                               
MKTALL   EQU   02                  MKT MUST BE ALL FOR NEW BILLING              
NOPROF   EQU   94                                                               
PROFERR  EQU   95                                                               
ACCERR   EQU   207                 LMT ACCESS ERROR                             
NOLEN    EQU   243                 SPOT LENGTH MISSING                          
INVLEN   EQU   151                 SPOT LENGTH NOT VALID                        
INVPRD2  EQU   27                  INVALID SECOND PRODUCT                       
GMCGRERR EQU   1317                CHECK CLIENTS IN CGROUP FOR GM               
TRDERR   EQU   1332                CASH/TRADE NOT ALLOWED FOR SOON              
TRDMIX   EQU   1333                MIX OF TRADE OPTIONS                         
TRDFRT   EQU   1334                MIX OF TRADE AND F-RATE OPTIONS              
         EJECT                                                                  
WORKD    DSECT                                                                  
RELO     DS    F                                                                
VLOCKET  DS    V                                                                
DISPADR  DS    F                             A(NEXT DISP LINE ON SCR)           
SKIPCTR  DS    H                             NUM OF RECS SKIPPED                
READCTR  DS    H                             NUM OF RECS READ                   
CANCCTR  DS    H                             NUM OF RECS CANCELLED              
DISPCTR  DS    H                             NUM OF RECS DISPLAYED              
TOTCTR   DS    256H                                                             
ACHKRNUM DS    A                                                                
AVVALMAX DS    A                                                                
LOCKLST  DS    CL20                          KEY LIST PASSED TO LOCKET          
SENUMBER DS    CL1                                                              
WORKDX   EQU   *                                                                
         SPACE 2                                                                
DISPLD   DSECT                                                                  
DLCANCH  DS    CL8                                                              
DLCANC   DS    CL1                                                              
DLHDR    DS    CL8                                                              
DLINE    DS    0CL77                                                            
DLNUM    DS    CL2                                                              
DLREQ    DS    CL75                                                             
DLNEXT   EQU   *                                                                
         ORG   *+DLHDR-DISPLD                                                   
DLHDR2   DS    CL8                                                              
DLINE2   DS    0CL77                                                            
DLREQ2   DS    CL77                                                             
         EJECT                                                                  
       ++INCLUDE SPREQSAVE                                                      
         EJECT                                                                  
       ++INCLUDE SPREQTEMP                                                      
         EJECT                                                                  
       ++INCLUDE SPREQFFBD                                                      
         EJECT                                                                  
       ++INCLUDE SPREQFEBD                                                      
       PRINT OFF                                                                
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE FLDIND                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE GERFPIOD                                                       
************LUDE FALOCKETD                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDESTN                                                     
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE SPGENSTAFX                                                     
       ++INCLUDE DDGETPROFD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       PRINT ON                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPREQ02   01/06/20'                                      
         END                                                                    
