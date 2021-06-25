*          DATA SET ACREPIP02  AT LEVEL 054 AS OF 08/17/00                      
*PHASE ACIP02A                                                                  
         TITLE 'ACIP02 - WRITE MAD WORKER FILE'                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  QOPT1=T =TST SYSTEM                                                          
*  QOPT2=Y =WRKR FILE ON HOLD                                                   
*  QOPT3=Y =TRACE                                                               
*  QOPT4=Y =RE-PROCESS AUTOPAY RECS                                             
*                                                                               
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- INPUT TAPE RECORD                              *         
*                R3 -- OUTPUT WORKER FILE RECORD                      *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ACIP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACIP02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING ACWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REQF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         MVI   WRKRSTAT,0                                                       
         BAS   RE,WRKROPEN         OPEN NEW AGY                                 
*                                                                               
         USING LAYOUTD,R4                                                       
REQF60   LA    RE,IO2              COPY TO WORKER RECORD                        
         LA    RF,1000                                                          
         XCEF                                                                   
         LA    R4,IO2                                                           
         OC    0(30,R4),=CL30' '                                                
         LA    R4,30(R4)                                                        
         MVI   LAYCNTRY,C'0'                                                    
         MVI   LAYACT,C'A'                                                      
         MVC   LAYCLT,=CL6'LBH'                                                 
         MVC   LAYPRD,=CL6'DOA'                                                 
         MVC   LAYJOB,=CL6'STNG'                                                
         MVC   LAYJOBN,=CL36'STAR TREK NEXT GENERATION'                         
         MVC   LAYFLT1(5),=C'     '                                             
         MVC   LAYODATE,=C'12/16/99'                                            
         MVC   LAYCDATE,=C'1/1/00  '                                            
         MVC   LAYPOB,=CL50' '                                                  
*                                                                               
         MVC   LAYUFLD1,=CL30'1'                                                
         MVC   LAYUFLD2,=CL30'2'                                                
         MVC   LAYUFLD3,=CL30'3'                                                
         MVC   LAYUFLD4,=CL30'4'                                                
         MVC   LAYUFLD5,=CL30'5'                                                
         MVC   LAYUFLD6,=CL30'6'                                                
         MVC   LAYUFLD7,=CL50'7'                                                
         MVC   LAYUFLD8,=CL50'8'                                                
         MVC   LAYUFLD9,=CL50'9'                                                
         MVC   LAYUFLDA,=CL50'A'                                                
         MVC   LAYOINF1,=CL50' '                                                
         MVC   LAYOINF2,=CL50' '                                                
         MVC   LAYOINF3,=CL50' '                                                
         MVC   LAYJCMT1,=CL50' '                                                
         MVC   LAYJCMT2,=CL50' '                                                
         MVC   LAYJCMT3,=CL50' '                                                
*                                                                               
         MVC   IO2L(2),=Y(LAYEND)                                               
         BAS   RE,WRKR                                                          
*                                                                               
         BAS   RE,WRKRCLSE                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,=X'0A4B'    DDSB ID NUMBER                               
         MVC   WLDESC,=CL16'$MAD DDSX0053'                                      
*                                                                               
         MVC   WLFILEID(5),=C'MADWK'                                            
*&&DO                                                                           
         MVC   WLSYSPRG(3),=C'PRD'                                              
         MVI   WLSUBPRG,C'T'       DDSB FACPAK T                                
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       SET FACTST                                   
         MVC   WLDAY,TDAY+2                                                     
*&&                                                                             
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,0                                                         
         MVI   WLATTB,WLATOBJ                                                   
*                                                                               
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
*&&DO                                                                           
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,=X'0A4B'    DDSB ID NUM                                  
         MVC   UKSYSPRG(3),=C'PRD'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
*&&                                                                             
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
*                                                                               
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 ACREPORT                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'PTJOBADD'                                            
         MVI   18(R1),C'I'           SET TYPE TO INSERT                         
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00726'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'           INSERT ERRORS AT FILE END                  
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20' '                                               
         MVC   30(8,R1),=CL8'DDSB'                                              
         MVC   38(8,R1),=CL8'DDS'                                               
         MVC   46(3,R1),=CL3'GHO'                                               
         MVC   IO2L(2),=H'53'        49 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         OI    WRKRSTAT,WSTATOPN   FILE OPEN                                    
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         TM    WRKRSTAT,WSTATOPN         FILE ALREADY OPEN                      
         BNO   WRKRCLX                                                          
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         NI    WRKRSTAT,X'FF'-WSTATOPN   FILE NOT OPEN                          
*                                                                               
         CLI   QOPT2,C'Y'                OPTION TO SET TO STATUS HOLD           
         BNE   EXIT                                                             
WRKRC10  GOTO1 DATAMGR,DMCB,=C'HOLD    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
WRKRCLX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
*                                                                               
         LA    R4,ELEM                                                          
         USING UKRECD,R4                                                        
         XC    ELEM,ELEM                                                        
         MVC   UKUSRID,=X'0A4B'    DDSB ID NUM                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,(R4),(R3),AWKBUFF                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   P,IO2                                                            
         GOTO1 ACREPORT                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
         EJECT                                                                  
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
COUNT    DS    F                   NUMBER OF RECORDS PUT OUT                    
OUTLIMIT DS    F                                                                
SEQNUM   DS    F                                                                
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
LASTAGY  DS    XL1                                                              
TDAY     DS    XL3                 YYMMDD PWOS                                  
CTDAY    DS    XL2                 COMPRESSED TODAY                             
DMACTN   DS    CL5                                                              
FIXED    DS    CL1                                                              
WRKRSTAT DS    XL1                                                              
WSTATOPN EQU   X'80'               A WORKER FILE IS OPEN                        
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
*                                                                               
         DS    0D                                                               
ELEM     DS    CL256                                                            
IO       DS    XL256               IO AREA                                      
IO2L     DS    F                                                                
IO2      DS    2000C               IO AREA                                      
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
*        WORKER FILE DSECT                                                      
***********************************************************************         
WRECD    DSECT                                                                  
WRHDR    DS    CL30                HEADER                                       
WRPAYER  DS    CL12                PAYER                                        
WRMEDIA  DS    CL1                 MEDIA                                        
WRCLIENT DS    CL3                 CLIENT                                       
WRPRD    DS    CL3                 PRODUCT                                      
WRPRD2   DS    CL3                 PARTNER                                      
WREST    DS    CL3                 ESTIMATE                                     
WRSTAT   DS    CL10                STATION                                      
WRSREP   DS    CL4                 SPECIAL REP                                  
WRMONTH  DS    CL6                 MONTH (MMM/YY)                               
WRINVCE  DS    CL12                INVOICE TRACKING                             
WRLENQ   EQU   *-WRECD                                                          
         EJECT                                                                  
LAYOUTD  DSECT                     LAYOUT                                       
LAYCNTRY DS    CL1                 COUNTRY                                      
LAYACT   DS    CL1                 ACTION                                       
LAYCLT   DS    CL6                 CLIENT                                       
LAYPRD   DS    CL6                 PRODUCT                                      
LAYJOB   DS    CL6                 JOB                                          
LAYJOBN  DS    CL36                JOB NAME                                     
LAYFLT1  DS    CL1                                                              
LAYFLT2  DS    CL1                                                              
LAYFLT3  DS    CL1                                                              
LAYFLT4  DS    CL1                                                              
LAYFLT5  DS    CL1                                                              
LAYODATE DS    CL8                 OPEN DATE                                    
LAYCDATE DS    CL8                 CLOSE DATE                                   
LAYPOB   DS    CL49                                                             
LAYUFLD1 DS    CL30                                                             
LAYUFLD2 DS    CL30                                                             
LAYUFLD3 DS    CL30                                                             
LAYUFLD4 DS    CL30                                                             
LAYUFLD5 DS    CL30                                                             
LAYUFLD6 DS    CL30                                                             
LAYUFLD7 DS    CL30                                                             
LAYUFLD8 DS    CL30                                                             
LAYUFLD9 DS    CL30                                                             
LAYUFLDA DS    CL30                                                             
LAYOINF1 DS    CL50                                                             
LAYOINF2 DS    CL50                                                             
LAYOINF3 DS    CL50                                                             
LAYJCMT1 DS    CL50                                                             
LAYJCMT2 DS    CL50                                                             
LAYJCMT3 DS    CL50                                                             
LAYEND   EQU   *-LAYOUTD                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE ACRAPPERD                                                      
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREPIP02 08/17/00'                                      
         END                                                                    
