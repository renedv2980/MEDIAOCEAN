*          DATA SET DDSTATSRT  AT LEVEL 016 AS OF 06/05/87                      
*PHASE STATSRT,*,NOAUTO                                                         
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE CARDS                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'DDSTATSRT -- WORKLOAD STATISTICS BY OFFICE'                     
STATSRT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*STATSRT,=V(REGSAVE),R9                                        
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'   READ MON/YR FROM CARD             
         MVC   TITLE(31),=C'WORKLOAD STATISTICS BY OFFICE  '                    
         MVC   TITLE+31(6),CARD                                                 
         MVC   MID1,HEADER1                                                     
         MVC   MID2,HEADER2                                                     
*                                                                               
         OPEN  (DATAIN,INPUT)      INPUT FILE                                   
         OPEN  (DATAOUT,OUTPUT)    OUTPUT FILE                                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         USING ADRRECD,R8                                                       
         LA    R8,REC                                                           
*                                                                               
         L     R2,AINFOX           LENGTH OF INFO TABLE                         
         S     R2,=A(INFO)                                                      
         SRDA  R2,32                                                            
         D     R2,=F'41'           COMPUTE NUMBER OF RECORDS IN TABLE           
         ST    R3,INFOLEN          SAVE RESULT                                  
*                                                                               
         LA    R2,INFO             PRECAUTIONARY TEST TO BE SURE                
         MVC   INFOKEY,=C'        '  THAT TABLE IS IN SORTED ORDER              
TESTLOOP CLC   INFOKEY,2(R2)                                                    
         BL    *+6                                                              
         DC    H'0'                TABLE IS OUT OF SEQUENCE                     
         MVC   INFOKEY,2(R2)                                                    
         LA    R2,41(R2)                                                        
         BCT   R3,TESTLOOP                                                      
         EJECT                                                                  
PUTLOOP  GET   DATAIN,REC                                                       
         PUT   DATAOUT,REC                                                      
         OC    REC,REC             IGNORE NULL RECORDS                          
         BZ    PUTLOOP                                                          
         CLI   REC,C'A'            IGNORE $PQ RECORDS                           
         BL    PUTLOOP                                                          
         CLC   =C'DD',REC          IGNORE LINES AT DDS                          
         BE    PUTLOOP                                                          
         CLC   =C'TE',REC          IGNORE LOCAL STATIONS                        
         BE    PUTLOOP                                                          
         CLC   =C'TW',REC          IGNORE THESE AS WELL                         
         BE    PUTLOOP                                                          
*                                                                               
         MVC   DMCB+8(4),INFOLEN   NUMBER OF RECORDS IN TABLE                   
         MVC   DMCB+20(4),INFOLEN                                               
         GOTO1 =V(BINSRCH),DMCB,(2,RECSPARE),INFO,,41,(2,8)                     
         CLI   0(R1),X'01'         SEE IF KEY IS ABSENT FROM TABLE              
         BE    PUTLOOP             MISSING KEY -- IGNORE RECORD                 
*                                                                               
         L     R3,0(R1)                                                         
         MVC   SOFFICE,0(R3)       STORE OFFICE                                 
         MVC   SGROUP,10(R3)       STORE GROUP                                  
         MVC   SSUBGRP,23(R3)      STORE SUBGROUP                               
         MVC   SCOLOR,36(R3)       STORE COLOR                                  
         MVC   SLINEID,2(R3)       STORE LINE-ID                                
         MVC   SADDRESS,6(R3)      STORE ADDRESS                                
         MVC   SINPCHAR,ADRMSGI    STORE NUMBER OF INPUT CHARACTERS             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         B     PUTLOOP                                                          
*                                                                               
EOF      CLOSE (DATAIN)            CLOSE FILES                                  
         CLOSE (DATAOUT)                                                        
         DROP  R8                                                               
*                                                                               
* SORT RECORDS ON OFFICE, LINE-ID, DESCRIPTION, AND ADDRESS                     
*                                                                               
         XC    TTCOLOR,TTCOLOR     CLEAR TOTAL ACCUMULATORS                     
         XC    TICOLOR,TICOLOR                                                  
         XC    TTSUBGRP,TTSUBGRP                                                
         XC    TISUBGRP,TISUBGRP                                                
         XC    TTGROUP,TTGROUP                                                  
         XC    TIGROUP,TIGROUP                                                  
         XC    TTOFFICE,TTOFFICE                                                
         XC    TIOFFICE,TIOFFICE                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET' FIRST RECORD                             
         L     R3,4(R1)                                                         
         MVC   SREC,0(R3)                                                       
*                                                                               
NEXTLINE SR    R2,R2               CLEAR LINE-ID/ADDRESS ACCUMULATORS           
         SR    R4,R4                                                            
*                                                                               
GETLOOP  LA    R2,1(R2)            TRANSACTION COUNTER FOR AN ADDRESS           
         AH    R4,SINPCHAR         INPUT CHARACTERS FOR AN ADDRESS              
*                                                                               
         L     R7,TTCOLOR          ADJUST COLOR TOTALS                          
         LA    R7,1(R7)                                                         
         ST    R7,TTCOLOR                                                       
         L     R7,TICOLOR                                                       
         AH    R7,SINPCHAR                                                      
         ST    R7,TICOLOR                                                       
*                                                                               
         L     R7,TTSUBGRP         ADJUST SUBGROUP TOTALS                       
         LA    R7,1(R7)                                                         
         ST    R7,TTSUBGRP                                                      
         L     R7,TISUBGRP                                                      
         AH    R7,SINPCHAR                                                      
         ST    R7,TISUBGRP                                                      
*                                                                               
         L     R7,TTGROUP          ADJUST GROUP TOTALS                          
         LA    R7,1(R7)                                                         
         ST    R7,TTGROUP                                                       
         L     R7,TIGROUP                                                       
         AH    R7,SINPCHAR                                                      
         ST    R7,TIGROUP                                                       
*                                                                               
         L     R7,TTOFFICE         ADJUST OFFICE TOTALS                         
         LA    R7,1(R7)                                                         
         ST    R7,TTOFFICE                                                      
         L     R7,TIOFFICE                                                      
         AH    R7,SINPCHAR                                                      
         ST    R7,TIOFFICE                                                      
*                                                                               
         MVC   OREC,SREC                                                        
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R3,4(R1)            ADDRESS OF SORTED RECORD                     
         LTR   R3,R3                                                            
         BNZ   CONT2                                                            
         MVI   EOFSORT,X'FF'       END OF SORTED RECORD FILE                    
         XC    SREC,SREC                                                        
         B     CONT3                                                            
*                                                                               
CONT2    MVC   SREC,0(R3)                                                       
         CLC   OREC,SREC           SEE IF KEYS HAVE CHANGED                     
         BE    GETLOOP             IF SO, GET ANOTHER RECORD                    
*                                                                               
* PRINT STATS FOR ONE ADDRESS                                                   
*                                                                               
CONT3    MVC   POFFICE(2),OOFFICE        OFFICE                                 
         MVC   PLINEID(4),OLINEID        LINE-ID                                
         MVC   PADDRESS(4),OADDRESS      ADDRESS                                
         MVC   PGRP(13),OGROUP           GROUP                                  
         MVC   PSUBGRP(13),OSUBGRP       SUBGROUP                               
         MVC   PCOLOR(5),OCOLOR          COLOR                                  
*                                                                               
         EDIT  (R2),(16,PTRANSAC),COMMAS=YES,ZERO=BLANK                         
         EDIT  (R4),(16,PINCHAR),COMMAS=YES,ZERO=BLANK                          
*                                                                               
         SR    R5,R5                     AVERAGE NO. INPUT CHARACTERS           
         SRDA  R4,31                                                            
         DR    R4,R2                                                            
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         EDIT  (R5),(16,PAVGCHAR),COMMAS=YES,ZERO=BLANK                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   SOFFICE,OOFFICE           SET FLAGS TO PRINT TOTALS OF           
         BE    L1                        KEYS WHICH HAVE CHANGED                
         MVI   FOFFICE,X'FF'                                                    
         MVI   FGROUP,X'FF'                                                     
L1       CLC   SGROUP,OGROUP                                                    
         BE    L2                                                               
         MVI   FGROUP,X'FF'                                                     
L2       CLC   SSUBGRP,OSUBGRP                                                  
         BE    L3                                                               
         CLC   OSUBGRP,SPACES                                                   
         BE    L3                                                               
         MVI   FSUBGRP,X'FF'                                                    
L3       CLC   SCOLOR,OCOLOR                                                    
         BE    L4                                                               
         CLC   OCOLOR,SPACES                                                    
         BE    L4                                                               
         MVI   FCOLOR,X'FF'                                                     
*                                                                               
L4       CLI   FCOLOR,X'FF'        SEE IF COLOR HAS CHANGED                     
         BNE   XSUBGRP                                                          
         MVI   FCOLOR,X'00'        RESET PRINT FLAG                             
         MVC   P,TOTALDC           PRINT DASHED LINE                            
         GOTO1 =V(PRINTER)                                                      
         MVC   TOTALSG,OGROUP      PRINT TOTAL LINE                             
         MVC   TOTALSS,OSUBGRP                                                  
         MVC   TOTALSC,OCOLOR                                                   
         MVC   P,TOTALS                                                         
         L     R2,TTCOLOR          GET TOTALS                                   
         L     R4,TICOLOR                                                       
         XC    TTCOLOR,TTCOLOR                                                  
         XC    TICOLOR,TICOLOR                                                  
         EDIT  (R2),(16,PTRANSAC),COMMAS=YES,ZERO=BLANK                         
         EDIT  (R4),(16,PINCHAR),COMMAS=YES,ZERO=BLANK                          
         SR    R5,R5               COMPUTE AVG. NO. INPUT CHARACTERS            
         SRDA  R4,31                                                            
         DR    R4,R2                                                            
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         EDIT  (R5),(16,PAVGCHAR),COMMAS=YES,ZERO=BLANK                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES            SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
XSUBGRP  CLI   FSUBGRP,X'FF'       SEE IF SUBGROUP HAS CHANGED                  
         BNE   XGROUP                                                           
         MVI   FSUBGRP,X'00'                                                    
         MVC   P,TOTALDS                                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   TOTALSG,OGROUP                                                   
         MVC   TOTALSS,OSUBGRP                                                  
         MVC   TOTALSC,SPACES                                                   
         MVC   P,TOTALS                                                         
         L     R2,TTSUBGRP                                                      
         L     R4,TISUBGRP                                                      
         XC    TTCOLOR,TTCOLOR                                                  
         XC    TICOLOR,TICOLOR                                                  
         XC    TTSUBGRP,TTSUBGRP                                                
         XC    TISUBGRP,TISUBGRP                                                
         EDIT  (R2),(16,PTRANSAC),COMMAS=YES,ZERO=BLANK                         
         EDIT  (R4),(16,PINCHAR),COMMAS=YES,ZERO=BLANK                          
         SR    R5,R5                                                            
         SRDA  R4,31                                                            
         DR    R4,R2                                                            
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         EDIT  (R5),(16,PAVGCHAR),COMMAS=YES,ZERO=BLANK                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
XGROUP   CLI   FGROUP,X'FF'        SEE IF GROUP HAS CHANGED                     
         BNE   XOFFICE                                                          
         MVI   FGROUP,X'00'                                                     
         MVC   P,TOTALDG                                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   TOTALSG,OGROUP                                                   
         MVC   TOTALSS,SPACES                                                   
         MVC   TOTALSC,SPACES                                                   
         MVC   P,TOTALS                                                         
         L     R2,TTGROUP                                                       
         L     R4,TIGROUP                                                       
         XC    TTCOLOR,TTCOLOR                                                  
         XC    TICOLOR,TICOLOR                                                  
         XC    TTSUBGRP,TTSUBGRP                                                
         XC    TISUBGRP,TISUBGRP                                                
         XC    TTGROUP,TTGROUP                                                  
         XC    TIGROUP,TIGROUP                                                  
         EDIT  (R2),(16,PTRANSAC),COMMAS=YES,ZERO=BLANK                         
         EDIT  (R4),(16,PINCHAR),COMMAS=YES,ZERO=BLANK                          
         SR    R5,R5                                                            
         SRDA  R4,31                                                            
         DR    R4,R2                                                            
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         EDIT  (R5),(16,PAVGCHAR),COMMAS=YES,ZERO=BLANK                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
XOFFICE  CLI   FOFFICE,X'FF'       SEE IF OFFICE HAS CHANGED                    
         BNE   XNOEJECT                                                         
         MVI   FOFFICE,X'00'                                                    
         MVC   P,TOTALDO                                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   TOTALSG,SPACES                                                   
         MVC   TOTALSS,SPACES                                                   
         MVC   TOTALSC,SPACES                                                   
         MVC   P,TOTALS                                                         
         L     R2,TTOFFICE                                                      
         L     R4,TIOFFICE                                                      
         XC    TTCOLOR,TTCOLOR                                                  
         XC    TICOLOR,TICOLOR                                                  
         XC    TTSUBGRP,TTSUBGRP                                                
         XC    TISUBGRP,TISUBGRP                                                
         XC    TTGROUP,TTGROUP                                                  
         XC    TIGROUP,TIGROUP                                                  
         XC    TTOFFICE,TTOFFICE                                                
         XC    TIOFFICE,TIOFFICE                                                
         EDIT  (R2),(16,PTRANSAC),COMMAS=YES,ZERO=BLANK                         
         EDIT  (R4),(16,PINCHAR),COMMAS=YES,ZERO=BLANK                          
         SR    R5,R5                                                            
         SRDA  R4,31                                                            
         DR    R4,R2                                                            
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         EDIT  (R5),(16,PAVGCHAR),COMMAS=YES,ZERO=BLANK                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ZAP   LINE,=P'75'         START A NEW PAGE                             
XNOEJECT CLI   EOFSORT,X'FF'                                                    
         BNE   NEXTLINE                                                         
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE                                                                  
         EJECT                                                                  
DATAIN   DCB   DDNAME=DATAIN,DSORG=PS,MACRF=(GM),EODAD=EOF,            +        
               RECFM=FB,BLKSIZE=1900,LRECL=38                                   
DATAOUT  DCB   DDNAME=DATAOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,BLKSIZE=1900,LRECL=38                                   
*                                                                               
DMCB     DS    6F                                                               
DUMPLIST DS    0F                                                               
         DC    A(STATSRT,25000)                                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
WORK     DS    CL17                                                             
DUB      DS    D                                                                
INFOLEN  DS    F                                                                
AINFOX   DC    A(INFOX)                                                         
INFOKEY  DS    CL8                                                              
*                                                                               
EOFSORT  DC    X'00'                                                            
FOFFICE  DC    X'00'                                                            
FGROUP   DC    X'00'                                                            
FSUBGRP  DC    X'00'                                                            
FCOLOR   DC    X'00'                                                            
*                                                                               
RECSPARE DS    0D                                                               
         DS    CL2                                                              
REC      DS    CL38                                                             
*                                                                               
SREC     DS    0CL43                                                            
SOFFICE  DS    CL2                                                              
SGROUP   DS    CL13                                                             
SSUBGRP  DS    CL13                                                             
SCOLOR   DS    CL5                                                              
SLINEID  DS    CL4                                                              
SADDRESS DS    CL4                                                              
SINPCHAR DS    H                                                                
*                                                                               
OREC     DS    0CL41                                                            
OOFFICE  DS    CL2                                                              
OGROUP   DS    CL13                                                             
OSUBGRP  DS    CL13                                                             
OCOLOR   DS    CL5                                                              
OLINEID  DS    CL4                                                              
OADDRESS DS    CL4                                                              
*                                                                               
CARD     DS    CL80                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,41,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=43'                                    
         EJECT                                                                  
HEADER1  DC    CL132'  OFFICE  LINE-ID  ADDRESS  GROUP          SUBGROU+        
               P       COLOR         TRANSACTIONS       INPUT CHARS    +        
                 AVERAGE INPUT'                                                 
HEADER2  DC    CL132'  ------  -------  -------  -----          -------+        
               -       -----         ------------       ----- -----    +        
                 ------- -----'                                                 
TOTALDO  DC    CL132'                   -------                        +        
                                     ------------       -----------    +        
                 -------------'                                                 
TOTALDG  DC    CL132'                   -------  -----                 +        
                                     ------------       -----------    +        
                 -------------'                                                 
TOTALDS  DC    CL132'                   -------  -----          -------+        
               -                     ------------       -----------    +        
                 -------------'                                                 
TOTALDC  DC    CL132'                   -------  -----          -------+        
               -       -----         ------------       -----------    +        
                 -------------'                                                 
TOTALS   DS    0CL132                                                           
         DC    CL28'                   TOTAL   '                                
TOTALSG  DS    CL13                                                             
         DC    CL2'  '                                                          
TOTALSS  DS    CL13                                                             
         DC    CL2'  '                                                          
TOTALSC  DS    CL5                                                              
         DC    CL69' '                                                          
*                                                                               
TTCOLOR  DS    F                                                                
TICOLOR  DS    F                                                                
TTSUBGRP DS    F                                                                
TISUBGRP DS    F                                                                
TTGROUP  DS    F                                                                
TIGROUP  DS    F                                                                
TTOFFICE DS    F                                                                
TIOFFICE DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* NOTE -- THIS TABLE MUST REMAIN SORTED ON BYTES 3-10 OF EACH ENTRY             
*                                                                               
INFO     DS    0F                                                               
         DC    C'SFBGWAC340SELECT                         '                     
         DC    C'NYBGWA4040SELECT                         '                     
         DC    C'ATBLAT40C1TV                             '                     
         DC    C'ATBLAT40C2TV                             '                     
         DC    C'ATBLAT40C3TV                             '                     
         DC    C'ATBLAT40C9TV                             '                     
         DC    C'ATBLAT404ATV                             '                     
         DC    C'ATBLAT4040TV                             '                     
         DC    C'BOBLBO40C1TV                             '                     
         DC    C'BOBLBO40C2TV                             '                     
         DC    C'BOBLBO40C4TV                             '                     
         DC    C'BOBLBO4040TV                             '                     
         DC    C'CLBLCLC1C1TV                             '                     
         DC    C'CLBLCLC1C2TV                             '                     
         DC    C'CLBLCLC140TV                             '                     
         DC    C'DEBLDE40C1TV                             '                     
         DC    C'DEBLDE40C3TV                             '                     
         DC    C'DEBLDE40C4TV                             '                     
         DC    C'DEBLDE4040TV                             '                     
         DC    C'HOBLDHC1C2TV                             '                     
         DC    C'HOBLDHC140TV                             '                     
         DC    C'DABLDH40C1TV                             '                     
         DC    C'DABLDH40C2TV                             '                     
         DC    C'DABLDH40C3TV                             '                     
         DC    C'DABLDH40C5TV                             '                     
         DC    C'DABLDH404ATV                             '                     
         DC    C'DABLDH404BTV                             '                     
         DC    C'DABLDH4040TV                             '                     
         DC    C'MIBLFLC1C1TV                             '                     
         DC    C'MIBLFLC1C2TV                             '                     
         DC    C'MIBLFLC140TV                             '                     
         DC    C'JABLFLC2C1TV                             '                     
         DC    C'JABLFLC2C2TV                             '                     
         DC    C'JABLFLC240TV                             '                     
         DC    C'CABLFL40C1TV                             '                     
         DC    C'CABLFL40C2TV                             '                     
         DC    C'CABLFL4040TV                             '                     
         DC    C'PHBLPH40C2TV                             '                     
         DC    C'PHBLPH40C3TV                             '                     
         DC    C'PHBLPH40C4TV                             '                     
         DC    C'PHBLPH4040TV                             '                     
         DC    C'NYBLRAC1C1TV           CBS          GREEN'                     
         DC    C'NYBLRAC1C2TV           IND          BLUE '                     
         DC    C'NYBLRAC1C3TV           ABC          RED  '                     
         DC    C'NYBLRAC1C4TV           CBS          RED  '                     
         DC    C'NYBLRAC1C5RESEARCH                       '                     
         DC    C'NYBLRAC140TV           NBC          BLUE '                     
         DC    C'NYBLRAC2C1TV           CBS          GREEN'                     
         DC    C'NYBLRAC2C2TV           IND          RED  '                     
         DC    C'NYBLRAC2C3TV           ABC          RED  '                     
         DC    C'NYBLRAC2C4TV           EXECUTIVE         '                     
         DC    C'NYBLRAC2C5TV           BAR               '                     
         DC    C'NYBLRAC240TV           NBC          RED  '                     
         DC    C'NYBLRA40C1RESEARCH                       '                     
         DC    C'NYBLRA40C2TV           ABC          GREEN'                     
         DC    C'NYBLRA40C3RESEARCH                       '                     
         DC    C'NYBLRA40C4TV           NBC          GREEN'                     
         DC    C'NYBLRA40C5ACCOUNTING                     '                     
         DC    C'NYBLRA40C6TV           NBC          GREEN'                     
         DC    C'NYBLRA4040SSD                            '                     
         DC    C'NYBLRBC3C1RESEARCH                       '                     
         DC    C'NYBLRBC3C2RESEARCH                       '                     
         DC    C'NYBLRBC3C3ACCOUNTING                     '                     
         DC    C'NYBLRBC3C5TV           NBC          GOLD '                     
         DC    C'NYBLRBC3C6TV           IND          BLUE '                     
         DC    C'NYBLRBC3C7TV           NBC          RED  '                     
         DC    C'NYBLRBC340TV           CBS          RED  '                     
         DC    C'NYBLRBC4C1TV           TRAINING ROOM     '                     
         DC    C'NYBLRBC4C2TV           COMMON            '                     
         DC    C'NYBLRBC5C1TV           ABC          GREEN'                     
         DC    C'NYBLRCC5C2TV           CBS          RED  '                     
         DC    C'NYBLRCC5C4TV           NBC          RED  '                     
         DC    C'NYBLRCC5C5RADIO                          '                     
         DC    C'NYBLRCC5C6TV           IND          BLUE '                     
         DC    C'NYBLRCC5C7TV           NBC          GOLD '                     
         DC    C'NYBLRCC540SSD                            '                     
         DC    C'NYBLRCC6C1TV           TRAINING ROOM     '                     
         DC    C'NYBLRCC6C2TV           COMMON            '                     
         DC    C'NYBLRDC7C1RESEARCH                       '                     
         DC    C'NYBLRDC7C3ACCOUNTING                     '                     
         DC    C'NYBLRDC7C4TV           NBC          BLUE '                     
         DC    C'NYBLRDC7C5TV           IND          RED  '                     
         DC    C'NYBLRDC7C6TV           ABC          RED  '                     
         DC    C'NYBLRDC7C7RESEARCH                       '                     
         DC    C'NYBLRDC740TV           EXECUTIVE         '                     
         DC    C'NYBLRDC8C1TV           TRAINING ROOM     '                     
         DC    C'NYBLRDC8C2TV           COMMON            '                     
         DC    C'NYBLREC9C2RADIO                          '                     
         DC    C'NYBLREC9C3TV           CBS          RED  '                     
         DC    C'NYBLREC9C4TV           NBC          GOLD '                     
         DC    C'NYBLREC9C5TV           ABC          BLUE '                     
         DC    C'NYBLREC9C6TV           ABC          BLUE '                     
         DC    C'NYBLREC9C7RESEARCH                       '                     
         DC    C'NYBLRE4AC1TV           TRAINING ROOM     '                     
         DC    C'NYBLRE4AC2RESEARCH                       '                     
         DC    C'NYBLRE4AC3TV           COMMON            '                     
         DC    C'NYBLRF4BC1RESEARCH                       '                     
         DC    C'NYBLRF4BC2TV           ABC          BLUE '                     
         DC    C'NYBLRF4BC3RADIO                          '                     
         DC    C'NYBLRF4BC4TV           NBC          BLUE '                     
         DC    C'NYBLRF4BC5ACCOUNTING                     '                     
         DC    C'NYBLRF4BC6TV           NBC          GOLD '                     
         DC    C'NYBLRF4BC7TV           NBC          RED  '                     
         DC    C'NYBLRF4B40SSD                            '                     
         DC    C'NYBLRF4CC1TV           TRAINING ROOM     '                     
         DC    C'NYBLRF4CC2RESEARCH                       '                     
         DC    C'NYBLRG4DC1TV           ABC          GREEN'                     
         DC    C'NYBLRG4DC2RESEARCH                       '                     
         DC    C'NYBLRG4DC3TV           IND          RED  '                     
         DC    C'NYBLRG4DC4TV           NBC          GREEN'                     
         DC    C'NYBLRG4DC5TV           CBS          GREEN'                     
         DC    C'NYBLRG4DC6TV           ABC          RED  '                     
         DC    C'NYBLRG4DC7RESEARCH                       '                     
         DC    C'NYBLRG4D40SSD                            '                     
         DC    C'NYBLRG4EC1TV           TRAINING ROOM     '                     
         DC    C'NYBLRG4EC2TV           AVAIL ROOM        '                     
         DC    C'NYBLRHD1C1RESEARCH                       '                     
         DC    C'NYBLRHD1C2TV           IND          BLUE '                     
         DC    C'NYBLRHD1C3TV           ABC          BLUE '                     
         DC    C'NYBLRHD1C5TV           IND          RED  '                     
         DC    C'NYBLRHD1C6TV           EXECUTIVE         '                     
         DC    C'NYBLRHD140TV           NBC          BLUE '                     
         DC    C'NYBLRHD2C1TV           AVAIL ROOM        '                     
         DC    C'NYBLRHD2C2RESEARCH                       '                     
         DC    C'NYBLRHD2C3TV           COMMON            '                     
         DC    C'NYBLRH4FC1SSD                            '                     
         DC    C'NYBLRH4FC2RESEARCH                       '                     
         DC    C'NYBLRH4FC3RADIO                          '                     
         DC    C'NYBLRH4FC4TV           CBS          GREEN'                     
         DC    C'NYBLRH4FC5RADIO                          '                     
         DC    C'NYBLRH4FC6RADIO                          '                     
         DC    C'NYBLRH4FC7TV           IND          RED  '                     
         DC    C'NYBLRH4FC8RESEARCH                       '                     
         DC    C'NYBLRH4FC9TV           ABC          RED  '                     
         DC    C'NYBLRH4F4ATV           CBS          GREEN'                     
         DC    C'NYBLRH4F4BTV           ABC          GREEN'                     
         DC    C'NYBLRH4F4CACCOUNTING                     '                     
         DC    C'NYBLRH4F4DTV           ABC          GREEN'                     
         DC    C'NYBLRH4F4ERESEARCH                       '                     
         DC    C'NYBLRH4F40SSD                            '                     
         DC    C'NYBLRIC2C1RESEARCH                       '                     
         DC    C'NYBLRIC2C2TV           IND          BLUE '                     
         DC    C'NYBLRIC2C3TV           ABC          BLUE '                     
         DC    C'NYBLRIC2C5TV           IND          RED  '                     
         DC    C'NYBLRIC2C6TV           EXECUTIVE         '                     
         DC    C'NYBLRIC240TV           NBC          BLUE '                     
         DC    C'NYBLRI40C1TV           AVAIL ROOM        '                     
         DC    C'NYBLRI40C2RESEARCH                       '                     
         DC    C'NYBLRI40C3TV           COMMON            '                     
         DC    C'NYBLRJD1C1TV           CBS          GREEN'                     
         DC    C'NYBLRJD1C2TV           IND          RED  '                     
         DC    C'NYBLRJD1C3TV           ABC          RED  '                     
         DC    C'NYBLRJD1C5TV           EXECUTIVE         '                     
         DC    C'NYBLRJD1C6TV           NBC          GREEN'                     
         DC    C'NYBLRJD140TV           NBC          RED  '                     
         DC    C'NYBLRKC1C1TV           TRAINING ROOM     '                     
         DC    C'NYBLRKC1C2TV           COMMON            '                     
         DC    C'NYBLRL40C1TV           ABC          GREEN'                     
         DC    C'NYBLRL40C2TV           PROGRAMMING       '                     
         DC    C'NYBLRL4040TV           NBC          BLUE '                     
         DC    C'NYBLRM40C1SELECT                         '                     
         DC    C'NYBLRM40C2SELECT                         '                     
         DC    C'NYBLRM4040SELECT                         '                     
         DC    C'BORAD1C140RADIO                          '                     
         DC    C'BORAD1C240TORBET                         '                     
         DC    C'NYRAD1C3C1RADIO                          '                     
         DC    C'NYRAD1C3C2RADIO                          '                     
         DC    C'NYRAD1C3C3RADIO                          '                     
         DC    C'NYRAD1C3C4RADIO                          '                     
         DC    C'NYRAD1C3C5RADIO                          '                     
         DC    C'NYRAD1C3C6RADIO                          '                     
         DC    C'NYRAD1C340RADIO                          '                     
         DC    C'BORAD1C440SELECT                         '                     
         DC    C'NYRAD1C5C1TORBET                         '                     
         DC    C'NYRAD1C5C2TORBET                         '                     
         DC    C'NYRAD1C540TORBET                         '                     
         DC    C'NYRAD140C1SELECT                         '                     
         DC    C'NYRAD140C2SELECT                         '                     
         DC    C'NYRAD140C3SELECT                         '                     
         DC    C'NYRAD14040SELECT                         '                     
         DC    C'PHRAD2C140RADIO                          '                     
         DC    C'PHRAD2C240TORBET                         '                     
         DC    C'PHRAD24040SELECT                         '                     
         DC    C'CHXBLA40C1TV           NBC          RED  '                     
         DC    C'CHXBLA40C2TV           ABC          RED  '                     
         DC    C'CHXBLA40C3TV           SUPPORT           '                     
         DC    C'CHXBLA40C4TV           CBS               '                     
         DC    C'CHXBLB40C1TV           ABC          RED  '                     
         DC    C'CHXBLB40C2TV           SUPPORT           '                     
         DC    C'CHXBLB40C3TV           CBS               '                     
         DC    C'CHXBLB40C4TV           IND               '                     
         DC    C'CHXBLB40C5TV           SUPPORT           '                     
         DC    C'CHXBLB4040TV           NBC          RED  '                     
         DC    C'CHXBLCC1C1TV           ABC          RED  '                     
         DC    C'CHXBLCC1C2CONTROL ROOM                   '                     
         DC    C'CHXBLCC1C3TV           C. HITCHINS       '                     
         DC    C'CHXBLCC1C6TV           ABC          BLUE '                     
         DC    C'CHXBLCC140TV           NBC          RED  '                     
         DC    C'CHXBLCC2C1TV           NBC          BLUE '                     
         DC    C'CHXBLCC2C2TV           ABC          BLUE '                     
         DC    C'CHXBLCC2C3TV           ABC          BLUE '                     
         DC    C'CHXBLCC2C4TV           IND               '                     
         DC    C'CHXBLCC2C5TV           NBC          BLUE '                     
         DC    C'CHXBLCC2C6TV           NBC          BLUE '                     
         DC    C'CHXBLCC240TV           IND               '                     
         DC    C'MNXBLM40C2TV                             '                     
         DC    C'MNXBLM40C3TV                             '                     
         DC    C'MNXBLM40C4TV                             '                     
         DC    C'MNXBLM404ATV                             '                     
         DC    C'MNXBLM4040TV                             '                     
         DC    C'SLXBSL40C1TV                             '                     
         DC    C'SLXBSL40C3TV                             '                     
         DC    C'SLXBSL40C4TV                             '                     
         DC    C'SLXBSL40C5TV                             '                     
         DC    C'ATXRA1C140SELECT                         '                     
         DC    C'ATXRA1C240TORBET                         '                     
         DC    C'SLXRA1C340RADIO                          '                     
         DC    C'SLXRA1C440SELECT                         '                     
         DC    C'SLXRA1C540TORBET                         '                     
         DC    C'ATXRA14040RADIO                          '                     
         DC    C'CHXRA2C140SELECT                         '                     
         DC    C'DEXRA2C240SELECT                         '                     
         DC    C'CHXRA2C340TORBET                         '                     
         DC    C'DEXRA2C440RADIO                          '                     
         DC    C'DEXRA2C540TORBET                         '                     
         DC    C'CHXRA240C1RADIO                          '                     
         DC    C'CHXRA240C2RADIO                          '                     
         DC    C'CHXRA240C3RADIO                          '                     
         DC    C'CHXRA240C4RADIO                          '                     
         DC    C'CHXRA240C5RADIO                          '                     
         DC    C'CHXRA240C6RADIO                          '                     
         DC    C'CHXRA24040RADIO                          '                     
         DC    C'DAXRA3C140RADIO                          '                     
         DC    C'DAXRA3C240TORBET                         '                     
         DC    C'HOXRA3C340RADIO                          '                     
         DC    C'HOXRA3C440SELECT                         '                     
         DC    C'HOXRA3C540TORBET                         '                     
         DC    C'DAXRA34040SELECT                         '                     
         DC    C'MNXWOTC340RADIO                          '                     
         DC    C'MNXWOTC440SELECT                         '                     
         DC    C'MNXWOTC540TORBET                         '                     
         DC    C'SEZBLM4040TV                             '                     
         DC    C'SFZBLS40C1TV                             '                     
         DC    C'SFZBLS40C2TV                             '                     
         DC    C'SFZBLS40C3TV                             '                     
         DC    C'SFZBLS40C4TV                             '                     
         DC    C'SFZBLS40C5TV                             '                     
         DC    C'SFZBLS404ASELECT                         '                     
         DC    C'SFZBLS4040TV                             '                     
         DC    C'LAZBL140C1TV           SUPPORT           '                     
         DC    C'LAZBL140C2TV           ABC               '                     
         DC    C'LAZBL140C3TV           ABC               '                     
         DC    C'LAZBL140C4TV           NBC               '                     
         DC    C'LAZBL140C5TV           IND               '                     
         DC    C'LAZBL14040TV           IND               '                     
         DC    C'LAZBL240C1TV           SUPPORT           '                     
         DC    C'LAZBL240C2TV           NBC               '                     
         DC    C'LAZBL240C3TV           IND               '                     
         DC    C'LAZBL240C4TV           ABC               '                     
         DC    C'LAZBL240C5TV           ABC               '                     
         DC    C'LAZBL24040TV           SUPPORT           '                     
         DC    C'LAZBL340C1RADIO                          '                     
         DC    C'LAZBL340C2TV           NBC               '                     
         DC    C'LAZBL340C3TV           NBC               '                     
         DC    C'LAZBL340C4TV           SUPPORT           '                     
         DC    C'LAZBL340C6TV           ABC               '                     
         DC    C'LAZBL34040RADIO                          '                     
         DC    C'SFZBL440C1TV                             '                     
         DC    C'SFZBL440C2TV                             '                     
         DC    C'SFZBL440C3RADIO                          '                     
         DC    C'SFZBL440C4TV                             '                     
         DC    C'SFZBL440C5RADIO                          '                     
         DC    C'SFZBL44040TV                             '                     
         DC    C'LAZKHJC3C1SELECT                         '                     
         DC    C'LAZKHJC3C2SELECT                         '                     
         DC    C'LAZKHJC340SELECT                         '                     
         DC    C'DVZNHPC240TV                             '                     
         DC    C'SEZRA1C140RADIO                          '                     
         DC    C'LAZRA1C240TORBET                         '                     
         DC    C'SFZRSFC140TORBET                         '                     
         DC    C'SFZRSF40C2SELECT                         '                     
         DC    C'SFZRSF4040RADIO                          '                     
INFOX    DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE FAADRREC                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         ORG   P                                                                
         DS    2C                                                               
POFFICE  DS    CL6                                                              
         DS    2C                                                               
PLINEID  DS    CL7                                                              
         DS    2C                                                               
PADDRESS DS    CL7                                                              
         DS    2C                                                               
PGRP     DS    CL13                                                             
         DS    2C                                                               
PSUBGRP  DS    CL13                                                             
         DS    2C                                                               
PCOLOR   DS    CL5                                                              
         DS    5C                                                               
PTRANSAC DS    CL16                                                             
         DS    2C                                                               
PINCHAR  DS    CL16                                                             
         DS    3C                                                               
PAVGCHAR DS    CL16                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDSTATSRT 06/05/87'                                      
         END                                                                    
